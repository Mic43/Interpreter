namespace Interpreter.AST

open System.Collections.Generic
open FParsec
open Microsoft.FSharp.Core


module SemanticAnalyser =

    type StatementAnalyser = ExecutionEnvironment -> Statement -> AnalyserResult option
    type ExpressionAnalyser = ExecutionEnvironment -> Expression -> AnalyserResult option

    let private singleStatementAnalyser: StatementAnalyser =
        fun environment statement ->
            match statement with
            | ScopedStatement (VarDeclarationStatement v) ->
                let error =
                    Environment.tryDefineVar environment v.Name Value.Void

                match error with
                | Ok _ -> None
                | Error _ ->
                    v.Name.Get()
                    |> AnalyserResult.VariableAlreadyDefined
                    |> Some
            | ScopedStatement (ReturnStatement _) ->
                if environment.IsCurrentGlobal then
                    Some(MisplacedReturnStatement)
                else
                    None
            | FunDeclaration f ->
                let error =
                    Environment.tryDefineCallable environment (f |> Function)

                match error with
                | Ok _ -> None
                | Error _ ->
                    f.Name.Get()
                    |> AnalyserResult.FunctionAlreadyDefined
                    |> Some
            | _ -> None

    let rec private singleExpressionAnalyser environment (exp: Expression) =
        match exp with
        | Mutable (Var v) ->
            match v |> Environment.tryGetVarValue environment with
            | Ok _ -> None
            | Error _ ->
                v.Get()
                |> AnalyserResult.VariableNotDefined
                |> Some
        | Assignment (Mutable m, _) ->
            m
            |> Mutable
            |> singleExpressionAnalyser environment

        | Assignment (exp, _) ->
            exp
            |> Printer.expressionToStr
            |> ExpressionMustBeLValue
            |> Some
        | FunCall funCall ->
            match funCall.Name
                  |> Environment.tryGetCallable environment
                with
            | Ok _ -> None
            | Error _ ->
                funCall.Name.Get()
                |> AnalyserResult.FunctionNotDefined
                |> Some
        | _ -> None

    let rec private expressionAnalyseInternal
        (nodeAnalyser: ExpressionAnalyser)
        environment
        expression
        cnt
        : AnalyserResult list =

        let expressionAnalyse =
            expressionAnalyseInternal nodeAnalyser environment

        let analyseCurrentNode () =
            (expression
             |> (nodeAnalyser environment)
             |> Option.toList)

        let appendCurrentNodeResults res =
            analyseCurrentNode () |> List.append res

        let genContinuationForSingleChild exp =
            expressionAnalyse exp
            |> Continuation.fromFun
            |> Continuation.map appendCurrentNodeResults

        let genContinuationForMultipleChildren expList =
            expList
            |> List.map (expressionAnalyse >> Continuation.fromFun)
            |> TraversableContinuationList.sequence
            |> Continuation.map (List.collect id >> appendCurrentNodeResults)

        match expression with
        | Binary ({ LeftOperand = left
                    RightOperand = right }) ->
            continuation {
                let! resLeft = expressionAnalyse left |> Continuation.fromFun
                let! resRight = expressionAnalyse right |> Continuation.fromFun

                return resLeft @ resRight |> appendCurrentNodeResults
            }
            |> Continuation.run cnt
        | SimpleUnary (op, operand) ->
            operand
            |> genContinuationForSingleChild
            |> Continuation.run cnt
        | Increment (op, exp) ->
            exp
            |> genContinuationForSingleChild
            |> Continuation.run cnt
        | Assignment (lValExp, exp) ->
            exp
            |> genContinuationForSingleChild
            |> Continuation.run cnt
        | ListCreation expList ->
            expList
            |> genContinuationForMultipleChildren
            |> Continuation.run cnt
        | UserTypeCreation { FieldsInitialization = fieldsInit } ->
            fieldsInit
            |> Map.values
            |> List.ofSeq
            |> genContinuationForMultipleChildren
            |> Continuation.run cnt
        | FunCall { ActualParameters = actualParamsExps } ->
            actualParamsExps
            |> List.ofSeq
            |> genContinuationForMultipleChildren
            |> Continuation.run cnt

        | Mutable (IndexedVar (leftExp, rightExp)) ->
            continuation {
                let! resLeft =
                    expressionAnalyse (leftExp |> Mutable)
                    |> Continuation.fromFun

                let! resRight = expressionAnalyse rightExp |> Continuation.fromFun

                return resLeft @ resRight |> appendCurrentNodeResults
            }
            |> Continuation.run cnt
        | Mutable (MemberAccess (leftExp, _)) ->
            leftExp
            |> Mutable
            |> genContinuationForSingleChild
            |> Continuation.run cnt
        | Mutable _ -> analyseCurrentNode () |> cnt
        | Constant _ -> analyseCurrentNode () |> cnt

    let analyseExpression environment expression =
        expressionAnalyseInternal singleExpressionAnalyser environment expression id

    let rec private analyseStatementInternal
        (nodeAnalyser: StatementAnalyser)
        environment
        (stmt: Statement)
        (cnt: AnalyserResult list -> AnalyserResult list)
        : AnalyserResult list =

        let analyseRec =
            analyseStatementInternal nodeAnalyser environment

        let statementAnalyser =
            nodeAnalyser environment >> Option.toList

        let analyseCurrentStatement () = statementAnalyser stmt

        let expressionAnalyser =
            analyseExpression environment

        let appendCurrentNodeResults res =
            statementAnalyser stmt |> List.append res

        match stmt with
        | ScopedStatement ss ->
            match ss with
            | ExpressionStatement e ->
                (e |> expressionAnalyser)
                @ analyseCurrentStatement ()
                |> cnt
            | VarDeclarationStatement vds ->
                (vds.InitExpression
                 |> Option.map expressionAnalyser
                 |> Option.defaultValue [])
                @ analyseCurrentStatement ()
                |> cnt
            | BlockStatement block ->
                environment |> Environment.nestNewEmptyEnvironment

                let analyserResults =
                    block.Content
                    |> List.map (
                        ScopedStatement
                        >> analyseRec
                        >> Continuation.fromFun
                    )

                analyserResults
                |> TraversableContinuationList.sequence
                |> Continuation.map (List.collect id)
                |> Continuation.map appendCurrentNodeResults
                |> Continuation.map (fun res ->
                    environment |> Environment.returnToParent
                    res)
                |> Continuation.run cnt
            | IfStatement ifStmt ->
                continuation {
                    let expRes =
                        ifStmt.Condition |> expressionAnalyser

                    let! trueBranch =
                        ifStmt.OnTrue
                        |> ScopedStatement
                        |> analyseRec
                        |> Continuation.fromFun

                    let! falseBranch =
                        match ifStmt.OnFalse with
                        | Some onFalse ->
                            onFalse
                            |> ScopedStatement
                            |> analyseRec
                            |> Continuation.fromFun
                        | None -> [] |> Continuation.ret

                    return
                        expRes
                        @ trueBranch
                          @ falseBranch @ analyseCurrentStatement ()
                }
                |> Continuation.run cnt
            | WhileStatement whileStmt ->
                continuation {
                    let condition =
                        whileStmt.Condition |> expressionAnalyser

                    let! body =
                        whileStmt.Body
                        |> ScopedStatement
                        |> analyseRec
                        |> Continuation.fromFun

                    return condition @ body @ analyseCurrentStatement ()
                }
                |> Continuation.run cnt
            | ForStatement forStatement ->
                continuation {
                    let! initializer =
                        match forStatement.Initializer with
                        | ExpressionInit expression ->
                            expression
                            |> expressionAnalyser
                            |> Continuation.ret
                        | VarDeclarationInit varDeclaration ->
                            do environment |> Environment.nestNewEmptyEnvironment

                            varDeclaration
                            |> VarDeclarationStatement
                            |> ScopedStatement
                            |> analyseRec
                            |> Continuation.fromFun

                    let condition =
                        forStatement.Condition |> expressionAnalyser

                    let incr =
                        forStatement.Increment |> expressionAnalyser

                    let! body =
                        forStatement.Body
                        |> ScopedStatement
                        |> analyseRec
                        |> Continuation.fromFun

                    do
                        match forStatement.Initializer with
                        | VarDeclarationInit _ -> environment |> Environment.returnToParent
                        | _ -> ()

                    return
                        condition
                        @ body
                          @ incr @ initializer @ analyseCurrentStatement ()
                }
                |> Continuation.run cnt
            | ReturnStatement expression ->
                (expression |> expressionAnalyser)
                @ analyseCurrentStatement ()
                |> cnt
            | Empty -> [] |> cnt
        | FunDeclaration funDecl ->
            do
                funDecl.Parameters
                |> List.map (fun param -> (param, ref Value.Void))
                |> Map.ofList
                |> Environment.nestNewEnvironment environment

            continuation {
                let curNodeRes = analyseCurrentStatement ()

                let! bodyRes =
                    funDecl.Body
                    |> BlockStatement
                    |> ScopedStatement
                    |> analyseRec
                    |> Continuation.fromFun

                do environment |> Environment.returnToParent
                return curNodeRes @ bodyRes
            }
            |> Continuation.run cnt
        | UserTypeDeclaration userType ->
            match userType.Kind with
            | Struct ``struct`` ->
                let fieldDeclarationStatements =
                    ``struct``.Members
                    |> Map.values
                    |> Seq.map (fun mb ->
                        match mb with
                        | Field varDeclaration ->
                            varDeclaration
                            |> VarDeclarationStatement
                            |> ScopedStatement)

                do environment |> Environment.nestNewEmptyEnvironment

                fieldDeclarationStatements
                |> Seq.map (analyseRec >> Continuation.fromFun)
                |> Seq.toList
                |> TraversableContinuationList.sequence
                |> Continuation.map (
                    (List.collect id)
                    >> appendCurrentNodeResults
                    >> (fun res ->
                        do environment |> Environment.returnToParent
                        res)
                )
                |> Continuation.run cnt

    let analyseStatement (nodeAnalyser: StatementAnalyser) environment (stmt: Statement) =
        analyseStatementInternal nodeAnalyser environment stmt id

    let analyseProgram environment =
        function
        | Program statements ->
            statements
            |> List.collect (analyseStatement singleStatementAnalyser environment)
    
    let analyseProgramTest environment (stmtsWithLineNmbs:(Statement*Position) list) =
        let analyse = (analyseStatement singleStatementAnalyser environment)         
        stmtsWithLineNmbs |> List.collect (fun (stmt,pos) -> stmt |> analyse |> List.map (fun ar -> (ar,pos)))