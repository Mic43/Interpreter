namespace Interpreter.AST

open System.Collections.Generic
open Microsoft.FSharp.Core


module SemanticAnalysers =
    type AnalyserResult =
        | MisplacedReturnStatement
        | VariableAlreadyDefined of string
        | VariableNotDefined of string
        | FunctionNotDefined of string
        | ExpressionMustBeLValue of Expression

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
            | _ -> None

    let private singleExpressionAnalyser environment (exp: Expression) =
        match exp with
        | Mutable (Var v) ->
            match v |> Environment.tryGetVarValue environment with
            | Ok _ -> None
            | Error _ ->
                v.Get()
                |> AnalyserResult.VariableNotDefined
                |> Some
        | Assignment (Mutable _, _) -> None
        | Assignment (exp, _) -> exp |> ExpressionMustBeLValue |> Some
        | FunCall funCall ->
            match funCall.Name
                  |> Environment.tryGetVarValue environment
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

        let appendCurrentNodeResults =
            analyseCurrentNode () |> List.append

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

        let appendCurrentNodeResults =
            analyseCurrentStatement () |> List.append

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
                    let! body =
                        forStatement.Body
                        |> ScopedStatement
                        |> analyseRec
                        |> Continuation.fromFun

                    let condition =
                        forStatement.Condition |> expressionAnalyser

                    let incr =
                        forStatement.Increment |> expressionAnalyser

                    let! initializer =
                        match forStatement.Initializer with
                        | ExpressionInit expression ->
                            expression
                            |> expressionAnalyser
                            |> Continuation.ret
                        | VarDeclarationInit varDeclaration ->
                            varDeclaration
                            |> VarDeclarationStatement
                            |> ScopedStatement
                            |> analyseRec
                            |> Continuation.fromFun

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
            | Empty -> []
        | FunDeclaration funDecl ->
            funDecl.Body
            |> BlockStatement
            |> ScopedStatement
            |> analyseRec
            |> Continuation.fromFun
            |> Continuation.map appendCurrentNodeResults
            |> Continuation.run cnt
        | UserTypeDeclaration userType ->
            match userType.Kind with
            | Struct ``struct`` ->
                let statements =
                    ``struct``.Members
                    |> Map.values
                    |> Seq.map (fun mb ->
                        match mb with
                        | Field varDeclaration ->
                            varDeclaration
                            |> VarDeclarationStatement
                            |> ScopedStatement)

                statements
                |> Seq.map (analyseRec >> Continuation.fromFun)
                |> Seq.toList
                |> TraversableContinuationList.sequence
                |> Continuation.map ((List.collect id) >> appendCurrentNodeResults)
                |> Continuation.run cnt

    let rec private analyseStatement (nodeAnalyser: StatementAnalyser) environment (stmt: Statement) =
        analyseStatementInternal nodeAnalyser environment stmt id
