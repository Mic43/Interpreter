namespace Interpreter.AST

open System.Collections.Generic
open Microsoft.FSharp.Core


module SemanticAnalysers =
    type AnalyserResult =
        | MisplacedReturnStatement
        | VariableAlreadyDefined of string
        | VariableNotDefined of string
        | ExpressionMustBeLValue of Expression

    type StatementAnalyser = ExecutionEnvironment -> Statement -> AnalyserResult option
    type ExpressionAnalyser = ExecutionEnvironment -> Expression -> AnalyserResult option

    let singleStatementAnalyser: StatementAnalyser =
        fun environment statement ->
            match statement with
            | ScopedStatement (VarDeclarationStatement v) ->
                let error =
                    Environment.tryDefineVar environment v.Name Value.Void

                match error with
                | Ok _ -> None
                | Error _ ->
                    v.Name.ToStr()
                    |> AnalyserResult.VariableAlreadyDefined
                    |> Some
            | ScopedStatement (ReturnStatement _) ->
                if environment.IsCurrentGlobal then
                    Some(MisplacedReturnStatement)
                else
                    None
            | _ -> None

    let singleExpressionAnalyser (exp: Expression) environment =
        match exp with
        | Mutable (Var v) ->
            match v |> Environment.tryGetVarValue environment with
            | Ok _ -> None
            | Error _ ->
                v.ToStr()
                |> AnalyserResult.VariableNotDefined
                |> Some
        | Assignment (Mutable _, _) -> None
        | Assignment (exp, _) -> exp |> ExpressionMustBeLValue |> Some
        | _ -> None

    let rec private expressionAnalyse
        (nodeAnalyser: ExpressionAnalyser)
        environment
        expression
        cnt
        : AnalyserResult list =

        let expressionAnalyse =
            expressionAnalyse nodeAnalyser environment

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
            |> List.map (fun exp -> expressionAnalyse exp |> Continuation.fromFun)
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
        | Mutable  (MemberAccess (leftExp, _)) ->
            leftExp |> Mutable |> genContinuationForSingleChild  |> Continuation.run cnt
        | Mutable _ ->  analyseCurrentNode () |> cnt
        | Constant _ -> analyseCurrentNode () |> cnt

    // let rec private analyse
    //     (nodeAnalyser: StatementAnalyser)
    //     environment
    //     (stmt: Statement)
    //     (cnt: AnalyserResult list -> AnalyserResult list)
    //     : AnalyserResult list =
    //
    //     let analyse =
    //         analyse nodeAnalyser environment
    //
    //     let nodeAnalyser = nodeAnalyser environment
    //     //let analyseScopedStatement (stmt: ScopedStatement) cnt =
    //     //  ()
    //     // match stmt with
    //     // | ExpressionStatement e -> e |> simplifyExp |> ExpressionStatement
    //     // | VarDeclarationStatement ({ InitExpression = ie } as vds) ->
    //     //     { vds with InitExpression = ie |> Option.map simplifyExp }
    //     //     |> VarDeclarationStatement
    //     // | BlockStatement block -> block |> simplifyBlock |> BlockStatement
    //     // | IfStatement ifStmt -> ifStmt |> simplifyIf
    //     // | WhileStatement whileStmt -> whileStmt |> simplifyWhile |> WhileStatement
    //     // | ForStatement forStmt -> forStmt |> simplifyForStmt |> ForStatement
    //     // | ReturnStatement exp -> exp |> simplifyExp |> ExpressionStatement
    //     // | Empty -> stmt
    //
    //     // let genContinuations
    //
    //     match stmt with
    //     | ScopedStatement ss ->
    //         match ss with
    //         | ExpressionStatement e -> analyse stmt (fun res -> res @ (nodeAnalyser stmt |> Option.toList) |> cnt)
    //     // | VarDeclarationStatement vds ->
    //     // | BlockStatement block ->
    //     // | IfStatement ifStmt ->
    //     // | WhileStatement whileStmt ->
    //     // | ForStatement forStmt ->
    //     // | ReturnStatement exp ->
    //     // | Empty -> stmt
    //     | FunDeclaration fd -> fd.Body.Content |> List.fold
    //     | UserTypeDeclaration utd -> stmt
//
// let rec fold f expression =
//     let foldR = fold f
//     match expression with
//         | Binary ({ LeftOperand = left
//                     RightOperand = right } as b) ->
//
//          | SimpleUnary (op, operand) -> (op, operand |> foldR) |> SimpleUnary
//          | Increment (op, exp) -> (op, exp |> foldR) |> Increment
//          | Assignment (lValExp, exp) -> (lValExp, exp |> foldR) |> Assignment
//          | ListCreation expList -> expList |> List.map foldR |> ListCreation
//          | UserTypeCreation ({ FieldsInitialization = fieldsInit } as utc) ->
//              { utc with FieldsInitialization = fieldsInit |> Map.mapValues simplify }
//              |> UserTypeCreation
//          | FunCall ({ ActualParameters = actualParamsExps } as funcCall) ->
//              { funcCall with ActualParameters = actualParamsExps |> List.map foldR }
//              |> FunCall
//          | Mutable (IndexedVar (mexp, exp)) -> (mexp, exp |> foldR) |> IndexedVar |> Mutable
//          | Mutable _ -> exp
//          | Constant _ -> exp)
