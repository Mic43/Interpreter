﻿namespace Interpreter.AST

open FSharpPlus

module Runner =
    // let defaultEnvironment = Environment.createEmptyGlobal ()
    // let mutable currentEnvironment = defaultEnvironment

    let ignoreResuls (res: Result<Value list, RunError>) = res |> Result.map (fun _ -> Value.Void)

    let getLastResultOrVoid (res: Result<Value list, RunError>) =
        res
        |> Result.map
            (fun vl ->
                vl
                |> (List.tryLast >> (Option.defaultValue Value.Void)))

    let tryPrint =
        function
        | IntValue iv ->
            printf "%i" iv
            Value.Void |> Result.Ok
        | FloatValue fv ->
            printf "%f" fv
            Value.Void |> Result.Ok
        | VoidValue _ ->
            "Cannot print void value"
            |> (Errors.createResult Other)


    let rec evaluateScopedStmt (environment: ExecutionEnvironment) (exp: ScopedStatement) : Result<Value, RunError> =
        //let evaluateScopedStmtRec = evaluateScopedStmt environment

        let evaluateBlock block =
            block
            |> traverse (evaluateScopedStmt environment)
            |> getLastResultOrVoid

        let evaluateFunction funIdentifier (actualParametersValues: Value list) =
            let funcs =
                match environment.CurrentEnvironment.Kind with
                | Global g -> g.Functions
                | _ -> invalidArg "environment" "functions cannot be defined in local scope"

            monad' {
                let! foundFunc =
                    (funcs.ContainsKey funIdentifier, funcs.[funIdentifier])
                    |> Option.ofPair
                    |> Option.toResultWith (Errors.create ErrorType.Other "function not defined")

                let! paramsWithValues =
                    actualParametersValues
                    |> Result.protect (List.zip foundFunc.Parameters)
                    |> Result.mapError (fun e -> Errors.create ErrorType.Evaluation "wrong parameters count")

                Environment.nestNewEnvironment environment (paramsWithValues |> Map.ofSeq)
                let! res = evaluateBlock foundFunc.Body
                environment |> Environment.returnToParent
                return res
            }

        let evaluateExpression exp =
            ExpEvaluator.tryEvaluate
                (Environment.tryUpdateVar environment)
                (Environment.tryGetVar environment)
                ExpEvaluator.constEvaluator
                ExpEvaluator.binaryOpEvaluator
                ExpEvaluator.unaryOpEvaluator
                evaluateFunction
                exp

        match exp with
        | ExpressionStatement exp -> evaluateExpression exp
        | Block block ->
            environment |> Environment.nestNewEmptyEnvironment

            monad' {
                let! res = evaluateBlock block
                environment |> Environment.returnToParent
                return res
            }
        | VarDeclaration vd ->
            monad' {
                let! initVal = vd.Initializer |> evaluateExpression

                return!
                    (Environment.tryDefineVar environment vd.Name initVal)
                    |> (Result.map Value.createVoid)
            }

        | PrintStatement exp -> //TODO: maybe put to environment
            monad' {
                let! v = (evaluateExpression exp)
                return! (v |> tryPrint)
            }
        | Empty _ -> Value.Void |> Result.Ok

    let evaluateStmt (environment: ExecutionEnvironment) statement =
        match (statement, environment.IsCurrentGlobal) with
        | (FunDeclaration fd, true) -> Environment.tryDefineFunction environment fd
        | (FunDeclaration _, false) ->
            "Function can be defined only on global scope"
            |> Errors.createResult Other
        | (ScopedStatement stmt, _) -> evaluateScopedStmt environment stmt

    let run (Program statementsList) =
        let environment = Environment.createEmpty ()

        statementsList
        |> (Utils.traverseM (evaluateStmt environment))
        |> getLastResultOrVoid
