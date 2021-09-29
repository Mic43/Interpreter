﻿namespace Interpreter.AST

open FSharpPlus
open System.Collections.Generic

module Runner =
    let private ignoreResuls (res: Result<Value list, RunError>) = res |> Result.map (fun _ -> Value.Void)

    let private getLastResultOrVoid (res: Result<Value list, RunError>) =
        res
        |> Result.map
            (fun vl ->
                vl
                |> (List.tryLast >> (Option.defaultValue Value.Void)))


    let rec private evaluateScopedStmt
        (environment: ExecutionEnvironment)
        (exp: ScopedStatement)
        : Result<Value, RunError> =
        let evaluateScopedStmtRec = evaluateScopedStmt environment

        let evaluateBlock block =
            block.Content
            |> traverse (evaluateScopedStmt environment)
            |> getLastResultOrVoid

        let evaluateFunctionCall funIdentifier (actualParametersValues: Value list) =
            let funcs =
                match environment.Global.Kind with
                | Global g -> g.Functions
                | _ -> invalidOp "default environment must be of global kind"

            monad' {
                let! foundFunc =
                    (funcs.ContainsKey funIdentifier, funcs.[funIdentifier])
                    |> Option.ofPair
                    |> Option.toResultWith (Errors.create ErrorType.Other "function not defined")

                match foundFunc with
                | Function func ->
                    let! paramsWithValues =
                        actualParametersValues
                        |> Result.protect (List.zip func.Parameters)
                        |> Result.mapError (fun e -> Errors.create ErrorType.Evaluation "wrong parameters count")

                    Environment.nestNewEnvironment environment (paramsWithValues |> Map.ofSeq)
                    let! res = evaluateBlock func.Body
                    environment |> Environment.returnToParent

                    return res
                | CompiledFunction compiledFun -> return! compiledFun.Execute actualParametersValues
            }

        let evaluateExpression exp =
            ExpEvaluator.tryEvaluate
                (Environment.tryUpdateVar environment)
                (Environment.tryGetVar environment)
                ExpEvaluator.constEvaluator
                ExpEvaluator.binaryOpEvaluator
                ExpEvaluator.unaryOpEvaluator
                evaluateFunctionCall
                exp

        match exp with
        | ExpressionStatement exp -> evaluateExpression exp
        | BlockStatement block ->
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

        | Empty _ -> Value.Void |> Result.Ok
        | IfStatement (ifs) ->
            monad' {
                let! condRes = ifs.Condition |> evaluateExpression

                if (condRes |> Value.toBool) then
                    return! (ifs.OnTrue |> evaluateScopedStmtRec)
                else
                    return! (ifs.OnFalse |> evaluateScopedStmtRec)
            }
        | WhileStatement (ws) ->
            let rec loop () : Result<Value, RunError> =
                monad' {
                    let! condVal = ws.Condition |> evaluateExpression

                    if (condVal |> Value.toBool) then
                        ws.Body |> evaluateScopedStmtRec |> ignore
                        return! loop ()
                    else
                        return Value.Void
                }

            loop ()


    let private evaluateStmt (environment: ExecutionEnvironment) statement =
        match (statement, environment.IsCurrentGlobal) with
        | (FunDeclaration fd, true) -> Environment.tryDefineCallable environment (fd |> Function)
        | (FunDeclaration _, false) ->
            "Function can be defined only on global scope"
            |> Errors.createResult Other
        | (ScopedStatement stmt, _) -> evaluateScopedStmt environment stmt

    let runWithDefaultEnv defaultEnvironment (Program statementsList) =

        let createDefaultEnvironment defaultEnvironment =
            defaultEnvironment
            |> Map.toList
            |> List.map (
                (fun (name, func) -> Callable.FromFunction(name |> Identifier.create) func)
                >> fun callable -> (callable.Name, callable)
            )
            |> Map.ofList
            |> Dictionary
            |> Environment.CreateGlobal

        let environment =
            Environment.create (createDefaultEnvironment defaultEnvironment)

        statementsList
        |> (Utils.traverseM (evaluateStmt environment))
        |> getLastResultOrVoid

    let run program =
        let defaultEnvironment =
            [ "print", DefaultEnvironment.tryPrint
              "readInt", DefaultEnvironment.readInt ]
            |> Map.ofList

        runWithDefaultEnv defaultEnvironment program
