namespace Interpretr.AST.ExpEvaluator

open Interpretr.AST.Environment
open Interpretr.AST.Types
open FSharpPlus


module Evaluators =
    module Basic = 
        let tryUpdateVar environment identifier newValue =
            if environment.Variables.ContainsKey identifier then
                environment.Variables.[identifier] = newValue
                |> ignore

                Result.Ok()
            else
                environment.Variables.[identifier] = newValue
                |> ignore

                "variable not defined"
                |> (Errors.createResult Errors.ErrorType.Other)

        let tryGetVar environment identifier =
            let vars () = environment.Variables

            (vars().ContainsKey identifier, vars().[identifier])
            |> Option.ofPair
            |> Option.toResultWith (Errors.create Errors.ErrorType.Other "variable not defined")

        let constEvaluator value = Result.Ok value

        let binaryOpEvaluator op val1 val2 =

            let evalArithmeticExp val1 val2 =
                function
                | (Add _) -> (val1 + val2)
                | (Sub _) -> (val1 - val2)
                | (Mul _) -> (val1 * val2)
                | (Div _) -> (val1 / val2)

            let evalArithmeticExp op =
                try
                    (evalArithmeticExp val1 val2 op)
                with
                | _ ->
                    "Arithmetic error"
                    |> (Errors.createResult Errors.ErrorType.Other)

            match op with
            | BinaryOp.ArithmeticOp op -> (evalArithmeticExp op)

        let unaryOpEvaluator op value =
            match op with
            | Negate -> failwith "Not implemented"

        let funEvaluator
            curretEnvironmentSwapper
            evaluateScopedStmt
            (environment: Environment)
            funIdentifier
            (actualParametersValues: Value list)
            =
            let funcs =
                match environment.Kind with
                | EnvironmentKind.Global g -> g.Functions
                | _ -> invalidArg "environment" "functions cannot be defined in local scope"

            monad' {
                let! foundFunc =
                    (funcs.ContainsKey funIdentifier, funcs.[funIdentifier])
                    |> Option.ofPair
                    |> Option.toResultWith (Errors.create Errors.ErrorType.Other "function not defined")

                let! paramsWithValues =
                    actualParametersValues
                    |> Result.protect (List.zip foundFunc.Parameters)
                    |> Result.mapError (fun e -> Errors.create Errors.ErrorType.Evaluation "wrong parameters count")

                let newEnvironment =
                    paramsWithValues
                    |> Map.ofSeq
                    |> Environment.createNested environment

                curretEnvironmentSwapper newEnvironment //currentEnvironment <- newEnvironment
                return! evaluateScopedStmt (foundFunc.Body |> ScopedStatement.Block)
            }

    let rec tryEvaluate
        varUpdater
        varEvaluator
        constEvaluator
        binOpEvaluator
        unaryOpEvaluator
        funEvaluator
        (expression: Expression)
        : Result<Value, Errors.RunError> =

        let tryEvaluateRec =
            tryEvaluate varUpdater varEvaluator constEvaluator binOpEvaluator unaryOpEvaluator funEvaluator

        match expression with
        | Constant c -> constEvaluator c
        | Var v -> varEvaluator v
        | Assignment (ident, expr) ->
            monad' {
                let! value = (tryEvaluateRec expr)
                do! varUpdater ident value
                return value
            }
        | Binary b ->
            //TODO: parallelize
            let leftVal = (tryEvaluateRec b.LeftOperand)
            let rightVal = (tryEvaluateRec b.RightOperand)

            monad' {
                let! l = leftVal
                let! r = rightVal

                return! binOpEvaluator b.BinaryOp l r
            }
        // Result.map2 (binOpEvaluator b.BinaryOp) leftVal rightVal

        | Unary (op, exp) ->
            monad' {
                let! value = tryEvaluateRec exp
                return unaryOpEvaluator op value
            }
        | FunCall fc ->
            monad' {
                let! values =
                    fc.ActualParameters
                    |> List.map tryEvaluateRec
                    |> sequence

                return! funEvaluator fc.Name values
            }
