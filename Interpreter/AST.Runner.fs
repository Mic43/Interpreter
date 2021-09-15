namespace Interpretr.AST.Runner

open FSharpPlus
open Interpretr.AST.Types
open Interpretr.AST.Types.Errors
open Interpretr.AST.Environment
open Interpretr.AST.ExpEvaluator.Evaluators

//module Environment =

//    type Global =
//        { Functions: Dictionary<Identifier, Function> }

//    type Scoped = { Parent: Environment }

//    and EnvironmentKind =
//        | Global of Global
//        | Scoped of Scoped

//    and Environment =
//        { Variables: Dictionary<Identifier, Value>
//          Kind: EnvironmentKind }

//    let createEmptyGlobal () =
//        { Variables = new Dictionary<Identifier, Value>()
//          Kind =
//              { Functions = new Dictionary<Identifier, Function>() }
//              |> EnvironmentKind.Global }

//    let createNested parent variables =
//        { Variables = new Dictionary<Identifier, Value>(variables |> Map.toSeq |> dict)
//          Kind = { Parent = parent } |> EnvironmentKind.Scoped }

//module Evaluator =
//    let rec tryEvaluate
//        varUpdater
//        varEvaluator
//        constEvaluator
//        binOpEvaluator
//        unaryOpEvaluator
//        funEvaluator
//        (expression: Expression)
//        : Result<Value, Errors.RunError> =

//        let tryEvaluateRec =
//            tryEvaluate varUpdater varEvaluator constEvaluator binOpEvaluator unaryOpEvaluator funEvaluator

//        match expression with
//        | Constant c -> constEvaluator c
//        | Var v -> varEvaluator v
//        | Assignment (ident, expr) ->
//            monad' {
//                let! value = (tryEvaluateRec expr)
//                do! varUpdater ident value
//                return value
//            }
//        | Binary b ->
//            //TODO: parallelize
//            let leftVal = (tryEvaluateRec b.LeftOperand)
//            let rightVal = (tryEvaluateRec b.RightOperand)

//            monad' {
//                let! l = leftVal
//                let! r = rightVal

//                return! binOpEvaluator b.BinaryOp l r
//            }
//        // Result.map2 (binOpEvaluator b.BinaryOp) leftVal rightVal

//        | Unary (op, exp) ->
//            monad' {
//                let! value = tryEvaluateRec exp
//                return unaryOpEvaluator op value
//            }
//        | FunCall fc ->
//            monad' {
//                let! values =
//                    fc.ActualParameters
//                    |> List.map tryEvaluateRec
//                    |> sequence

//                return! funEvaluator fc.Name values
//            }

//open Environment


//module BasicEvaluators =
//    let tryUpdateVar environment identifier newValue =
//        if environment.Variables.ContainsKey identifier then
//            environment.Variables.[identifier] = newValue
//            |> ignore

//            Result.Ok()
//        else
//            environment.Variables.[identifier] = newValue
//            |> ignore

//            "variable not defined"
//            |> (Errors.createResult Errors.ErrorType.Other)

//    let tryGetVar environment identifier =
//        let vars () = environment.Variables

//        (vars().ContainsKey identifier, vars().[identifier])
//        |> Option.ofPair
//        |> Option.toResultWith (Errors.create Errors.ErrorType.Other "variable not defined")

//    let constEvaluator value = Result.Ok value

//    let binaryOpEvaluator op val1 val2 =

//        let evalArithmeticExp val1 val2 =
//            function
//            | (Add _) -> (val1 + val2)
//            | (Sub _) -> (val1 - val2)
//            | (Mul _) -> (val1 * val2)
//            | (Div _) -> (val1 / val2)

//        let evalArithmeticExp op =
//            try
//                (evalArithmeticExp val1 val2 op)
//            with
//            | _ ->
//                "Arithmetic error"
//                |> (Errors.createResult Errors.ErrorType.Other)

//        match op with
//        | BinaryOp.ArithmeticOp op -> (evalArithmeticExp op)

//    let unaryOpEvaluator op value =
//        match op with
//        | Negate -> failwith "Not implemented"

//    let funEvaluator
//        curretEnvironmentSwapper
//        evaluateScopedStmt
//        (environment: Environment)
//        identifier
//        (actualParametersValues: Value list)
//        =
//        let funcs =
//            match environment.Kind with
//            | EnvironmentKind.Global g -> g.Functions
//            | _ -> invalidArg "environment" "functions cannot be defined in local scope"

//        monad' {
//            let! foundFunc =
//                (funcs.ContainsKey identifier, funcs.[identifier])
//                |> Option.ofPair
//                |> Option.toResultWith (Errors.create Errors.ErrorType.Other "function not defined")

//            let! paramsWithValues =
//                actualParametersValues
//                |> Result.protect (List.zip foundFunc.Parameters)
//                |> Result.mapError (fun e -> Errors.create Errors.ErrorType.Evaluation "wrong parameters count")

//            let newEnvironment =
//                paramsWithValues
//                |> Map.ofSeq
//                |> Environment.createNested environment

//            curretEnvironmentSwapper newEnvironment //currentEnvironment <- newEnvironment
//            return! evaluateScopedStmt (foundFunc.Body |> ScopedStatement.Block) expressionEvaluator
//        }

type Runner() =
    let defaultEnvironment = Environment.createEmptyGlobal ()
    let mutable currentEnvironment = defaultEnvironment

    let ignoreResuls (res: Result<Value list, RunError>) =
        res |> Result.map (fun _ -> () |> Value.VoidValue)

    let tryPrint =
        function
        | IntValue iv ->
            printf "%i" iv
            () |> VoidValue |> Result.Ok
        | FloatValue fv ->
            printf "%f" fv
            () |> VoidValue |> Result.Ok
        | VoidValue _ ->
            "Cannot print void value"
            |> (Errors.createResult Errors.Other)

    let rec evaluateScopedStmt expressionEvaluator (exp: ScopedStatement) : Result<Value, Errors.RunError> =
        match exp with
        | ExpressionStatement exp -> expressionEvaluator exp
        | Block block ->
            block
            |> traverse (evaluateScopedStmt expressionEvaluator)
            |> ignoreResuls //TODO: traverse until get return statement
        | PrintStatement exp -> //TODO: maybe put to environment
            monad' {
                let! v = (expressionEvaluator exp)
                return! (v |> tryPrint)
            }

    let rec expressionEvaluator exp =
        tryEvaluate
            (Basic.tryUpdateVar currentEnvironment)
            (Basic.tryGetVar currentEnvironment)
            Basic.constEvaluator
            Basic.binaryOpEvaluator
            Basic.unaryOpEvaluator
            (Basic.funEvaluator
                (fun newEnv -> currentEnvironment <- newEnv)
                (evaluateScopedStmt expressionEvaluator)
                currentEnvironment)
            exp

    let evaluateFunDeclaration fdecl enviroment =
        let message =
            fdecl.Name
            |> Identifier.toStr
            |> sprintf "Function %s already defined"

        (enviroment.Functions.TryAdd(fdecl.Name, fdecl), ())
        |> Option.ofPair
        |> (message
            |> (Errors.create Errors.ErrorType.Other
                >> Option.toResultWith))
        |> Result.map Value.VoidValue

    let evaluateStmt statement =
        match (statement, currentEnvironment.Kind) with
        | (FunDeclaration fd, Global ge) -> evaluateFunDeclaration fd ge
        | (ScopedStatement stmt, _) -> evaluateScopedStmt expressionEvaluator stmt

    member this.Run =
        function
        | Program program -> program |> (traverse evaluateStmt) |> ignoreResuls //TODO: break when first error
