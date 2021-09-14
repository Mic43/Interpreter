﻿namespace Interpretr.AST.Runner

open Interpretr.AST.Types
open FSharpPlus
open System.Collections.Generic
open System
open Identifier
open Value

module Environment =

    type Global =
        { Functions: Dictionary<Identifier, Function> }

    type Scoped = { Parent: Environment }

    and EnvironmentKind =
        | Global of Global
        | Scoped of Scoped

    and Environment =
        { Variables: Dictionary<Identifier, Value>
          Kind: EnvironmentKind }

    let createEmptyGlobal () =
        { Variables = new Dictionary<Identifier, Value>()
          Kind =
              { Functions = new Dictionary<Identifier, Function>() }
              |> EnvironmentKind.Global }

module Errors =
    type ErrorType =
        | Evaluation
        | Other

    type RunError = { Message: string; Type: ErrorType }

    let create str errorType = { Message = str; Type = errorType }
    let createResult str errorType = Result.Error(create str errorType)

module Evaluator =
    //  type EvalError = { Message: string }
    open Errors

    let rec tryEvaluate
        varUpdater
        varEvaluator
        constEvaluator
        binOpEvaluator
        unaryOpEvaluator
        funEvaluator
        (expression: Expression)
        : Result<Value, RunError> =

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

open Environment

type Runner() =
    let defaultEnvironment = Environment.createEmptyGlobal ()
    let mutable currentEnvironment = defaultEnvironment

    //let expressionEvaluator exp =
    //    let tryUpdateVar environment identifier newValue =
    //        if environment.Variables.ContainsKey identifier then
    //            environment.Variables.[identifier] = newValue
    //            |> ignore

    //            Result.Ok()
    //        else
    //            environment.Variables.[identifier] = newValue
    //            |> ignore

    //            Errors.createResult "variable not defined" Errors.ErrorType.Other

    //    let tryGetVar environment identifier =
    //        let vars () = environment.Variables

    //        (vars().ContainsKey identifier, vars().[identifier])
    //        |> Option.ofPair
    //        |> Option.toResultWith (Errors.create "variable not defined" Errors.ErrorType.Other)

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
    //                (evalArithmeticExp val1 val2 op) |> Result.Ok
    //            with
    //            | _ -> (Errors.createResult "Arithmetic error" Errors.ErrorType.Other)

    //        match op with
    //        | BinaryOp.ArithmeticOp op -> (evalArithmeticExp op)

    //    let unaryOpEvaluator op value =
    //        match op with
    //        | Negate -> failwith "Not implemented"

    //    let funEvaluator environment identifier actualParameters =
    //        let funcs () = environment.Functions

    //        monad' {
    //            let! f = (funcs().ContainsKey identifier, funcs().[identifier])
    //                    |> Option.ofPair
    //                    |> Option.toResultWith (Errors.create "function not defined" Errors.ErrorType.Other)
                
    //        }            
      
    //    Evaluator.tryEvaluate
    //        (tryUpdateVar currentEnvironment)
    //        (tryGetVar currentEnvironment)
    //        constEvaluator
    //        binaryOpEvaluator
    //        unaryOpEvaluator
    //        exp

    let rec runScopedStmt exp =
        
        let expressionEvaluator exp =
            let tryUpdateVar environment identifier newValue =
                if environment.Variables.ContainsKey identifier then
                    environment.Variables.[identifier] = newValue
                    |> ignore

                    Result.Ok()
                else
                    environment.Variables.[identifier] = newValue
                    |> ignore

                    Errors.createResult "variable not defined" Errors.ErrorType.Other

            let tryGetVar environment identifier =
                let vars () = environment.Variables

                (vars().ContainsKey identifier, vars().[identifier])
                |> Option.ofPair
                |> Option.toResultWith (Errors.create "variable not defined" Errors.ErrorType.Other)

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
                        (evalArithmeticExp val1 val2 op) |> Result.Ok
                    with
                    | _ -> (Errors.createResult "Arithmetic error" Errors.ErrorType.Other)

                match op with
                | BinaryOp.ArithmeticOp op -> (evalArithmeticExp op)

            let unaryOpEvaluator op value =
                match op with
                | Negate -> failwith "Not implemented"

            let funEvaluator environment identifier actualParameters =
                let funcs () = environment.Functions

                monad' {
                    let! f = (funcs().ContainsKey identifier, funcs().[identifier])
                            |> Option.ofPair
                            |> Option.toResultWith (Errors.create "function not defined" Errors.ErrorType.Other)
                       
                }            
             
            Evaluator.tryEvaluate
                (tryUpdateVar currentEnvironment)
                (tryGetVar currentEnvironment)
                constEvaluator
                binaryOpEvaluator
                unaryOpEvaluator
                funEvaluator
                exp
                
        match exp with 
        | ExpressionStatement exp -> expressionEvaluator exp

    let runStmt statement =
        match (statement, currentEnvironment.Kind) with
        | (FunDeclaration fd, Global ge) ->
            (ge.Functions.TryAdd(fd.Name, fd),
             Errors.create (sprintf "Function %s already defined" (Identifier.toStr fd.Name)) Errors.ErrorType.Other)
            |> Option.ofPair
        | (ScopedStatement s, _) -> runScopedStmt s
        | _ -> failwith "internal error"

    member this.Run =
        function
        | Program program -> program |> (traverse runStmt) //TODO: break when first option is some
