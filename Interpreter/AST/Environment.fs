﻿namespace Interpreter.AST

open System.Collections.Generic
open Interpreter.AST
open FSharpPlus

type Global =
    { Functions: Dictionary<Identifier, Callable> }

type Scoped = { Parent: Environment }

and EnvironmentKind =
    | Global of Global
    | Scoped of Scoped

and Environment =
    { Variables: Dictionary<Identifier, Value>
      Kind: EnvironmentKind }

type ExecutionEnvironment =
    private
        { mutable CurrentEnvironment: Environment
          Global: Environment }
    member this.Current = this.CurrentEnvironment

    member this.IsCurrentGlobal =
        match this.Current.Kind with
        | Global _ -> true
        | _ -> false

module Environment =

    let private createEmptyGlobal () =
        { Variables = new Dictionary<Identifier, Value>()
          Kind =
              { Functions = new Dictionary<Identifier, Callable>() }
              |> EnvironmentKind.Global }

    let create defaultGlobal =
        { Global = defaultGlobal
          CurrentEnvironment = defaultGlobal }

    let createEmpty () = create (createEmptyGlobal ())

    let private createNested parent variables =
        { Variables = new Dictionary<Identifier, Value>(variables |> Map.toSeq |> dict)
          Kind = { Parent = parent } |> EnvironmentKind.Scoped }

    let nestNewEnvironment (environment: ExecutionEnvironment) newVariables =
        environment.CurrentEnvironment <- (createNested environment.CurrentEnvironment newVariables)

    let nestNewEmptyEnvironment (environment: ExecutionEnvironment) =
        nestNewEnvironment environment Map.empty<Identifier, Value>

    let tryUpdateVar (environment: ExecutionEnvironment) identifier newValue =
        if environment.Current.Variables.ContainsKey identifier then
            environment.Current.Variables.[identifier] = newValue
            |> ignore

            Result.Ok()
        else
            "variable not defined"
            |> (Errors.createResult ErrorType.Other)

    let tryDefineVar (environment: ExecutionEnvironment) identifier value =
        if environment.Current.Variables.ContainsKey identifier then
            "variable already defined defined"
            |> (Errors.createResult ErrorType.Other)
        else
            environment.Current.Variables.Add(identifier, value)
            Result.Ok()

    let tryGetVar (environment: ExecutionEnvironment) identifier =
        let vars () = environment.Current.Variables

        (vars().ContainsKey identifier, vars().[identifier])
        |> Option.ofPair
        |> Option.toResultWith (Errors.create ErrorType.Other "variable not defined")


    let tryDefineCallable enviroment (callable:Callable) =
        let message =
            callable.Name
            |> Identifier.toStr
            |> sprintf "Function %s already defined"
        
        match enviroment.Global.Kind with
        | Global g ->
            (g.Functions.TryAdd(callable.Name, callable), ())
            |> Option.ofPair
            |> (message
                |> (Errors.create ErrorType.Other
                    >> Option.toResultWith))
            |> Result.map Value.createVoid
        | Scoped (_) -> failwith "Cannot define function inside local scope"

    let returnToParent (enviromnet: ExecutionEnvironment) =
        enviromnet.CurrentEnvironment <-
            (match enviromnet.CurrentEnvironment.Kind with
             | Scoped s -> s.Parent
             | Global _ -> failwith "there is no parent environment for current environment")
