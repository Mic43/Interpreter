namespace Interpreter.AST

open System.Collections.Generic
open Interpreter.AST
open FSharpPlus

type Global =
    { Functions: Dictionary<Identifier, Function> }

type Scoped = { Parent: Environment }

and EnvironmentKind =
    | Global of Global
    | Scoped of Scoped

and Environment =
    { Variables: Dictionary<Identifier, Value>
      Kind: EnvironmentKind }

module Environment =
    let createEmptyGlobal () =
        { Variables = new Dictionary<Identifier, Value>()
          Kind =
              { Functions = new Dictionary<Identifier, Function>() }
              |> EnvironmentKind.Global }

    let createNested parent variables =
        { Variables = new Dictionary<Identifier, Value>(variables |> Map.toSeq |> dict)
          Kind = { Parent = parent } |> EnvironmentKind.Scoped }

    let tryUpdateVar environment identifier newValue =
        if environment.Variables.ContainsKey identifier then
            environment.Variables.[identifier] = newValue
            |> ignore

            Result.Ok()
        else
            "variable not defined"
            |> (Errors.createResult ErrorType.Other)

    let tryDefineVar environment identifier value =
        if environment.Variables.ContainsKey identifier then
            "variable already defined defined"
            |> (Errors.createResult ErrorType.Other)
        else
            environment.Variables.Add(identifier, value)
            Result.Ok()

    let tryGetVar environment identifier =
        let vars () = environment.Variables

        (vars().ContainsKey identifier, vars().[identifier])
        |> Option.ofPair
        |> Option.toResultWith (Errors.create ErrorType.Other "variable not defined")

    let tryDefineFunction enviroment fdecl  =
        let message =
            fdecl.Name
            |> Identifier.toStr
            |> sprintf "Function %s already defined"

        (enviroment.Functions.TryAdd(fdecl.Name, fdecl), ())
        |> Option.ofPair
        |> (message
            |> (Errors.create ErrorType.Other
                >> Option.toResultWith))
        |> Result.map Value.createVoid
