namespace Interpreter.AST

open System.Collections.Generic
open Interpreter.AST
open FSharpPlus

type Global =
    { Functions: IDictionary<Identifier, Callable> }

type Scoped = { Parent: Environment }

and EnvironmentKind =
    | Global of Global
    | Scoped of Scoped

and Environment =
    { Variables: Dictionary<Identifier, Ref<Value>>
      Kind: EnvironmentKind }
    static member CreateGlobal(functions: IDictionary<Identifier, Callable>) =
        { Variables = new Dictionary<Identifier, Ref<Value>>()
          Kind = { Functions = functions } |> Global }

type ExecutionEnvironment =
    private
        { mutable CurrentEnvironment: Environment
          VariablesCache: Dictionary<Identifier, Ref<Value>>
          Global: Environment }
    member this.Current = this.CurrentEnvironment

    member this.IsCurrentGlobal =
        match this.Current.Kind with
        | Global _ -> true
        | _ -> false

module Environment =

    let private createEmptyGlobal () =
        { Variables = new Dictionary<Identifier, Ref<Value>>()
          Kind =
            { Functions = new Dictionary<Identifier, Callable>() }
            |> Global }

    let create defaultGlobal =
        { Global = defaultGlobal
          VariablesCache = new Dictionary<Identifier, Ref<Value>>()
          CurrentEnvironment = defaultGlobal }

    let createEmpty () = create (createEmptyGlobal ())

    let private createNested parent variables =
        { Variables = new Dictionary<Identifier, Ref<Value>>(variables |> Map.toSeq |> dict)
          Kind = { Parent = parent } |> EnvironmentKind.Scoped }

    let nestNewEnvironment (environment: ExecutionEnvironment) newVariables =
        environment.CurrentEnvironment <- (createNested environment.CurrentEnvironment newVariables)
        environment.VariablesCache.Clear()

    let nestNewEmptyEnvironment (environment: ExecutionEnvironment) =
        nestNewEnvironment environment Map.empty<Identifier, Ref<Value>>

    let private tryFindVariableEnvironment (environment: ExecutionEnvironment) identifier =
        let rec recurse (environment: Environment) =
            let vars = environment.Variables

            if vars.ContainsKey identifier then
                environment |> Some
            else
                match environment.Kind with
                | Global _ -> None
                | Scoped s -> recurse s.Parent

        recurse environment.CurrentEnvironment

    let tryUpdateVar (environment: ExecutionEnvironment) identifier newValue =
        monad' {
            let! targetEnvironment = tryFindVariableEnvironment environment identifier
            targetEnvironment.Variables.[identifier].Value <- newValue
            return ()
        }
        |> Option.toResultWith (
            Errors.create
                ErrorType.Other
                (identifier.ToStr()
                 |> sprintf "variable not defined: %s")
        )

    let tryDefineVar (environment: ExecutionEnvironment) identifier value =
        if environment.Current.Variables.ContainsKey identifier then
            (sprintf "variable already defined: %s" (identifier.ToStr()))
            |> (Errors.createResult ErrorType.Other)
        else
            environment.Current.Variables.Add(identifier, ref value)
            Result.Ok()

    let tryGetVarValue (environment: ExecutionEnvironment) identifier =
        if environment.VariablesCache.ContainsKey(identifier) then
            environment.VariablesCache.[identifier] |> Ok
        else            
            monad' {
                let! targetEnvironment = tryFindVariableEnvironment environment identifier
                let found = targetEnvironment.Variables.[identifier]
                environment.VariablesCache.Add(identifier,found)
                return found
            }
            |> Option.toResultWith (
                Errors.create
                    ErrorType.Other
                    (identifier.ToStr()
                     |> sprintf "variable not defined: %s")
            )

    let tryDefineCallable enviroment (callable: Callable) =
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

    let returnToParent (environment: ExecutionEnvironment) =
        environment.CurrentEnvironment <-
            (match environment.CurrentEnvironment.Kind with
             | Scoped s -> s.Parent
             | Global _ -> failwith "there is no parent environment for current environment")

        environment.VariablesCache.Clear()
