namespace Interpreter.AST

open FSharpPlus
open System.Collections.Generic

module Interpreter =

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
        |> Utils.traverseMTail (StmtEvaluator.evaluate environment)
        |> Value.getLastResultOrVoid

    let run program =
        let defaultEnvironment =
            [ "print", DefaultEnvironment.tryPrint
              "println", DefaultEnvironment.tryPrintLn
              "readInt", DefaultEnvironment.tryReadInt          
              "len", DefaultEnvironment.tryGetLen ]
            |> Map.ofList

        runWithDefaultEnv defaultEnvironment program
