namespace Interpreter.AST

open FSharpPlus
open System.Collections.Generic

module Interpreter =
    let run environment (Statements statementsList) =
        statementsList
        |> Traversable.traverseMTail (StmtEvaluator.evaluate environment)
        |> Result.mapError List.singleton
        |> Value.getLastResultOrVoid

    let runWithDefaultEnvironment program =
        let defaultEnvironment =
            [ "print", DefaultEnvironment.tryPrint
              "println", DefaultEnvironment.tryPrintLn
              "readInt", DefaultEnvironment.tryReadInt
              "len", DefaultEnvironment.tryGetLen ]
            |> Map.ofList

        run
            (defaultEnvironment
             |> Environment.fromDefaultFunctions)
            program
