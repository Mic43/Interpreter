namespace Interpreter.AST

open FSharpPlus
open System.Collections.Generic

module Interpreter =
    let runProgram environment (Statements statementsList) =
        statementsList
        |> Traversable.traverseMTail (StmtEvaluator.evaluate environment)
        |> Result.mapError List.singleton
        |> Value.getLastResultOrVoid

    let run environment (statementWithInfos : StatementWithInfo list)  =
        statementWithInfos
        |> Traversable.traverseMTail (StmtEvaluator.evaluateWithInfo environment)
        |> Result.mapError List.singleton
        |> Value.getLastResultOrVoid
                           
    let runProgramWithDefaultEnvironment program =
        let defaultEnvironment =
            [ "print", DefaultEnvironment.tryPrint
              "println", DefaultEnvironment.tryPrintLn
              "readInt", DefaultEnvironment.tryReadInt
              "len", DefaultEnvironment.tryGetLen ]
            |> Map.ofList

        runProgram
            (defaultEnvironment
             |> Environment.fromDefaultFunctions)
            program
