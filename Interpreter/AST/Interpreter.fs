namespace Interpreter.AST

open FSharpPlus
open System.Collections.Generic

module Interpreter =
    let run environment (Statements statementsList) =
        statementsList
        |> Traversable.traverseMTail (StmtEvaluator.evaluate environment)
        |> Result.mapError List.singleton
        |> Value.getLastResultOrVoid

    // let runTest environment (statementWithInfos : StatementWithInfo list) =
    //     let statements,infos  = statementWithInfos |> List.map (fun s -> (s.Statement,s.Info)) |> List.unzip
    //     statements |> Statements |> run environment |> List.zipShortest infos                      
         
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
