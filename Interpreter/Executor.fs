namespace Interpreter

open System
open Interpreter.AST
open FParsec.CharParsers
open FSharpPlus
open Interpreter.AST.SemanticAnalyser


// type RunnerError =
//     | ExecuteError of ExecuteError
//     | ParseError of string

module Executor =
    let private preprocessComments programStr =
        let comment = "//"

        programStr
        |> String.split [ "\n" ]
        |> Seq.map (fun line -> line |> String.split [ comment ] |> Seq.head)
        |> Seq.reduce (fun acc s -> sprintf "%s\n%s" acc s)

    let private dummyPreprocess programStr = programStr

    let private defaultEnvironment =
        [ "print", DefaultEnvironment.tryPrint
          "println", DefaultEnvironment.tryPrintLn
          "readInt", DefaultEnvironment.tryReadInt
          "len", DefaultEnvironment.tryGetLen ]
        |> Map.ofList

    let private createEnvironment () =
        defaultEnvironment
        |> Environment.fromDefaultFunctions

    let run programString =
        let semanticErrorsMapper (analyserResults: AnalyserResult list) =
            analyserResults
            |> List.map (fun ar -> "" |>  ExecuteError.create (ar |> SemanticError))

        let preprocess = preprocessComments

        let parse str =
            let parser = Parser.Statement.pProgram

            match str |> run parser with
            | Success (program, _, _) -> program |> Result.Ok
            | Failure (errorMsg, _, _) ->
                errorMsg
                |> ExecuteError.createResult ErrorType.ParseError

        let optimize program =
            let expOptimizer =
                ExpSimplifier.simplifyNode
                |> ExpSimplifier.simplify

            let programOptimizer =
                StmtSimplifier.simplifyProgram expOptimizer

            program |> programOptimizer |> Ok

        let analyser =
            SemanticAnalyser.analyseProgram

        let analyse environment program =
            let res = analyser environment program

            if res |> List.isEmpty then
                program |> Ok
            else
                res |> semanticErrorsMapper |> List.head |> Error

        programString
        |> preprocess
        |> parse
        |> Result.bind (createEnvironment () |> analyse)
        |> Result.bind optimize
        |> Result.bind (createEnvironment () |> Interpreter.run)
