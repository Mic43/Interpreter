namespace Interpreter

open System
open Interpreter.AST
open Interpreter.Parser
open FParsec.CharParsers
open FSharpPlus


type RunnerError =
    | ExecuteError of ExecuteError
    | ParseError of string

module Runner =
    let private preprocessComments programStr =
        let comment = "//"

        programStr
        |> String.split [ "\n" ]
        |> Seq.map (fun line -> line |> String.split [ comment ] |> Seq.head)
        |> Seq.reduce (fun acc s -> sprintf "%s\n%s" acc s)

    let private dummyPreprocess programStr = programStr

    let run programString =
        let preprocess = preprocessComments

        let runParser str =
            let parser = Parser.Statement.pProgram

            match str |> run parser with
            | Success (program, _, _) -> program |> Result.Ok
            | Failure (errorMsg, _, _) -> errorMsg |> ParseError |> Error

        let optimize program =
            let expOptimizer =
                ExpSimplifier.simplifyNode
                |> ExpSimplifier.simplify

            let programOptimizer =
                StmtSimplifier.simplifyProgram expOptimizer

            program |> programOptimizer |> Ok

        programString
        |> preprocess
        |> runParser
        |> Result.bind optimize
        |> Result.bind (Interpreter.run >> Result.mapError ExecuteError)
