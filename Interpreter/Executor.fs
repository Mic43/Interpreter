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

    let private buildAnalyser () =
        let analyser =
            SemanticAnalyser.analyseStatementsWithInfo

        let emptyMessageBuilder (errorType:ErrorType) = ""

        let analyse (errorMessageBuilder: ErrorType -> string) environment program =
            let res = analyser environment program

            if res |> List.isEmpty then
                program |> Ok
            else
                res
                |> List.map SemanticError
                |> List.map (fun e -> e |> emptyMessageBuilder |> ExecuteError.create e)
                |> Result.Error

        analyse emptyMessageBuilder

    let run programString =

        let preprocess = preprocessComments

        let parse parser str =
            match str |> run parser with
            | Success (program, _, _) -> program |> Result.Ok
            | Failure (errorMsg, _, _) ->
                errorMsg
                |> ExecuteError.createResult ErrorType.ParseError
                |> Result.mapError List.singleton

        let optimize program =
            let expOptimizer =
                ExpSimplifier.simplifyNode
                |> ExpSimplifier.simplify

            let programOptimizer =
                StmtSimplifier.simplifyProgram expOptimizer

            program |> programOptimizer |> Ok

        let analyse = buildAnalyser ()

        let extractStatements statementWithInfos =
            statementWithInfos
            |> List.map (fun statementWithInfo -> statementWithInfo.Statement)
            |> Program.Of

        programString
        |> preprocess
        |> parse Parser.Statement.pStatementsWithInfo
        |> Result.bind (createEnvironment () |> analyse)
        |> Result.map extractStatements
        |> Result.bind optimize
        |> Result.bind (createEnvironment () |> Interpreter.run)
