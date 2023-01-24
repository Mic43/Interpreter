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

        let analyserMessageBuilder (errorType: ErrorType) = ""

        let analyse (errorMessageBuilder: ErrorType -> string) environment program =
            let res = analyser environment program

            if res |> List.isEmpty then
                program |> Ok
            else
                seq {
                    for ar, pos in res do
                        let errorType = ar |> SemanticError

                        let message =
                            errorType |> analyserMessageBuilder

                        yield ExecuteError.create message errorType pos

                }
                |> Seq.toList
                |> Result.Error

        analyse analyserMessageBuilder

    let run programString =

        let preprocess = preprocessComments

        let parse parser str =
            match str |> run parser with
            | Success (program, _, _) -> program |> Result.Ok
            | Failure (errorMsg, error, _) ->
                ExecuteError.create
                    errorMsg
                    ErrorType.ParseError
                    { StatementPosition.Line = error.Position.Line
                      StatementPosition.Column = error.Position.Column }
                |> Result.Error
                |> Result.mapError List.singleton

        let optimise (program: StatementWithInfo list) =
            let expOptimiser =
                ExpOptimiser.simplifyNode |> ExpOptimiser.optimise

            let stmtOptimiser =
                StmtOptimiser.optimise expOptimiser

            seq {
                for si in program do
                    { si with Statement = si.Statement |> stmtOptimiser }
            }
            |> Seq.toList
            |> Ok

        let analyse = buildAnalyser ()

        let interpret environment program =
            Interpreter.run environment program
            |> Result.mapError (fun runtimeErrors ->
                runtimeErrors
                |> List.map (fun (re, position) -> ExecuteError.create re.Message ErrorType.RuntimeError position))

        programString
        |> preprocess
        |> parse Parser.Statement.pStatementsWithInfo
        |> Result.bind (createEnvironment () |> analyse)
        |> Result.bind optimise
        |> Result.bind (createEnvironment () |> interpret)
