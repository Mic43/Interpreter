namespace Interpreter

open System
open Interpreter.AST
open Interpreter.Parser
open FParsec.CharParsers
open FSharpPlus


module Runner =
    let private preprocessComments programStr =
        let comment = "//"

        programStr
        |> String.split [ "\n" ]
        |> Seq.map (fun line -> line |> String.split [ comment ] |> Seq.head)
        |> Seq.reduce (fun acc s -> sprintf "%s\n%s" acc s)

    let private dummyPreprocess programStr = programStr

    let run programString =
        let preprocessor = preprocessComments
        let parser = Parser.Statement.pProgram

        match programString |> preprocessor |> run parser with
        | Success (program, _, _) ->
            match Interpreter.run program with
            | Ok r -> r |> ignore
            | Error (errorValue) -> printfn "Failure: %A" errorValue
        | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg
