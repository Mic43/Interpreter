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
        let defaultSemanticErrorsMapper (analyserResults: AnalyserResult list) =
            analyserResults
            |> List.map (fun ar -> "" |>  ExecuteError.create (ar |> SemanticError))

        let withPositionSemanticErrorsMapper (analyserResults: (AnalyserResult*FParsec.Position) list) =
            analyserResults
            |> List.map (fun (ar,pos) -> $"Line: {pos.Line}" |>  ExecuteError.create (ar |> SemanticError))
        
        let preprocess = preprocessComments

        let parse parser str =
            
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
        let noOptimize program = program
        // let analyser =
        //     SemanticAnalyser.analyseProgram
        let analyser =
            SemanticAnalyser.analyseProgramTest

        let analyse semanticErrorsMapper environment program =
            let res = analyser environment program

            if res |> List.isEmpty then
                program |> Ok
            else
                res |> semanticErrorsMapper |> List.head |> Error
                
        let analyseTest semanticErrorsMapper environment program =
            let res = analyser environment program

            if res |> List.isEmpty then
                program |> Ok
            else
                res |> withPositionSemanticErrorsMapper |> List.head |> Error

        programString
        |> preprocess
        |> parse Parser.Statement.pProgramTest
        |> Result.bind (createEnvironment () |> (analyseTest withPositionSemanticErrorsMapper))
        |> Result.map ((fun l -> l |> List.map (fun (stmt,_) -> stmt) ) >> Program.Of) 
        |> Result.bind optimize
        |> Result.bind (createEnvironment () |> Interpreter.run)
