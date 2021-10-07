// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Interpreter.AST
open Interpreter.Parser
open FParsec.CharParsers
open FSharpPlus


let test p str =
    match run p str with
    | Success (result, _, _) -> printfn "Success: %A" result
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg
let interpret p str =
    match run p str with
    | Success (result, _, _) -> Interpreter.run result
    | Failure (errorMsg, _, _) -> 
        printfn "Failure: %s" errorMsg
        Errors.createResult ErrorType.Other "error parsing"

[<EntryPoint>]
let main argv =

    let parser = Statement.pProgram
  
    let str = "fun foo(x) 
    {
        println(x);              
    } 
    foo(false);"
      
    match (interpret parser str) with 
        | Ok r -> printfn "%A" r
        | Error e -> printfn " %A" e

    0 // return an integer exit code
