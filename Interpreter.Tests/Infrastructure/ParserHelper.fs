module Interpreter.Tests.Infrastructure.ParserHelper


open FParsec.CharParsers
open Interpreter.AST

let isErrorSpecific (ar:AnalyserResult) (result:Result<Value,ExecuteError list>)  =
    match (result) with // |> Result.mapError (fun e -> (e |> List.exactlyOne).Type = eType)) with  
    | Error [{Type = SemanticError(analyserResult, _)}] -> analyserResult = ar
    | _ -> false
let map (f:'a->'b) pResult =
    match pResult with
    | Success (r, u, o) -> (r |> f, u, o) |> Success
    | Failure(s, parserError, a)  -> (s, parserError, a) |> Failure

let toOption pResult =
    match pResult with
    | Success (r, _, _) -> r |> Some
    | Failure _ -> None