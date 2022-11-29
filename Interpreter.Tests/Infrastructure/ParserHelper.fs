module Interpreter.Tests.Infrastructure.ParserHelper


open FParsec.CharParsers
open Interpreter.AST

let isErrorSpecific eType (result:Result<Value,ExecuteError>)  =
    match (result |> Result.mapError (fun e -> e.Type = eType)) with
    | Ok _-> false
    | Error v -> v

let map (f:'a->'b) pResult =
    match pResult with
    | Success (r, u, o) -> (r |> f, u, o) |> Success
    | Failure(s, parserError, a)  -> (s, parserError, a) |> Failure

let toOption pResult =
    match pResult with
    | Success (r, _, _) -> r |> Some
    | Failure _ -> None