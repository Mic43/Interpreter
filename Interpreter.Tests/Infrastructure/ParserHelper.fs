module Interpreter.Tests.Infrastructure.ParserHelper


open FParsec.CharParsers

let map (f:'a->'b) pResult =
    match pResult with
    | Success (r, u, o) -> (r |> f, u, o) |> Success
    | Failure(s, parserError, a)  -> (s, parserError, a) |> Failure

let toOption pResult =
    match pResult with
    | Success (r, _, _) -> r |> Some
    | Failure _ -> None