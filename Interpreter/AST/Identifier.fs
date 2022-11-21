namespace Interpreter.AST

open FSharpPlus

type Identifier =
    private
    | Identifier of string
    member this.Get() =
        match this with
        | Identifier s -> s

module Identifier =
    let tryCreate (s: string) =
        if s.Length > 0 then
            Identifier s |> Some
        else
            None

    let create s =
        match (tryCreate s) with
        | Some v -> v
        | None -> "s" |> invalidArg "Not valid identifier"

    let asString (identifier: Identifier) = identifier.Get()
