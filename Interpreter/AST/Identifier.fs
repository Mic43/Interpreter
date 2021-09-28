namespace Interpreter.AST

type Identifier =
    private
    | Identifier of string
    member this.ToStr() =
        match this with
        | Identifier s -> s

module Identifier =
    let tryCreateIdentifier s = Identifier s
    let createIdentifier s = (tryCreateIdentifier s)

    let toStr (identifier: Identifier) = identifier.ToStr()
