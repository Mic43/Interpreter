namespace Interpreter.AST

type Identifier =
    private
    | Identifier of string
    member this.ToStr() =
        match this with
        | Identifier s -> s

module Identifier =
    let tryCreate s = Identifier s
    let create s = (tryCreate s)

    let toStr (identifier: Identifier) = identifier.ToStr()
