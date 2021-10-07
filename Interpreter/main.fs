namespace Interpreter

open Interpreter.AST

module test =
    [<EntryPoint>]
    let main arguments =
        let ident = ("print" |> Identifier.create)

        let program =
            [ (Expression.funCall ident [ Expression.intConstant 5 ]) ]
            |> List.map Statement.FromExpression
            |> Program

        Runner.run program
        0

// 2579
