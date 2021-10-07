// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Interpreter.AST
open Interpreter.Parser
open FParsec.CharParsers


let test p str =
    match run p str with
    | Success (result, _, _) -> printfn "Success: %A" result
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main argv =
    // let ident = ("print" |> Identifier.create)

    // let program =
    //     [ (Expression.funCall ident [ Expression.intConstant 5 ])
    //       (Expression.funCall ("readInt" |> Identifier.create) [])
    //       (Expression.funCall ident [ Expression.var "aa" ])
    //       (Expression.funCall ident [ Expression.intConstant 1 ])
    //       ]
    //     |> List.map Statement.FromExpression
    //     |> Program

    let parser = Statement.pProgram
    //Statement.pScopedStatement()

    test parser "fun aa (){var z= c(22,false);} {}{}1.55;k; fun c ( x , y ) {true;} "
    // Runner.run program |> printf "%A"
    0 // return an integer exit code
