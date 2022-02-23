namespace Interpreter.AST

open System
open FSharpPlus

module DefaultEnvironment =
    let tryPrint (parametersList: Value list) =
        if (parametersList.Length <> 1) then
            "Wrong parameter count"
            |> (Errors.createResult ErrorType.Other)
        else
            parametersList.[0].ToString() |> printf "%s"
            Value.Void |> Ok

    let tryPrintLn (parametersList: Value list) =
        monad' {
            let! v = (tryPrint parametersList)
            printfn ""
            return v
        }

    let tryReadInt (parametersList: Value list) =
        if not parametersList.IsEmpty then
            "Wrong parameter count"
            |> (Errors.createResult ErrorType.Other)
        else
            match System.Int32.TryParse(Console.ReadLine()) with
            | true, int -> int |> IntValue |> Result.Ok
            | _ -> Errors.createResult ErrorType.Other "error parsing int"

    let tryGetLen (parametersList: Value list) =
        match parametersList with
        | [ StringValue v ] -> v.Length |> IntValue |> Result.Ok
        | [ ListValue v ] -> v.Length |> IntValue |> Result.Ok
        | [ _ ] -> Errors.createResult ErrorType.Other "error getting len of the variable"
        | _ ->
            "Wrong parameter count"
            |> (Errors.createResult ErrorType.Other)

// let tryReadLine (parametersList: Value list) =
//     if not parametersList.IsEmpty then
//          "Wrong parameter count"
//        |> (Errors.createResult Other)
//     else
//         let v: int option = Console.ReadLine() |> tryParse

//         let line = Console.ReadLine()
//         match line with
//             line when
//                 (let (res,v) = (System.Int32.TryParse line
//                 res) -> v |> IntValue |> Result.Ok
