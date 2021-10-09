namespace Interpreter.AST

open System
open FSharpPlus

module DefaultEnvironment =
    let tryPrint (parametersList: Value list) =
        if (parametersList.Length <> 1) then
            "Wrong parameter count"
            |> (Errors.createResult ErrorType.Other)
        else
            match parametersList.[0] with
            | IntValue iv ->
                printf "%i" iv
                Value.Void |> Ok
            | FloatValue fv ->
                printf "%f" fv
                Value.Void |> Ok
            | VoidValue _ ->
                "Cannot print void value"
                |> (Errors.createResult Other)
            | BoolValue (b) ->
                printf "%b" b
                Value.Void |> Ok

    let tryPrintLn (parametersList: Value list) =
        monad' {
            let! v =  (tryPrint parametersList)
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
                     
