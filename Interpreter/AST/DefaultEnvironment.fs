namespace Interpreter.AST

open System


module DefaultEnvironment =
    let tryPrint (parametersList: Value list) =
        if (parametersList.Length <> 1) then
            "Wrong parameter count"
            |> (Errors.createResult ErrorType.Other)
        else
            match parametersList.[0] with
            | IntValue iv ->
                printf "%i" iv
                Value.Void |> Result.Ok
            | FloatValue fv ->
                printf "%f" fv
                Value.Void |> Result.Ok
            | VoidValue _ ->
                "Cannot print void value"
                |> (Errors.createResult Other)

    let readInt (parametersList: Value list) =
        if not parametersList.IsEmpty then
            "Wrong parameter count"
            |> (Errors.createResult ErrorType.Other)
        else
            match System.Int32.TryParse(Console.ReadLine()) with
            | true, int -> int |> IntValue |> Result.Ok
            | _ -> Errors.createResult ErrorType.Other "error parsing int"
