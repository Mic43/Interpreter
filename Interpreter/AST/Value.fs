namespace Interpreter.AST

type Value =
    | IntValue of int
    | FloatValue of float
    | VoidValue of unit

module Value =
    let toFloat =
        function
        | IntValue v -> v |> float
        | FloatValue v -> v
        | _ -> invalidArg "val" "cannot convert void to float"

    let toStr =
        function
        | IntValue i -> sprintf "%d" i
        | FloatValue f -> sprintf "%f" f
        | VoidValue _ -> "null"

    let private compute opFloat opInt v1 v2 =
        match (v1, v2) with
        | (IntValue v1, IntValue v2) -> (opInt v1 v2) |> IntValue |> Result.Ok
        | (VoidValue _, _)
        | (_, VoidValue _) ->
            "cannot convert void to float"
            |> (Errors.createResult Other)
        | (v1, v2) ->
            (opFloat (v1 |> toFloat) (v2 |> toFloat))
            |> FloatValue
            |> Result.Ok

    let private computeBool opFloat opInt v1 v2 : bool =
        match (v1, v2) with
        | (IntValue v1, IntValue v2) -> (opInt v1 v2)
        | (v1, v2) -> (opFloat (v1 |> toFloat) (v2 |> toFloat))

    let (+) v1 v2 = compute (+) (+) v1 v2
    let (-) v1 v2 = compute (-) (-) v1 v2
    let (*) v1 v2 = compute (*) (*) v1 v2
    let (/) v1 v2 = compute (/) (/) v1 v2

    let Void = () |> VoidValue
    let createVoid () = Void
