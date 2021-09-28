namespace Interpreter.AST

type Value =
    | IntValue of int
    | FloatValue of float
    | BoolValue of bool
    | VoidValue of unit
    member this.ToBool() =
        match this with
        | IntValue v -> System.Convert.ToBoolean(v)
        | FloatValue v -> System.Convert.ToBoolean(v)
        | BoolValue v -> v
        | _ -> invalidArg "val" "cannot convert void to bool"

    member this.ToFloat() =
        match this with
        | IntValue v -> v |> float
        | FloatValue v -> v
        | BoolValue v -> System.Convert.ToSingle(v) |> float
        | _ -> invalidArg "val" "cannot convert void to float"

module Value =
    let toFloat (v: Value) = v.ToFloat()
    let toBool (v: Value) = v.ToBool()

    let toStr =
        function
        | IntValue i -> sprintf "%d" i
        | FloatValue f -> sprintf "%f" f
        | VoidValue _ -> "null"
        | BoolValue (b) -> sprintf "%b" b

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

    let private computeBool operatorFun (v1: Value) (v2: Value) =
        (operatorFun (v1.ToBool()) (v2.ToBool()))
        |> BoolValue |> Result.Ok

    let (+) v1 v2 = compute (+) (+) v1 v2
    let (-) v1 v2 = compute (-) (-) v1 v2
    let (*) v1 v2 = compute (*) (*) v1 v2
    let (/) v1 v2 = compute (/) (/) v1 v2

    let (.&&) v1 v2 = computeBool (&&) v1 v2
    let (.||) v1 v2 = computeBool (||) v1 v2
    let (.==) v1 v2 = computeBool (=) v1 v2
    let (.!=) v1 v2 = computeBool (<>) v1 v2

    let Void = () |> VoidValue
    let createVoid () = Void
