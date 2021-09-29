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

    let private compute resultConvFloat resultConvInt opFloat opInt v1 v2 =
        match (v1, v2) with
        | (IntValue v1, IntValue v2) -> (opInt v1 v2) |> resultConvInt |> Result.Ok
        | (VoidValue _, _)
        | (_, VoidValue _) ->
            "cannot convert void to float"
            |> (Errors.createResult Other)
        | (v1, v2) ->
            (opFloat (v1 |> toFloat) (v2 |> toFloat))
            |> resultConvFloat
            |> Result.Ok

    let private computeArithmetic opFloat opInt v1 v2 =
        compute FloatValue IntValue opFloat opInt v1 v2

    let private computeRelational opFloat opInt v1 v2 =
        compute BoolValue BoolValue opFloat opInt v1 v2

    let private computeLogical operatorFun (v1: Value) (v2: Value) =
        (operatorFun (v1.ToBool()) (v2.ToBool()))
        |> BoolValue
        |> Result.Ok

    let (+) v1 v2 = computeArithmetic (+) (+) v1 v2
    let (-) v1 v2 = computeArithmetic (-) (-) v1 v2
    let (*) v1 v2 = computeArithmetic (*) (*) v1 v2
    let (/) v1 v2 = computeArithmetic (/) (/) v1 v2

    let (.&&) v1 v2 = computeLogical (&&) v1 v2
    let (.||) v1 v2 = computeLogical (||) v1 v2

    let (~-) (v: Value) = ((-1) |> IntValue) * v
    let (!) (v: Value) = not (v.ToBool()) |> BoolValue |> Result.Ok

    let (.==) v1 v2 = computeRelational (=) (=) v1 v2    
    let (.!=) v1 v2 = (v1 .== v2) |> Result.map (!)
    let (.>) v1 v2 = computeRelational (>) (>) v1 v2
    let (.<) v1 v2 = computeRelational (<) (<) v1 v2

    let Void = () |> VoidValue
    let createVoid () = Void
