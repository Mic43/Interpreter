namespace Interpreter.AST

open FSharpPlus

type Value =
    | IntValue of int
    | FloatValue of float
    | BoolValue of bool
    | StringValue of string
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
        | v -> invalidArg "val" (sprintf "cannot convert %A to float" v)

module Value =
    let Void = () |> VoidValue
    let createVoid () = Void

    let ignoreResuls (res: Result<Value list, RunError>) = res |> Result.map (fun _ -> Void)

    let getLastResultOrVoid (res: Result<Value list, RunError>) =
        res
        |> Result.map (fun vl -> vl |> (List.tryLast >> (Option.defaultValue Void)))

    let toFloat (v: Value) = v.ToFloat()
    let toBool (v: Value) = v.ToBool()

    let toStr =
        function
        | IntValue i -> sprintf "%d" i
        | FloatValue f -> sprintf "%f" f
        | VoidValue _ -> "null"
        | BoolValue (b) -> sprintf "%b" b
        | StringValue (s) -> s

    let private compute resultConvFloat resultConvInt resultConvString opFloat opInt opString v1 v2 =
        match (v1, v2) with
        | (IntValue v1, IntValue v2) -> (opInt v1 v2) |> resultConvInt |> Result.Ok
        | (StringValue s1, v2) ->
            monad' {
                let! opString = opString
                let! resultConvString = resultConvString
                return (opString s1 (toStr v2)) |> resultConvString
            }
            |> Option.toResultWith (
                "Operation is not supported for string types"
                |> (Errors.create Other)
            )
        | (v1, StringValue s2) ->
            monad' {
                let! opString = opString
                let! resultConvString = resultConvString
                return (opString (toStr v1) s2) |> resultConvString
            }
            |> Option.toResultWith (
                "Operation is not supported for string types"
                |> (Errors.create Other)
            )
        | (VoidValue _, _)
        | (_, VoidValue _) ->
            "cannot convert void to float"
            |> (Errors.createResult Other)
        | (v1, v2) ->
            (opFloat (v1 |> toFloat) (v2 |> toFloat))
            |> resultConvFloat
            |> Ok

    let private computeArithmetic opFloat opInt opString v1 v2 =
        compute FloatValue IntValue (Some StringValue) opFloat opInt opString v1 v2

    let private computeRelational opFloat opInt opString v1 v2 =
        compute BoolValue BoolValue (Some BoolValue) opFloat opInt opString v1 v2

    let private computeLogical operatorFun (v1: Value) (v2: Value) =
        (operatorFun (v1.ToBool()) (v2.ToBool()))
        |> BoolValue
        |> Result.Ok

    let (+) v1 v2 =
        computeArithmetic (+) (+) (Some(+)) v1 v2

    let (-) v1 v2 = computeArithmetic (-) (-) None v1 v2
    let (*) v1 v2 = computeArithmetic (*) (*) None v1 v2
    let (/) v1 v2 = computeArithmetic (/) (/) None v1 v2

    let (.&&) v1 v2 = computeLogical (&&) v1 v2
    let (.||) v1 v2 = computeLogical (||) v1 v2

    let (~-) (v: Value) = ((-1) |> IntValue) * v

    let (!) (v: Value) =
        not (v.ToBool()) |> BoolValue |> Result.Ok

    let (.==) v1 v2 =
        computeRelational (=) (=) (Some(=)) v1 v2

    let (.!=) v1 v2 = (v1 .== v2) |> Result.map (!)
    let (.>) v1 v2 = computeRelational (>) (>) None v1 v2
    let (.<) v1 v2 = computeRelational (<) (<) None v1 v2
