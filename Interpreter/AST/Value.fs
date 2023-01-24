namespace Interpreter.AST

open FSharpPlus

type StructValue =
    { TypeName: Identifier
      Fields: Map<Identifier, Ref<Value>> }

and Value =
    | IntValue of int
    | FloatValue of float
    | BoolValue of bool
    | StringValue of string
    | VoidValue of unit
    | ListValue of Ref<Value> list
    | StructValue of StructValue


    member this.IsZero() =
        match this with
        | IntValue 0 -> true
        | FloatValue 0.0 -> true
        | BoolValue false -> true
        | StringValue "" -> true
        | ListValue [] -> true
        | _ -> false

    member this.ToBool() =
        match this with
        | IntValue v -> System.Convert.ToBoolean(v)
        | FloatValue v -> System.Convert.ToBoolean(v)
        | BoolValue v -> v
        | _ -> invalidArg "val" "cannot convert this type to bool"

    override this.ToString() =
        match this with
        | IntValue i -> sprintf "%d" i
        | FloatValue f -> sprintf "%f" f
        | VoidValue _ -> "null"
        | BoolValue b -> sprintf "%b" b
        | StringValue s -> s
        | ListValue lv ->
            if lv.Length <> 0 then
                lv
                |> Seq.map (fun l -> l.ToString())
                |> Seq.reduce (fun a b -> sprintf "%s, %s" a b)
                |> sprintf "[%s]"
            else
                ""
        | StructValue sv -> $"%A{sv.TypeName} %A{sv.Fields}"

    member this.ToFloat() =
        match this with
        | IntValue v -> v |> float
        | FloatValue v -> v
        | BoolValue v -> System.Convert.ToSingle(v) |> float
        | v -> invalidArg "val" (sprintf "cannot convert %A to float" v)
    
    member this.ToList() =
        match this with
        | ListValue v -> v
        | VoidValue _ -> invalidArg "val" "cannot convert void to list"
        | v -> ref v |> List.singleton

type EvaluationStopped =
    | RuntimeError of RuntimeError
    | ReturnStmtReached of Value

module Value =
    let Void = () |> VoidValue    
    let createVoid () = Void

    let createStructInstance fields strName =
        { TypeName = strName |> Identifier.create
          Fields = fields }
        |> StructValue

    let createDefaultStructInstance fieldNames =
        createStructInstance (
            fieldNames
            |> List.map (fun n -> (n |> Identifier.create, ref Void))
            |> Map.ofList
        )

    let createEmptyStructInstance = createStructInstance Map.empty

    let ignoreResuls (res: Result<Value list, ExecuteError>) = res |> Result.map (fun _ -> Void)

    let getLastResultOrVoid (res) =
        res
        |> Result.map (fun vl -> vl |> (List.tryLast >> (Option.defaultValue Void)))

    /// Changes returnstmtreached error result to actual Ok returned value
    let getReturnedValue (res: Result<Value, EvaluationStopped>) =

        res
        |> Result.bindError (fun e ->
            match e with
            | ReturnStmtReached value -> value |> Ok
            | RuntimeError e -> e |> RuntimeError |> Error)

    let toFloat (v: Value) = v.ToFloat()
    let toBool (v: Value) = v.ToBool()
    let toStr (v: Value) = v.ToString()

    let private compute
        resultConvFloat
        resultConvInt
        resultConvString
        resultConvList
        opFloat
        opInt
        opString
        opList
        v1
        v2
        =
        match (v1, v2) with
        | IntValue v1, IntValue v2 -> (opInt v1 v2) |> resultConvInt |> Result.Ok
        | ListValue _, _
        | _, ListValue _ ->
            monad' {
                let! opList = opList
                let! resultConvList = resultConvList

                return
                    opList (v1.ToList()) (v2.ToList())
                    |> resultConvList
            }
            |> Option.toResultWith (
                "Operation is not supported for list types"
                |> ExecuteError.createRuntimeError
            )
        | StringValue _, _
        | _, StringValue _ ->
            monad' {
                let! opString = opString
                let! resultConvString = resultConvString

                return opString (toStr v1) (toStr v2) |> resultConvString
            }
            |> Option.toResultWith (
                "Operation is not supported for string types"
                |> ExecuteError.createRuntimeError
            )

        | VoidValue _, _
        | _, VoidValue _ ->
            "cannot convert void to float"
            |> ExecuteError.createRuntimeErrorResult
        | _, _ ->
            opFloat (v1 |> toFloat) (v2 |> toFloat)
            |> resultConvFloat
            |> Ok

    let private computeArithmetic opFloat opInt opString opList v1 v2 =
        compute FloatValue IntValue (Some StringValue) (Some ListValue) opFloat opInt opString opList v1 v2

    let private computeRelational opFloat opInt opString opList v1 v2 =
        compute BoolValue BoolValue (Some BoolValue) (Some BoolValue) opFloat opInt opString opList v1 v2

    let private computeLogical operatorFun (v1: Value) (v2: Value) =
        operatorFun (v1.ToBool()) (v2.ToBool())
        |> BoolValue
        |> Result.Ok

    let (+) v1 v2 =
        computeArithmetic (+) (+) (Some(+)) (Some List.append) v1 v2

    let (-) v1 v2 =
        computeArithmetic (-) (-) None None v1 v2

    let (*) v1 v2 =
        computeArithmetic (*) (*) None None v1 v2

    let (/) v1 v2 =
        computeArithmetic (/) (/) None None v1 v2

    let (.&&) v1 v2 = computeLogical (&&) v1 v2
    let (.||) v1 v2 = computeLogical (||) v1 v2

    let (~-) (v: Value) = (-1 |> IntValue) * v

    let (!) (v: Value) =
        not (v.ToBool()) |> BoolValue |> Result.Ok

    let (.==) v1 v2 =
        computeRelational (=) (=) (Some(=)) (Some(=)) v1 v2

    let (.!=) v1 v2 = (v1 .== v2) |> Result.map (!)

    let (.>) v1 v2 =
        computeRelational (>) (>) None None v1 v2

    let (.<) v1 v2 =
        computeRelational (<) (<) None None v1 v2
