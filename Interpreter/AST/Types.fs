namespace Interpreter.AST

open System
open FSharpPlus

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
//let (>) v1 v2 = computeBool (>) (>) v1 v2


type Identifier = private Identifier of string

module Identifier =
    let tryCreateIdentifier s = Identifier s
    let createIdentifier s = (tryCreateIdentifier s)

    let toStr =
        function
        | Identifier s -> s

type UnaryOp =
    | Negate
    | Minus

type BinaryArithmeticOp =
    | Add
    | Sub
    | Mul
    | Div

type BinaryLogicalOp =
    | And
    | Or

type BinaryRelationalOp =
    | Greater
    | Less
    | Equal

type BinaryOp =
    | ArithmeticOp of BinaryArithmeticOp
    | RelationalOp of BinaryRelationalOp
    | LogicalOp of BinaryLogicalOp

type Expression =
    | Binary of
        {| BinaryOp: BinaryOp
           LeftOperand: Expression
           RightOperand: Expression |}
    | Unary of UnaryOp * Expression
    | Assignment of Identifier * Expression
    | FunCall of FunCall
    | Var of Identifier
    | Constant of Value

and FunCall =
    { Name: Identifier
      ActualParameters: Expression list }

type VarDeclaration =
    { Name: Identifier
      Initializer: Expression }

type Function =
    { Name: Identifier
      Parameters: Identifier list
      Body: Block }

and Block = ScopedStatement list

and ScopedStatement =
    | ExpressionStatement of Expression
    | VarDeclaration of VarDeclaration
    | PrintStatement of Expression
    | Block of Block
    | Empty

type Statement =
    | FunDeclaration of Function
    | ScopedStatement of ScopedStatement   

type Program = Program of Statement list
    
