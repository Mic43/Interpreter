namespace Interpreter.AST

open System
open FSharpPlus

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

type MutableExpression = Var of Identifier

type Expression =
    | Binary of
        {| BinaryOp: BinaryOp
           LeftOperand: Expression
           RightOperand: Expression |}
    | Unary of UnaryOp * Expression
    | Assignment of Identifier * Expression
    | FunCall of FunCall
    | Mutable of MutableExpression
    | Constant of Value

and FunCall =
    { Name: Identifier
      ActualParameters: Expression list }
