namespace Interpreter.AST

open System
open FSharpPlus

type UnaryOp =
    | Negate
    | Minus

type IncrementOp =
    | Pre
    | Post

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
    | SimpleUnary of UnaryOp * Expression
    | Increment of IncrementOp * Expression
    | Assignment of Expression * Expression
    | FunCall of FunCall
    | Mutable of MutableExpression
    | Constant of Value

and FunCall =
    { Name: Identifier
      ActualParameters: Expression list }

module Expression =
    let var identifier =
        identifier |> Identifier.create |> Var |> Mutable

    let funCall identifier parameters =
        { Name = identifier
          ActualParameters = parameters }
        |> FunCall

    let floatConstant value = value |> FloatValue |> Constant
    let intConstant value = value |> IntValue |> Constant
    let voidConstant () = Value.Void |> Constant

    let assignment (identifier: Identifier) exp =
        (identifier |> Var |> Mutable, exp) |> Assignment

    let binary operator leftOperand rightOperand =
        {| BinaryOp = operator
           LeftOperand = leftOperand
           RightOperand = rightOperand |}
        |> Binary

    let add leftOperand rightOperand =
        binary (Add |> ArithmeticOp) leftOperand rightOperand

    let sub leftOperand rightOperand =
        binary (Sub |> ArithmeticOp) leftOperand rightOperand

    let mul leftOperand rightOperand =
        binary (Mul |> ArithmeticOp) leftOperand rightOperand

    let div leftOperand rightOperand =
        binary (Div |> ArithmeticOp) leftOperand rightOperand

    let equals leftOperand rightOperand =
        binary (Equal |> RelationalOp) leftOperand rightOperand

    let or_ leftOperand rightOperand =
        binary (Or |> LogicalOp) leftOperand rightOperand

    let and_ leftOperand rightOperand =
        binary (And |> LogicalOp) leftOperand rightOperand

    let less leftOperand rightOperand =
        binary (Less |> RelationalOp) leftOperand rightOperand

    let greater leftOperand rightOperand =
        binary (Greater |> RelationalOp) leftOperand rightOperand

    let neg operand = (Negate, operand) |> SimpleUnary

    let unaryMinus operand = (Minus, operand) |> SimpleUnary

    let preIncr operand = (Pre, operand) |> Increment

    let postIncr operand = (Post, operand) |> Increment
