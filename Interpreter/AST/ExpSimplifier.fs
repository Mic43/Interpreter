namespace Interpreter.AST

open FSharpPlus
open System.Collections.Generic
open System.Linq


module ExpSimplifier =
    let private simplifyUnary =
        function
        | (Negate, Constant (BoolValue true)) -> (BoolValue false) |> Constant
        | (Negate, Constant (BoolValue false)) -> (BoolValue true) |> Constant
        | (op, exp) -> (op, exp) |> SimpleUnary

    let private simplifyBinary =
        function
        | { BinaryOp = ArithmeticOp Add
            LeftOperand = Constant v
            RightOperand = right } when v.IsZero() -> right
        | { BinaryOp = ArithmeticOp Add
            RightOperand = Constant v
            LeftOperand = left } when v.IsZero() -> left
        | { BinaryOp = ArithmeticOp Sub
            RightOperand = Constant v
            LeftOperand = left } when v.IsZero() -> left
        | b -> b |> Binary

    let simplifyNode =
        function
        | Binary b -> simplifyBinary b
        | SimpleUnary (op, e) -> simplifyUnary (op, e)
        | e -> e

    let rec simplify (simplifyNode: Expression -> Expression) exp =
        let simplify = simplify simplifyNode

        (match exp with
         | Binary ({ LeftOperand = left
                     RightOperand = right } as b) ->

             { b with
                 LeftOperand = left |> simplify
                 RightOperand = right |> simplify }
             |> Binary
         | SimpleUnary (op, operand) -> (op, operand |> simplify) |> SimpleUnary
         | Increment (op, exp) -> (op, exp |> simplify) |> Increment
         | Assignment (lValExp, exp) -> (lValExp, exp |> simplify) |> Assignment
         | ListCreation expList -> expList |> List.map simplify |> ListCreation
         | UserTypeCreation ({ FieldsInitialization = fieldsInit } as utc) ->
             { utc with FieldsInitialization = fieldsInit |> Map.mapValues simplify }
             |> UserTypeCreation
         | FunCall ({ ActualParameters = actualParamsExps } as funcCall) ->
             { funcCall with ActualParameters = actualParamsExps |> List.map simplify }
             |> FunCall
         | Mutable (IndexedVar (mexp, exp)) -> (mexp, exp |> simplify) |> IndexedVar |> Mutable
         | Mutable _ -> exp
         | Constant _ -> exp)
        |> simplifyNode
