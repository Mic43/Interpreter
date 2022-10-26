﻿namespace Interpreter.AST

open FSharpPlus
open System.Collections.Generic
open System.Linq


module ExpSimplifier =

    /// matches (a*b) + (c*d) binary expression
    let private (|MulDistributivity|_|) (exp: BinaryExpression) =

        match exp with
        | { LeftOperand = Binary ({ LeftOperand = a
                                    RightOperand = b
                                    BinaryOp = ArithmeticOp Mul })
            RightOperand = Binary ({ LeftOperand = c
                                     RightOperand = d
                                     BinaryOp = ArithmeticOp Mul })
            BinaryOp = ArithmeticOp Add } -> Some((a, b, c, d))
        | _ -> None

    let rec simplifyNode exp =
        let simplifyUnary =
            function
            | (op, Constant v) as exp ->
                ExpEvaluator.unaryOpEvaluator op v
                |> Result.map Constant
                |> Result.defaultValue (exp |> SimpleUnary)
            | (op, exp) -> (op, exp) |> SimpleUnary

        let simplifyConstantBinary exp v1 v2 =
            ExpEvaluator.binaryOpEvaluator exp.BinaryOp v1 v2
            |> Result.map Constant
            |> Result.defaultValue (exp |> Binary)

        let simplifyMul =
            function
            | { LeftOperand = Constant v }
            | { RightOperand = Constant v } when v.IsZero() -> 0 |> Expression.intConstant
            | { LeftOperand = Constant v
                RightOperand = other }
            | { RightOperand = Constant v
                LeftOperand = other } when
                v
                |> Result.protect (fun v -> v.ToFloat())
                |> Result.contains 1.0
                ->
                other
            | b -> b |> Binary

        let simplifyAdd exp =
            let simplifiedDistributivity a b c =
                (a,
                 (b, c)
                 ||> Expression.binary (ArithmeticOp Add)
                 |> simplifyNode)

                ||> Expression.binary (ArithmeticOp Mul)
                |> simplifyNode

            match exp with
            | { LeftOperand = Constant v
                RightOperand = right } when v.IsZero() -> right
            | { RightOperand = Constant v
                LeftOperand = left } when v.IsZero() -> left
            //| { LeftOperand = Binary ({ LeftOperand = a
            //                            RightOperand = b
            //                            BinaryOp = ArithmeticOp Mul })
            //    RightOperand = Binary ({ LeftOperand = a2
            //                             RightOperand = c
            //                             BinaryOp = ArithmeticOp Mul }) }
            //| { LeftOperand = Binary ({ LeftOperand = b
            //                            RightOperand = a
            //                            BinaryOp = ArithmeticOp Mul })
            //    RightOperand = Binary ({ LeftOperand = a2
            //                             RightOperand = c
            //                             BinaryOp = ArithmeticOp Mul }) }
            //| { LeftOperand = Binary ({ LeftOperand = a
            //                            RightOperand = b
            //                            BinaryOp = ArithmeticOp Mul })
            //    RightOperand = Binary ({ LeftOperand = c
            //                             RightOperand = a2
            //                             BinaryOp = ArithmeticOp Mul }) }
            //| { LeftOperand = Binary ({ LeftOperand = b
            //                            RightOperand = a
            //                            BinaryOp = ArithmeticOp Mul })
            //    RightOperand = Binary ({ LeftOperand = c
            //                             RightOperand = a2
            //                             BinaryOp = ArithmeticOp Mul }) } when

            //    a = a2
            //    ->

            //    (a,
            //     (b, c)
            //     ||> Expression.binary (ArithmeticOp Add)
            //     |> simplifyNode)

            //    ||> Expression.binary (ArithmeticOp Mul)
            //    |> simplifyNode
            //(a*b) + (c*d)                                      _*(_+_)
            | MulDistributivity (a, b, c, d) when a = c -> simplifiedDistributivity a b d
            | MulDistributivity (a, b, c, d) when a = d -> simplifiedDistributivity a b c
            | MulDistributivity (a, b, c, d) when b = c -> simplifiedDistributivity b a d
            | MulDistributivity (a, b, c, d) when b = d -> simplifiedDistributivity b a c

            | b -> b |> Binary

        let simplifySub =
            function
            | { RightOperand = Constant v
                LeftOperand = left } when v.IsZero() -> left
            | { RightOperand = right
                LeftOperand = left } when left = right -> 0 |> Expression.intConstant
            | b -> b |> Binary

        let simplifyBinary =
            function
            | { LeftOperand = Constant v1
                RightOperand = Constant v2 } as exp -> simplifyConstantBinary exp v1 v2
            | { BinaryOp = ArithmeticOp Add } as exp -> simplifyAdd exp
            | { BinaryOp = ArithmeticOp Sub } as exp -> simplifySub exp
            | { BinaryOp = ArithmeticOp Mul } as exp -> simplifyMul exp
            | b -> b |> Binary

        match exp with
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
