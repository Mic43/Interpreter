﻿namespace Interpreter.AST

open FSharpPlus

type ExpEvaluator = Expression -> Result<Value, RunError>

module ExpEvaluator =
    open Value

    let constEvaluator value = Result.Ok value

    let binaryOpEvaluator op (val1: Value) (val2: Value) =
        let evalArithmeticExp val1 val2 =
            function
            | (Add _) -> (val1 + val2)
            | (Sub _) -> (val1 - val2)
            | (Mul _) -> (val1 * val2)
            | (Div _) -> (val1 / val2)

        let evalArithmeticExp op =
            try
                (evalArithmeticExp val1 val2 op)
                |> Result.mapError EvalError
            with
            | _ ->
                "Arithmetic error"
                |> (Errors.createResult Other)
                |> Result.mapError EvalError

        let evalLogicalExp op =
            match op with
            | And -> val1 .&& val2
            | Or -> val1 .|| val2

        let evalRelationalExp op =
            (match op with
             | Equal -> val1 .== val2
             | Greater -> val1 .> val2
             | Less -> val1 .< val2)
            |> Result.mapError EvalError

        match op with
        | BinaryOp.ArithmeticOp op -> (evalArithmeticExp op)
        | BinaryOp.LogicalOp op -> (evalLogicalExp op)
        | BinaryOp.RelationalOp op -> (evalRelationalExp op)

    let unaryOpEvaluator op value =
        match op with
        | Negate -> !value
        | Minus -> -(value)
        |> Result.mapError EvalError

    let rec tryEvaluate
        (varUpdater: Identifier -> Value -> Result<unit, RunError>)
        (varEvaluator: Identifier -> Result<Value, RunError>)
        constEvaluator
        binOpEvaluator
        unaryOpEvaluator
        funEvaluator
        (expression: Expression)
        : Result<Value, EvalStopped> =

        let tryEvaluateRec =
            tryEvaluate varUpdater varEvaluator constEvaluator binOpEvaluator unaryOpEvaluator funEvaluator

        let incrementEvaluator op exp =
            (match exp with
             | Mutable mutableExpr ->
                 match mutableExpr with
                 | Var identifier ->
                     monad' {
                         let! beforeValue = (varEvaluator identifier)
                         let! newValue = beforeValue + (IntValue 1)
                         do! varUpdater identifier newValue

                         return
                             match op with
                             | Pre -> newValue
                             | Post -> beforeValue
                     }
             | _ ->
                 "Left operand of assignment expression must be LValue"
                 |> (Errors.createResult Other))
            |> Result.mapError EvalError

        let assignmentEvaluator mutableExpr expr =
            match mutableExpr with
            | Mutable mutableExpr ->
                match mutableExpr with
                | Var ident ->
                    monad' {
                        let! value = (tryEvaluateRec expr)

                        do!
                            (varUpdater ident value)
                            |> Result.mapError EvalError

                        return value
                    }
            | _ ->
                Errors.createResult Other "Left operand of assignment expression must be LValue"
                |> Result.mapError EvalError

        let mutableExpEvaluator mutableExpr : Result<Value,EvalStopped>=
            match mutableExpr with
            | Var v -> varEvaluator v |> Result.mapError EvalError
            | IndexedVar (ident, exp) ->
                monad' {
                    let! index = exp |> tryEvaluateRec

                    let! value =
                        (varEvaluator ident)
                        |> (Result.mapError EvalError)

                    return!
                        match (value, index) with
                        | (ListValue lv, IntValue index) -> lv.[index] |> Result.Ok
                        | (StringValue s, IntValue index) -> s.[index] |> string |> StringValue |> Result.Ok
                        | _ ->
                            (Errors.createResult Other "indexing expression must evaluate to int")
                            |> (Result.mapError EvalError)
                }

        match expression with
        | Constant c -> constEvaluator c
        | Mutable m -> mutableExpEvaluator m
        | Assignment (mutableExpr, expr) -> assignmentEvaluator mutableExpr expr
        | Binary b ->
            //TODO: parallelize? not possible because of side effects via environment
            let leftVal = (tryEvaluateRec b.LeftOperand)
            let rightVal = (tryEvaluateRec b.RightOperand)

            monad' {
                let! l = leftVal
                let! r = rightVal

                return! binOpEvaluator b.BinaryOp l r
            }

        | SimpleUnary (op, exp) ->
            monad' {
                let! value = tryEvaluateRec exp
                return! unaryOpEvaluator op value
            }
        | FunCall fc ->
            let parametersValues =
                fc.ActualParameters
                |> Utils.traverseM tryEvaluateRec

            parametersValues
            |> Result.bind (fun v -> v |> funEvaluator fc.Name)
        | Increment (op, exp) -> incrementEvaluator op exp
        | ListCreation (expList) ->
            expList
            |> (Utils.traverseM tryEvaluateRec)
            |> Result.map ListValue
