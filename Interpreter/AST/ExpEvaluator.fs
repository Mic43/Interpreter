﻿namespace Interpreter.AST

open FSharpPlus
open System.Collections.Generic
open System.Linq

type ExpEvaluator = Expression -> Result<Value, EvalStopped>

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
        | ArithmeticOp op -> (evalArithmeticExp op)
        | LogicalOp op -> (evalLogicalExp op)
        | RelationalOp op -> (evalRelationalExp op)

    let unaryOpEvaluator op value =
        match op with
        | Negate -> !value
        | Minus -> -(value)
        |> Result.mapError EvalError

    let rec tryEvaluate
     //   (userTypeFinder: Identifier -> Result<UserType, RunError>)
        (varEvaluator: Identifier -> Result<Ref<Value>, ExecuteError>)
        constEvaluator
        binOpEvaluator
        unaryOpEvaluator
        funEvaluator
        evalUserTypeCreation      
        (expression: Expression)
        : Result<Value, EvalStopped> =

        let tryEvaluateRec =
            tryEvaluate varEvaluator constEvaluator binOpEvaluator unaryOpEvaluator funEvaluator evalUserTypeCreation//varDeclEvaluator

        let rec mutableExpEvaluator mutableExpr : Result<Ref<Value>, EvalStopped> =

            match mutableExpr with
            | Var v -> varEvaluator v |> Result.mapError EvalError
            | IndexedVar (ident, exp) ->
                monad' {
                    let! index = exp |> tryEvaluateRec
                    let! value = mutableExpEvaluator ident

                    return!
                        match (value.Value, index) with
                        | (ListValue lv, IntValue index) ->
                            if index < lv.Length then
                                lv.[index] |> Ok
                            else
                                $"array index out of bounds: {index}"
                                |> Errors.createResult Other
                        | (StringValue s, IntValue index) ->
                            if index < s.Length then
                                ref (s.[index] |> string |> StringValue) |> Ok
                            else
                                $"string index out of bounds: {index}"
                                |> Errors.createResult Other
                        | _ ->
                            "indexing expression must evaluate to int"
                            |> Errors.createResult Other
                        |> Result.mapError EvalError
                }
            | MemberAccess (var, field) ->
                monad' {
                    let! value = mutableExpEvaluator var

                    return!
                        match value.Value with
                        | (StructValue sv) ->
                            sv.Fields
                            |> Map.tryFind field
                            |> Option.toResultWith (Errors.create Other $"Cannot find member: {field}")
                        | _ -> "left side of member access operator must be struct" |> Errors.createResult Other
                        |> Result.mapError EvalError
                }

        let assignmentEvaluator mutableExpr expr =
            match mutableExpr with
            | Mutable mutableExpr ->
                monad' {
                    let! result = mutableExpEvaluator mutableExpr
                    let! newValue = (tryEvaluateRec expr)

                    result.Value <- newValue

                    return newValue
                }
            | _ ->
                "Left operand of assignment expression must be LValue"
                |> Errors.createResult Other
                |> Result.mapError EvalError

        let incrementEvaluator op exp =
            match exp with
            | Mutable mutableExpr ->
                monad' {
                    let! valueRef = mutableExpEvaluator mutableExpr
                    let oldValue = valueRef.Value

                    let! newValue =
                        (oldValue + (1 |> IntValue))
                        |> Result.mapError EvalError

                    valueRef.Value <- newValue

                    return
                        match op with
                        | Pre -> newValue
                        | Post -> oldValue
                }
            | _ ->
                "Left operand of assignment expression must be LValue"
                |> Errors.createResult Other
                |> Result.mapError EvalError

        match expression with
        | Constant c -> constEvaluator c
        | Mutable m ->
            mutableExpEvaluator m
            |> Result.map (fun r -> r.Value)
        | Assignment (mutableExpr, expr) -> assignmentEvaluator mutableExpr expr
        | Binary b ->            
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
            |> Result.map (fun l -> l |> List.map (fun i -> ref i) |> ListValue)
        | UserTypeCreation (userTypeExp) ->
            evalUserTypeCreation userTypeExp
        
