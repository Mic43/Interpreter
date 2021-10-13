namespace Interpreter.AST

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
            with
            | _ ->
                "Arithmetic error"
                |> (Errors.createResult ErrorType.Other)

        let evalLogicalExp op =
            match op with
            | And -> val1 .&& val2
            | Or -> val1 .|| val2

        let evalRelationalExp op =
            match op with
            | Equal -> val1 .== val2
            | Greater -> val1 .> val2
            | Less -> val1 .< val2

        match op with
        | BinaryOp.ArithmeticOp op -> (evalArithmeticExp op)
        | BinaryOp.LogicalOp op -> (evalLogicalExp op)
        | BinaryOp.RelationalOp op -> (evalRelationalExp op)

    let unaryOpEvaluator op value =
        match op with
        | Negate -> !value
        | Minus -> -(value)


    let rec tryEvaluate
        varUpdater
        varEvaluator
        constEvaluator
        binOpEvaluator
        unaryOpEvaluator
        funEvaluator
        (expression: Expression)
        : Result<Value, RunError> =

        let tryEvaluateRec =
            tryEvaluate varUpdater varEvaluator constEvaluator binOpEvaluator unaryOpEvaluator funEvaluator

        let incrementEvaluator op exp =
            match exp with
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
            | _ -> Errors.createResult ErrorType.Other "Left operand of assignment expression must be LValue"

        let assignmentEvaluator mutableExpr expr =
            match mutableExpr with
            | Mutable mutableExpr ->
                match mutableExpr with
                | Var ident ->
                    monad' {
                        let! value = (tryEvaluateRec expr)
                        do! varUpdater ident value
                        return value
                    }
            | _ -> Errors.createResult ErrorType.Other "Left operand of assignment expression must be LValue"

        let mutableExpEvaluator mutableExpr =
            match mutableExpr with
            | Var v -> varEvaluator v
            | IndexedVar (ident, exp) ->
                monad' {
                    let! index = exp |> tryEvaluateRec
                    let! value = varEvaluator ident

                    return!
                        match (value, index) with
                        | (ListValue lv, IntValue index) -> lv.[index] |> Result.Ok
                        | (StringValue s, IntValue index) -> s.[index] |> string |> StringValue |> Result.Ok
                        | _ -> Errors.createResult ErrorType.Other "indexing expression must evaluate to int"
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
