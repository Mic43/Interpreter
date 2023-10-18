namespace Interpreter.AST

open FSharpPlus
open System.Collections.Generic
open System.Linq

type ExpEvaluator = Expression -> Result<Value, EvaluationStopped>

type BasicEvaluators =
    { VarEvaluator: Identifier -> Result<Ref<Value>, RuntimeError>
      ConstEvaluator: Value -> Result<Value, EvaluationStopped>
      BinOpEvaluator: BinaryOp -> Value -> Value -> Result<Value, EvaluationStopped>
      UnaryOpEvaluator: UnaryOp -> Value -> Result<Value, EvaluationStopped>
      FunEvaluator: Identifier -> Value list -> Result<Value, EvaluationStopped>
      EvalUserTypeCreation: StructCreationExpression -> Result<Value, EvaluationStopped> }

    static member Create varEvaluator constEvaluator binOpEvaluator unaryOpEvaluator funEvaluator evalUserTypeCreation =
        { VarEvaluator = varEvaluator
          ConstEvaluator = constEvaluator
          BinOpEvaluator = binOpEvaluator
          UnaryOpEvaluator = unaryOpEvaluator
          FunEvaluator = funEvaluator
          EvalUserTypeCreation = evalUserTypeCreation }

module ExpEvaluator =
    open Value

    let constEvaluator value = Result.Ok value

    let binaryOpEvaluator op (val1: Value) (val2: Value) =
        let evalArithmeticExp val1 val2 =
            function
            | Add _ -> (val1 + val2)
            | Sub _ -> (val1 - val2)
            | Mul _ -> (val1 * val2)
            | Div _ -> (val1 / val2)

        let evalArithmeticExp op =
            try
                (evalArithmeticExp val1 val2 op)
                |> Result.mapError RuntimeError
            with
            | _ ->
                "Arithmetic error"
                |> ExecuteError.createRuntimeErrorResult
                |> Result.mapError RuntimeError

        let evalLogicalExp op =
            match op with
            | And -> val1 .&& val2
            | Or -> val1 .|| val2

        let evalRelationalExp op =
            (match op with
             | Equal -> val1 .== val2
             | Greater -> val1 .> val2
             | Less -> val1 .< val2)
            |> Result.mapError RuntimeError

        match op with
        | ArithmeticOp op -> (evalArithmeticExp op)
        | LogicalOp op -> (evalLogicalExp op)
        | RelationalOp op -> (evalRelationalExp op)

    let unaryOpEvaluator op value =
        match op with
        | Negate -> !value
        | Minus -> -(value)
        |> Result.mapError RuntimeError

    let rec tryEvaluate (basicEvaluators: BasicEvaluators) (expression: Expression) : Result<Value, EvaluationStopped> =

        let tryEvaluateRec =
            tryEvaluate basicEvaluators

        let rec mutableExpEvaluator mutableExpr : Result<Ref<Value>, EvaluationStopped> =

            match mutableExpr with
            | Var v ->
                basicEvaluators.VarEvaluator v
                |> Result.mapError RuntimeError
            | IndexedVar (ident, exp) ->
                monad' {
                    let! index = exp |> tryEvaluateRec
                    let! value = mutableExpEvaluator ident

                    return!
                        match (value.Value, index) with
                        | ListValue lv, IntValue index ->
                            if index < lv.Length then
                                lv.[index] |> Ok
                            else
                                $"array index out of bounds: {index}"
                                |> ExecuteError.createRuntimeErrorResult
                        | StringValue s, IntValue index ->
                            if index < s.Length then
                                ref (s.[index] |> string |> StringValue) |> Ok
                            else
                                $"string index out of bounds: {index}"
                                |> ExecuteError.createRuntimeErrorResult
                        | _ ->
                            "indexing expression must evaluate to int"
                            |> ExecuteError.createRuntimeErrorResult
                        |> Result.mapError RuntimeError
                }
            | MemberAccess (var, field) ->
                monad' {
                    let! value = mutableExpEvaluator var

                    return!
                        match value.Value with
                        | StructValue sv ->
                            sv.Fields
                            |> Map.tryFind field
                            |> Option.toResultWith (ExecuteError.createRuntimeError $"Cannot find member: {field}")
                        | _ ->
                            "left side of member access operator must be struct"
                            |> ExecuteError.createRuntimeErrorResult
                        |> Result.mapError RuntimeError
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
                |> ExecuteError.createRuntimeErrorResult
                |> Result.mapError RuntimeError

        let incrementEvaluator op exp =
            match exp with
            | Mutable mutableExpr ->
                monad' {
                    let! valueRef = mutableExpEvaluator mutableExpr
                    let oldValue = valueRef.Value

                    let! newValue =
                        (oldValue + (1 |> IntValue))
                        |> Result.mapError RuntimeError

                    valueRef.Value <- newValue

                    return
                        match op with
                        | Pre -> newValue
                        | Post -> oldValue
                }
            | _ ->
                "Left operand of assignment expression must be LValue"
                |> ExecuteError.createRuntimeErrorResult
                |> Result.mapError RuntimeError

        match expression with
        | Constant c -> constEvaluator c
        | Mutable m ->
            mutableExpEvaluator m
            |> Result.map (fun r -> r.Value)
        | Assignment (mutableExpr, expr) -> assignmentEvaluator mutableExpr expr
        | Binary b ->
            let leftVal = (tryEvaluateRec b.LeftOperand)

            let rightVal =
                (tryEvaluateRec b.RightOperand)

            monad' {
                let! l = leftVal
                let! r = rightVal

                return! basicEvaluators.BinOpEvaluator b.BinaryOp l r
            }
        | SimpleUnary (op, exp) ->
            monad' {
                let! value = tryEvaluateRec exp
                return! unaryOpEvaluator op value
            }
        | FunCall fc ->
            let parametersValues =
                fc.ActualParameters
                |> Traversable.traverseM tryEvaluateRec

            parametersValues
            |> Result.bind (fun v -> v |> basicEvaluators.FunEvaluator fc.Name)
        | Increment (op, exp) -> incrementEvaluator op exp
        | ListCreation expList ->
            expList
            |> (Traversable.traverseM tryEvaluateRec)
            |> Result.map (fun l -> l |> List.map (fun i -> ref i) |> ListValue)
        | UserTypeCreation userTypeExp -> basicEvaluators.EvalUserTypeCreation userTypeExp
