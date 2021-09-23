namespace Interpreter.AST

open FSharpPlus

type Runner() =
    let defaultEnvironment = Environment.createEmptyGlobal ()
    let mutable currentEnvironment = defaultEnvironment

    let ignoreResuls (res: Result<Value list, RunError>) =
        res |> Result.map (fun _ -> Value.Void)

    let getLastResultOrVoid (res: Result<Value list, RunError>) =
        res
        |> Result.map
            (fun vl ->
                vl
                |> (List.tryLast
                    >> (Option.defaultValue Value.Void)))

    let tryPrint =
        function
        | IntValue iv ->
            printf "%i" iv
            Value.Void |> Result.Ok
        | FloatValue fv ->
            printf "%f" fv
            Value.Void |> Result.Ok
        | VoidValue _ ->
            "Cannot print void value"
            |> (Errors.createResult Other)

    let returnToParentEnvironment () =
        currentEnvironment <-
            (match currentEnvironment.Kind with
             | Scoped s -> s.Parent
             | Global _ -> failwith "internal error")

    let rec evaluateScopedStmt expressionEvaluator (exp: ScopedStatement) : Result<Value, RunError> =
        match exp with
        | ExpressionStatement exp -> expressionEvaluator exp
        | Block block ->
            block
            |> traverse (evaluateScopedStmt expressionEvaluator)
            |> getLastResultOrVoid
            |> Result.bind
                (fun res ->
                    do returnToParentEnvironment ()
                    res |> Result.Ok)
        | VarDeclaration vd ->
            monad' {
                let! initVal = vd.Initializer |> expressionEvaluator

                return!
                    (Environment.tryDefineVar currentEnvironment vd.Name initVal)
                    |> (Result.map Value.createVoid)
            }

        | PrintStatement exp -> //TODO: maybe put to environment
            monad' {
                let! v = (expressionEvaluator exp)
                return! (v |> tryPrint)
            }

    let rec evaluateExpression exp =
        Evaluators.tryEvaluate
            (Environment.tryUpdateVar currentEnvironment)
            (Environment.tryGetVar currentEnvironment)
            Evaluators.Basic.constEvaluator
            Evaluators.Basic.binaryOpEvaluator
            Evaluators.Basic.unaryOpEvaluator
            (Evaluators.Basic.funEvaluator
                (fun newEnv -> currentEnvironment <- newEnv)
                (evaluateScopedStmt evaluateExpression)
                currentEnvironment)
            exp

    let evaluateStmt statement =
        match (statement, currentEnvironment.Kind) with
        | (FunDeclaration fd, Global ge) -> Environment.tryDefineFunction ge fd
        | (ScopedStatement stmt, _) -> evaluateScopedStmt evaluateExpression stmt

    member this.Run =
        function
        | Program program ->
            program
            |> (Utils.traverseM evaluateStmt)
            |> getLastResultOrVoid
