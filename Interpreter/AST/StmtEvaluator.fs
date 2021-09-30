namespace Interpreter.AST

open FSharpPlus

module StmtEvaluator =   
    let rec private evaluateScopedStmt
        (environment: ExecutionEnvironment)
        (exp: ScopedStatement)
        : Result<Value, RunError> =
        let evaluateScopedStmtRec = evaluateScopedStmt environment

        let evaluateBlock block =
            block.Content
            |> traverse (evaluateScopedStmt environment)
            |> Value.getLastResultOrVoid

        let evaluateFunctionCall funIdentifier (actualParametersValues: Value list) =
            let funcs =
                match environment.Global.Kind with
                | Global g -> g.Functions
                | _ -> invalidOp "default environment must be of global kind"

            monad' {
                let! foundFunc =
                    (funcs.ContainsKey funIdentifier, funcs.[funIdentifier])
                    |> Option.ofPair
                    |> Option.toResultWith (Errors.create ErrorType.Other "function not defined")

                match foundFunc with
                | Function func ->
                    let! paramsWithValues =
                        actualParametersValues
                        |> Result.protect (List.zip func.Parameters)
                        |> Result.mapError (fun e -> Errors.create ErrorType.Evaluation "wrong parameters count")

                    Environment.nestNewEnvironment environment (paramsWithValues |> Map.ofSeq)
                    let! res = evaluateBlock func.Body
                    environment |> Environment.returnToParent

                    return res
                | CompiledFunction compiledFun -> return! compiledFun.Execute actualParametersValues
            }

        let evaluateExpression exp =
            ExpEvaluator.tryEvaluate
                (Environment.tryUpdateVar environment)
                (Environment.tryGetVar environment)
                ExpEvaluator.constEvaluator
                ExpEvaluator.binaryOpEvaluator
                ExpEvaluator.unaryOpEvaluator
                evaluateFunctionCall
                exp

        let evaluateVarDeclaration vd =
            monad' {
                let! initVal = vd.InitExpression |> evaluateExpression

                return!
                    (Environment.tryDefineVar environment vd.Name initVal)
                    |> (Result.map Value.createVoid)
            }

        let rec loop body condition incrementExp : Result<Value, RunError> =
            monad' {
                let! condVal = condition |> evaluateExpression

                if (condVal |> Value.toBool) then
                    let! _ = body |> evaluateScopedStmtRec
                    let! _ = incrementExp |> evaluateExpression
                    return! loop body condition incrementExp
                else
                    return Value.Void
            }

        match exp with
        | ExpressionStatement exp -> evaluateExpression exp
        | BlockStatement block ->
            environment |> Environment.nestNewEmptyEnvironment

            monad' {
                let! res = evaluateBlock block
                environment |> Environment.returnToParent
                return res
            }
        | VarDeclarationStatement vd -> vd |> evaluateVarDeclaration
        | IfStatement (ifs) ->
            monad' {
                let! condRes = ifs.Condition |> evaluateExpression

                let branch =
                    if condRes |> Value.toBool then
                        ifs.OnTrue
                    else
                        ifs.OnFalse

                return! (branch |> evaluateScopedStmtRec)
            }
        | WhileStatement (ws) -> loop ws.Body ws.Condition (Expression.voidConstant ())
        | ForStatement fs ->
            monad' {
                let! _ =
                    match fs.Initializer with
                    | ExpressionInit ex -> ex |> evaluateExpression
                    | VarDeclarationInit vd ->
                        monad' {
                            let! initValue = vd.InitExpression |> evaluateExpression

                            [ vd.Name, initValue ]
                            |> Map.ofList
                            |> Environment.nestNewEnvironment environment

                            Value.Void
                        }

                return! (loop fs.Body fs.Condition fs.Increment)
            }

    let evaluate (environment: ExecutionEnvironment) statement =
        match (statement, environment.IsCurrentGlobal) with
        | (FunDeclaration fd, true) -> Environment.tryDefineCallable environment (fd |> Function)
        | (FunDeclaration _, false) ->
            "Function can be defined only on global scope"
            |> Errors.createResult Other
        | (ScopedStatement stmt, _) -> evaluateScopedStmt environment stmt
