namespace Interpreter.AST

open FSharpPlus

module StmtEvaluator =
    let rec private evaluateScopedStmt
        (environment: ExecutionEnvironment)
        (exp: ScopedStatement)
        : Result<Value, RunError> =

        let evaluateScopedStmtRec = evaluateScopedStmt environment

        let evaluateBlock block =
            Environment.nestNewEmptyEnvironment environment

            block.Content
            |> Utils.traverseM (evaluateScopedStmt environment)
            |> Value.getLastResultOrVoid
            |> Result.bind
                (fun res ->
                    environment |> Environment.returnToParent
                    res |> Ok)

        let evaluateFunctionCall funIdentifier (actualParametersValues: Value list) =
            let funcs =
                match environment.Global.Kind with
                | Global g -> g.Functions
                | _ -> invalidOp "default environment must be of global kind"

            monad' {
                let! foundFunc =
                    (funcs.TryGetValue funIdentifier)
                    |> Option.ofPair
                    |> Option.toResultWith (
                        Errors.create
                            ErrorType.Other
                            (funIdentifier.ToStr()
                             |> sprintf "function not defined: %s")
                    )

                match foundFunc with
                | Function func ->
                    let! paramsWithValues =
                        actualParametersValues
                        |> Result.protect (List.zip func.Parameters)
                        |> Result.mapError (fun e -> Errors.create ErrorType.Evaluation "wrong parameters count")

                    Environment.nestNewEnvironment environment (paramsWithValues |> Map.ofSeq)

                    let! res = func.Body |> evaluateBlock

                    environment |> Environment.returnToParent

                    return res
                | CompiledFunction compiledFun -> return! compiledFun.Execute actualParametersValues
            }

        let evaluateExpression exp =
            ExpEvaluator.tryEvaluate
                (Environment.tryUpdateVar environment)
                (Environment.tryGetVarValue environment)
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
        | ExpressionStatement exp -> exp |> evaluateExpression
        | BlockStatement block -> block |> evaluateBlock
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
        | Empty -> Value.Void |> Ok

    let evaluate (environment: ExecutionEnvironment) statement =
        match (statement, environment.IsCurrentGlobal) with
        | (FunDeclaration fd, true) -> Environment.tryDefineCallable environment (fd |> Function)
        | (FunDeclaration _, false) ->
            "Function can be defined only on global scope"
            |> Errors.createResult Other
        | (ScopedStatement stmt, _) -> evaluateScopedStmt environment stmt
