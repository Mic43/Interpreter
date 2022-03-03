namespace Interpreter.AST

open FSharpPlus

module StmtEvaluator =
    let rec private evaluateScopedStmt
        (environment: ExecutionEnvironment)
        (exp: ScopedStatement)
        : Result<Value, EvalStopped> =

        let evaluateScopedStmtRec = evaluateScopedStmt environment

        let evaluateBlock block =
            Environment.nestNewEmptyEnvironment environment

            let res =
                block.Content
                |> Utils.traverseMTail evaluateScopedStmtRec
                |> Result.map (fun _ -> Value.Void)

            do environment |> Environment.returnToParent
            res

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
                        (Errors.create
                            ErrorType.Other
                            (funIdentifier.ToStr()
                             |> sprintf "function not defined: %s"))
                        |> EvalError
                    )

                match foundFunc with
                | Function func ->
                    let! paramsWithValues =
                        actualParametersValues
                        |> List.map ref
                        |> Result.protect (List.zip func.Parameters)
                        |> Result.mapError (fun _ ->
                            Errors.create ErrorType.Evaluation "wrong parameters count"
                            |> EvalError)

                    Environment.nestNewEnvironment environment (paramsWithValues |> Map.ofSeq)

                    let! res =
                        func.Body
                        |> evaluateBlock
                        |> Value.getReturnedValue

                    environment |> Environment.returnToParent

                    return res
                | CompiledFunction compiledFun ->
                    return!
                        (compiledFun.Execute actualParametersValues)
                        |> Result.mapError EvalError
            }

        let calculateInitValue (evaluateExpression: ExpEvaluator) varDeclarationInit =
            varDeclarationInit.InitExpression
            |> Option.defaultValue (Expression.voidConstant ())
            |> evaluateExpression

        let evaluateVarDeclaration evaluateExpression vd =
            monad' {
                let! initVal = vd |> calculateInitValue evaluateExpression

                return!
                    (Environment.tryDefineVar environment vd.Name initVal)
                    |> (Result.map Value.createVoid)
                    |> Result.mapError EvalError
            }

        let evalUserTypeCreation (evaluateExpression: ExpEvaluator) userTypeExp =
            let userTypeFinder = Environment.tryGetUserType environment
            let varEvaluator = Environment.tryGetVarValue environment

            monad' {
                let! userType =
                    userTypeFinder userTypeExp.StructTypeName
                    |> Result.mapError EvalError

                match userType.Kind with
                | Struct s ->
                    Environment.nestNewEmptyEnvironment environment

                    let allFields =
                        s.Members
                        |> Map.toList
                        |> List.choose (fun (ident, mem) ->
                            match mem with
                            | Field f -> Some(ident, f))

                    let! userInitializedfields =
                        userTypeExp.FieldsInitialization
                        |> Map.toList
                        |> Utils.traverseM (fun (identifier, initializerExp) ->
                            evaluateExpression initializerExp
                            |> Result.map (fun v -> (identifier, ref v)))
                        |> Result.map (Map.ofList)

                    let! fields =
                        allFields
                        |> Utils.traverseM (fun (id, vd) ->
                            (userInitializedfields.TryFind id)
                            |> Option.map Ok
                            |> Option.defaultWith (fun () ->

                                evaluateVarDeclaration evaluateExpression vd
                                |> ignore

                                varEvaluator vd.Name |> Result.mapError EvalError)
                            |> Result.map (fun v -> (id, v)))
                        |> Result.map (Map.ofList)

                    Environment.returnToParent environment

                    return
                        { StructValue.TypeName = userTypeExp.StructTypeName
                          Fields = fields }
                        |> StructValue
            }

        let rec evaluateExpression exp =
            ExpEvaluator.tryEvaluate
                (Environment.tryGetVarValue environment)
                ExpEvaluator.constEvaluator
                ExpEvaluator.binaryOpEvaluator
                ExpEvaluator.unaryOpEvaluator
                evaluateFunctionCall
                (evalUserTypeCreation evaluateExpression)
                exp


        let rec loop body condition incrementExp : Result<Value, EvalStopped> =
            monad' {
                let! condVal = condition |> evaluateExpression

                if (condVal |> Value.toBool) then
                    let! _ = body |> evaluateScopedStmtRec
                    let! _ = incrementExp |> evaluateExpression
                    return! loop body condition incrementExp
                else
                    return Value.Void
            }


        let evaluateVarDeclaration =
            evaluateVarDeclaration evaluateExpression

        let calculateInitValue = calculateInitValue evaluateExpression

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
                        ifs.OnFalse |> Option.defaultValue Empty

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
                            let! initValue = vd |> calculateInitValue

                            [ vd.Name, ref initValue ]
                            |> Map.ofList
                            |> Environment.nestNewEnvironment environment

                            Value.Void
                        }

                let! loopRes = (loop fs.Body fs.Condition fs.Increment)
                Environment.returnToParent environment
                return loopRes
            }
        | ReturnStatement exp ->
            (evaluateExpression exp)
            |> Result.bind (ReturnStmtReached >> Error)
        | Empty -> Value.Void |> Ok

    let evaluate (environment: ExecutionEnvironment) statement =
        match (statement, environment.IsCurrentGlobal) with
        | (FunDeclaration fd, true) -> Environment.tryDefineCallable environment (fd |> Function)
        | (UserTypeDeclaration userTypeDecl, true) -> Environment.tryDefineUserType environment userTypeDecl
        | (FunDeclaration _, false)
        | (UserTypeDeclaration _, false) ->
            "Function and structures can be defined only on global scope"
            |> Errors.createResult Other
        | (ScopedStatement stmt, _) ->
            evaluateScopedStmt environment stmt
            |> Result.mapError (fun e ->
                match e with
                | EvalError re -> re
                | _ -> invalidOp "Return should be replaced by its value by this point")
