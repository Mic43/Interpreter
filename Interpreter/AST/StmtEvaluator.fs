namespace Interpreter.AST

open FSharpPlus
open FSharpPlus.Data

module StmtEvaluator =
    let rec private evaluateScopedStmt
        (environment: ExecutionEnvironment)
        (exp: ScopedStatement)
        : Result<Value, EvaluationStopped> =

        let evaluateScopedStmtRec =
            evaluateScopedStmt environment

        let evaluateBlock environment block =
            Environment.nestNewEmptyEnvironment environment

            let res =
                block.Content
                |> Traversable.traverseMTail (evaluateScopedStmt environment)
                |> Result.map (fun _ -> Value.Void)

            do environment |> Environment.returnToParent
            res

        let evaluateFunctionCall funIdentifier (actualParametersValues: Value list) =

            monad' {
                let! foundFunc =
                    Environment.tryGetCallable environment funIdentifier
                    |> Result.mapError RuntimeError

                match foundFunc with
                | Function func ->
                    let! paramsWithValues =
                        actualParametersValues
                        |> List.map ref
                        |> Result.protect (List.zip func.Parameters)
                        |> Result.mapError (fun _ -> ExecuteError.createRuntimeError "wrong parameters count")
                        |> Result.mapError RuntimeError

                    let newEnvironment =
                        Environment.create environment.Global

                    Environment.nestNewEnvironment newEnvironment (paramsWithValues |> Map.ofSeq)

                    return!
                        func.Body
                        |> evaluateBlock newEnvironment
                        |> Value.getReturnedValue

                | CompiledFunction compiledFun ->
                    return!
                        (compiledFun.Execute actualParametersValues)
                        |> Result.mapError RuntimeError
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
                    |> Result.mapError RuntimeError
            }

        let evalUserTypeCreation (evaluateExpression: ExpEvaluator) userTypeExp =
            let userTypeFinder =
                Environment.tryGetUserType environment

            let varEvaluator =
                Environment.tryGetVarValue environment

            monad' {
                let! userType =
                    userTypeFinder userTypeExp.StructTypeName
                    |> Result.mapError RuntimeError

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
                        |> Traversable.traverseM (fun (identifier, initializerExp) ->
                            evaluateExpression initializerExp
                            |> Result.map (fun v -> (identifier, ref v)))
                        |> Result.map Map.ofList

                    let! fields =
                        allFields
                        |> Traversable.traverseM (fun (id, vd) ->
                            (userInitializedfields.TryFind id)
                            |> Option.map Ok
                            |> Option.defaultWith (fun () ->

                                evaluateVarDeclaration evaluateExpression vd
                                |> ignore

                                varEvaluator vd.Name
                                |> Result.mapError RuntimeError)
                            |> Result.map (fun v -> (id, v)))
                        |> Result.map Map.ofList

                    Environment.returnToParent environment

                    return
                        { StructValue.TypeName = userTypeExp.StructTypeName
                          Fields = fields }
                        |> StructValue
            }

        let rec evaluateExpression exp =
            ExpEvaluator.tryEvaluate
                (BasicEvaluators.Create
                    (Environment.tryGetVarValue environment)
                    ExpEvaluator.constEvaluator
                    ExpEvaluator.binaryOpEvaluator
                    ExpEvaluator.unaryOpEvaluator
                    evaluateFunctionCall
                    (evalUserTypeCreation evaluateExpression))
                exp

        let rec loop body condition incrementExp : Result<Value, EvaluationStopped> =
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

        let calculateInitValue =
            calculateInitValue evaluateExpression

        match exp with
        | ExpressionStatement exp -> exp |> evaluateExpression
        | BlockStatement block -> block |> evaluateBlock environment
        | VarDeclarationStatement vd -> vd |> evaluateVarDeclaration
        | IfStatement ifs ->
            monad' {
                let! condRes = ifs.Condition |> evaluateExpression

                let branch =
                    if condRes |> Value.toBool then
                        ifs.OnTrue
                    else
                        ifs.OnFalse |> Option.defaultValue Empty

                return! (branch |> evaluateScopedStmtRec)
            }
        | WhileStatement ws -> loop ws.Body ws.Condition (Expression.voidConstant ())
        | ForStatement fs ->
            match fs.Initializer with
            | ExpressionInit ex ->
                monad' {
                    let! _ = ex |> evaluateExpression
                    return! (loop fs.Body fs.Condition fs.Increment)
                }

            | VarDeclarationInit vd ->
                monad' {
                    let! initValue = vd |> calculateInitValue

                    [ vd.Name, ref initValue ]
                    |> Map.ofList
                    |> Environment.nestNewEnvironment environment

                    let! loopRes = (loop fs.Body fs.Condition fs.Increment)

                    do Environment.returnToParent environment

                    return loopRes
                }

        | ReturnStatement exp ->
            (evaluateExpression exp)
            |> Result.bind (ReturnStmtReached >> Error)
        | Empty -> Value.Void |> Ok

    let evaluate (environment: ExecutionEnvironment) statement =
        match (statement, environment.IsCurrentGlobal) with
        | FunDeclaration fd, true -> Environment.tryDefineCallable environment (fd |> Function)
        | UserTypeDeclaration userTypeDecl, true -> Environment.tryDefineUserType environment userTypeDecl
        | FunDeclaration _, false
        | UserTypeDeclaration _, false ->
            "Function and structures can be defined only on global scope"
            |> ExecuteError.createRuntimeErrorResult
        | ScopedStatement stmt, _ ->
            evaluateScopedStmt environment stmt
            |> Result.mapError (fun e ->
                match e with
                | RuntimeError re -> re
                | _ -> invalidOp "Return should be replaced by its value by this point")

    let evaluateWithInfo
        (environment: ExecutionEnvironment)
        (statementWithInfo: StatementWithInfo)
        : Result<Value, RuntimeError * StatementPosition> =

        evaluate environment statementWithInfo.Statement
        |> Result.mapError (fun re -> (re, statementWithInfo.Info))
