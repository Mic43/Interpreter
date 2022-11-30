namespace Interpreter.AST


module StmtSimplifier =

    let rec private simplify (simplifyExp: Expression -> Expression) (stmt: Statement) : Statement =

        let rec simplifyBlock block =
            { block with Content = block.Content |> List.map simplifyScopedStatement }

        and simplifyScopedStatement (stmt: ScopedStatement) =

            let simplifyIf ifStmt =
                let simplified =
                    { Condition = ifStmt.Condition |> simplifyExp
                      OnTrue = ifStmt.OnTrue |> simplifyScopedStatement
                      OnFalse =
                        ifStmt.OnFalse
                        |> Option.map simplifyScopedStatement }

                match simplified with
                | { Condition = c } when c = Expression.trueConstant -> simplified.OnTrue
                | { Condition = c } when c = Expression.falseConstant -> simplified.OnFalse |> Option.defaultValue Empty
                | _ -> simplified |> IfStatement

            let simplifyWhile (whileStmt: While) =
                { Condition = whileStmt.Condition |> simplifyExp
                  Body = whileStmt.Body |> simplifyScopedStatement }

            let simplifyForStmt (forStmt: For) =
                { forStmt with
                    Condition = forStmt.Condition |> simplifyExp
                    Body = forStmt.Body |> simplifyScopedStatement
                    Increment = forStmt.Increment |> simplifyExp }

            match stmt with
            | ExpressionStatement e -> e |> simplifyExp |> ExpressionStatement
            | VarDeclarationStatement ({ InitExpression = ie } as vds) ->
                { vds with InitExpression = ie |> Option.map simplifyExp }
                |> VarDeclarationStatement
            | BlockStatement block -> block |> simplifyBlock |> BlockStatement
            | IfStatement ifStmt -> ifStmt |> simplifyIf
            | WhileStatement whileStmt -> whileStmt |> simplifyWhile |> WhileStatement 
            | ForStatement forStmt -> forStmt |> simplifyForStmt |> ForStatement
            | ReturnStatement exp -> exp |> simplifyExp |> ReturnStatement
            | Empty -> stmt

        match stmt with
        | FunDeclaration ({ Body = body } as fds) ->
            { fds with Body = body |> simplifyBlock }
            |> FunDeclaration
        | UserTypeDeclaration _ -> stmt
        | ScopedStatement sc -> sc |> simplifyScopedStatement |> ScopedStatement

    let simplifyProgram simplifyExp =
        function
        | Program p -> p |> List.map (simplifyExp |> simplify) |> Program
