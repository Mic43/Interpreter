namespace Interpreter.AST


module StmtSimplifier =

    let rec simplify (simplifyExp: Expression -> Expression) (stmt: Statement) : Statement =

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

            match stmt with
            | ExpressionStatement e -> e |> simplifyExp |> ExpressionStatement
            | VarDeclarationStatement ({ InitExpression = ie } as vds) ->
                { vds with InitExpression = ie |> Option.map simplifyExp }
                |> VarDeclarationStatement
            | BlockStatement block -> block |> simplifyBlock |> BlockStatement
            | IfStatement ifStmt -> ifStmt |> simplifyIf
            | WhileStatement whileStmt -> stmt
            | ForStatement forStmt -> stmt
            | ReturnStatement exp -> exp |> simplifyExp |> ExpressionStatement
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