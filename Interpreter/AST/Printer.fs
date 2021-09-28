namespace Interpreter.AST

module Printer =
    let rec expressionToStr (exp: Expression) =
        match exp with
        | Constant c -> c |> Value.toStr
        | Binary (_) -> failwith "Not Implemented"
        | Unary (_, _) -> failwith "Not Implemented"
        | Assignment (_, _) -> failwith "Not Implemented"
        | FunCall (_) -> failwith "Not Implemented"
        | Mutable (me) ->
            match me with
            | Var ident -> ident |> Identifier.toStr

    let rec scopedStmtToStr (stmt: ScopedStatement) =
        match stmt with
        | Empty -> ""
        | ExpressionStatement e -> expressionToStr e
        | VarDeclaration (vd) ->
            sprintf
                "%s = %s"
                (vd.Name |> Identifier.toStr)
                (vd.Initializer
                 |> ExpressionStatement
                 |> scopedStmtToStr)
        | BlockStatement block ->
            sprintf
                "{\n %s \n}"
                (block.Content
                 |> (List.map scopedStmtToStr)
                 |> List.reduce (fun acc s -> sprintf "%s\n%s" acc s))


    let rec stmtToStr (stmt: Statement) =
        match stmt with
        | FunDeclaration fd ->
            sprintf
                "%s (%s) \n %s"
                (fd.Name |> Identifier.toStr)
                (fd.Parameters
                 |> List.fold (fun s p -> sprintf "%s, %s" s (p |> Identifier.toStr)) "")
                (fd.Body
                 |> BlockStatement
                 |> ScopedStatement
                 |> stmtToStr)
        | ScopedStatement stmt -> scopedStmtToStr stmt

    let toStr (Program statementsList) =
        statementsList
        |> List.map stmtToStr
        |> List.reduce (fun acc s -> sprintf "%s\n%s" acc s)
