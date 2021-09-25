namespace Interpreter.AST

module Printer =
    let rec expressionToStr (exp: Expression) =
        match exp with
        | Constant c -> c |> Value.toStr
        | Binary (_) -> failwith "Not Implemented"
        | Unary (_, _) -> failwith "Not Implemented"
        | Assignment (_, _) -> failwith "Not Implemented"
        | FunCall (_) -> failwith "Not Implemented"
        | Var (_) -> failwith "Not Implemented"

    let rec scopedStmtToStr (stmt: ScopedStatement) =
        match stmt with
        | Empty -> ""
        | ExpressionStatement e -> expressionToStr e
        | VarDeclaration (vd) ->
            sprintf
                "%s = %A"
                (vd.Name |> Identifier.toStr)
                (vd.Initializer
                 |> ExpressionStatement
                 |> scopedStmtToStr)
        | PrintStatement (_) -> failwith "Not Implemented"
        | BlockStatement block ->
            sprintf
                "{\n %s }"
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

    let toStr (Program statementsList) = statementsList |> List.map stmtToStr
