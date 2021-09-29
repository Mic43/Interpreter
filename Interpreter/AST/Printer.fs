namespace Interpreter.AST

module Printer =

    let operatorToStr op =
        match op with
        | ArithmeticOp op ->
            match op with
            | Add -> "+"
            | Sub -> "-"
        | LogicalOp op ->
            match op with
            | And -> "&&"
            | Or -> "||"
        | RelationalOp op ->
            match op with
            | Greater -> ">"
            | Equal -> "=="

    let rec expressionToStr (exp: Expression) =
        match exp with
        | Constant c -> c |> Value.toStr
        | Binary (b) ->
            sprintf
                "%s %s %s"
                (b.LeftOperand |> expressionToStr)
                (b.BinaryOp |> operatorToStr)
                (b.RightOperand |> expressionToStr)
        | Unary (_, _) -> failwith "Not Implemented"
        | Assignment (_, _) -> failwith "Not Implemented"
        | FunCall fc ->
            sprintf
                "%s(%s)"
                (fc.Name |> Identifier.toStr)
                (fc.ActualParameters
                 |> (List.map expressionToStr)
                 |> List.reduce (fun acc s -> acc + "," + s))
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
                "{\n%s\n}"
                (block.Content
                 |> (List.map scopedStmtToStr)
                 |> List.reduce (fun acc s -> sprintf "%s\n%s" acc s))
        | IfStatement is ->
            sprintf
                "if %s \n\t%s\nelse\n\t%s"
                (is.Condition |> expressionToStr)
                (is.OnTrue |> scopedStmtToStr)
                (is.OnFalse |> scopedStmtToStr)

    let rec stmtToStr (stmt: Statement) =
        match stmt with
        | FunDeclaration fd ->
            sprintf
                "%s (%s) \n%s"
                (fd.Name |> Identifier.toStr)
                (fd.Parameters
                 |> List.map Identifier.toStr
                 |> List.reduce (fun a b -> sprintf "%s, %s" a b))
                (fd.Body
                 |> BlockStatement
                 |> ScopedStatement
                 |> stmtToStr)
        | ScopedStatement stmt -> scopedStmtToStr stmt

    let toStr (Program statementsList) =
        statementsList
        |> List.map stmtToStr
        |> List.reduce (fun acc s -> sprintf "%s\n%s" acc s)
