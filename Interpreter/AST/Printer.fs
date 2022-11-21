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
        | Binary b ->
            sprintf
                "%s %s %s"
                (b.LeftOperand |> expressionToStr)
                (b.BinaryOp |> operatorToStr)
                (b.RightOperand |> expressionToStr)
        | SimpleUnary _ -> failwith "Not Implemented"
        | Assignment _ -> failwith "Not Implemented"
        | FunCall fc ->
            sprintf
                "%s(%s)"
                (fc.Name |> Identifier.asString)
                (fc.ActualParameters
                 |> (List.map expressionToStr)
                 |> List.reduce (fun acc s -> acc + "," + s))
        | Mutable me ->
            match me with
            | Var ident -> ident |> Identifier.asString


    let rec scopedStmtToStr (stmt: ScopedStatement) =
        let initializerToStr initializer =
            match initializer with
            | ExpressionInit ei -> ei |> expressionToStr
            | VarDeclarationInit vd -> vd |> VarDeclarationStatement |> scopedStmtToStr

        match stmt with
        | ExpressionStatement e -> expressionToStr e
        | VarDeclarationStatement vd ->
            sprintf
                "%s = %s"
                (vd.Name |> Identifier.asString)
                (vd.InitExpression |> Option.defaultValue (Expression.voidConstant())
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
                (is.OnFalse |> Option.defaultValue Empty |> scopedStmtToStr)
        | WhileStatement ws -> sprintf "while %s\n\t%s" (ws.Condition |> expressionToStr) (ws.Body |> scopedStmtToStr)
        | ForStatement fs ->
            sprintf
                "for %s %s %s\n\t%s"
                (fs.Initializer |> initializerToStr)
                (fs.Condition |> expressionToStr)
                (fs.Increment |> expressionToStr)
                (fs.Body |> scopedStmtToStr)
        | Empty -> ";"

    let rec stmtToStr (stmt: Statement) =
        match stmt with
        | FunDeclaration fd ->
            sprintf
                "%s (%s) \n%s"
                (fd.Name |> Identifier.asString)
                (fd.Parameters
                 |> List.map Identifier.asString
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
