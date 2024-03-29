﻿namespace Interpreter.AST

open FSharpPlus

type VarDeclaration =
    { Name: Identifier
      InitExpression: Expression option }

type ForInitializer =
    | ExpressionInit of Expression
    | VarDeclarationInit of VarDeclaration

type ScopedStatement =
    | ExpressionStatement of Expression
    | VarDeclarationStatement of VarDeclaration
    | BlockStatement of Block
    | IfStatement of If
    | WhileStatement of While
    | ForStatement of For
    | ReturnStatement of Expression
    | Empty

and While =
    { Condition: Expression
      Body: ScopedStatement }

and For =
    { Initializer: ForInitializer
      Condition: Expression
      Increment: Expression
      Body: ScopedStatement }

and If =
    { Condition: Expression
      OnTrue: ScopedStatement
      OnFalse: ScopedStatement option }

and Block =
    { Content: ScopedStatement list }
    static member Create content = { Content = content }

    static member FromExpressions expressions =
        expressions
        |> (List.map ExpressionStatement)
        |> Block.Create

type Function =
    { Name: Identifier
      Parameters: Identifier list
      Body: Block }

type CompiledFunction =
    { Name: Identifier
      Execute: Value list -> Result<Value, RuntimeError> }

type Callable =
    | Function of Function
    | CompiledFunction of CompiledFunction
    member this.Name =
        match this with
        | Function f -> f.Name
        | CompiledFunction c -> c.Name

    static member FromFunction name f =
        { Name = name; Execute = f } |> CompiledFunction

type Member = Field of VarDeclaration
//  | NestedType of UserType

and Struct = { Members: Map<Identifier, Member> }

and UserTypeKind = Struct of Struct

and UserType =
    { Name: Identifier
      Kind: UserTypeKind }

type Statement =
    | FunDeclaration of Function
    | UserTypeDeclaration of UserType
    | ScopedStatement of ScopedStatement
    static member FromExpression(exp: Expression) =
        exp |> ExpressionStatement |> ScopedStatement

type StatementWithInfo =
    { Statement: Statement
      Info: StatementPosition }

type Program =
    | Statements of Statement list
    // | StatementsWithInfo of StatementWithInfo list
    static member Of statements = statements |> Statements

module Statement =
    let structDeclare name (members: Map<string, Expression option>) =
        { Name = name |> Identifier
          Kind =
            { Members =
                members
                |> Map.toList
                |> List.map (fun (k, v) ->
                    (k |> Identifier.create,
                     { Name = k |> Identifier.create
                       InitExpression = v }
                     |> Field))
                |> Map.ofList }
            |> Struct }
        |> UserTypeDeclaration

    let varDeclare name initExp =
        { Name = name |> Identifier.create
          InitExpression = initExp |> Some }
        |> VarDeclarationStatement
        |> ScopedStatement

    let block content =
        (Block.Create content)
        |> BlockStatement
        |> ScopedStatement

    let ifStmt condition onTrue onFalse =
        { If.Condition = condition
          OnTrue = onTrue
          OnFalse = onFalse }
        |> IfStatement
        |> ScopedStatement

    let funDeclare name parameters body =
        { Function.Name = name |> Identifier.create
          Parameters = parameters |> List.map Identifier.create
          Body = body }
        |> FunDeclaration
