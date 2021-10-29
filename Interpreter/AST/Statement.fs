namespace Interpreter.AST

open System
open FSharpPlus

type VarDeclaration =
    { Name: Identifier
      InitExpression: Expression }

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
      Execute: (Value list -> Result<Value, RunError>) }

type Callable =
    | Function of Function
    | CompiledFunction of CompiledFunction
    member this.Name =
        match this with
        | Function f -> f.Name
        | CompiledFunction c -> c.Name

    static member FromFunction name f =
        { Name = name; Execute = f } |> CompiledFunction

type Statement =
    | FunDeclaration of Function
    | ScopedStatement of ScopedStatement
    static member FromExpression(exp: Expression) =
        exp |> ExpressionStatement |> ScopedStatement

type Program = Program of Statement list
    with static member Of statements = statements |> Program

module Statement =
    let varDeclare name initExp =
        { Name = name |> Identifier.create
          InitExpression = initExp }
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

    let funDeclare name paramters body =
        { Function.Name = name |> Identifier.create
          Parameters = paramters |> List.map Identifier.create
          Body = body }
        |> FunDeclaration
