﻿namespace Interpreter.AST

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
      OnFalse: ScopedStatement }

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
