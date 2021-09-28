namespace Interpreter.AST

open System
open FSharpPlus


type VarDeclaration =
    { Name: Identifier
      Initializer: Expression }

type ScopedStatement =
    | ExpressionStatement of Expression
    | VarDeclaration of VarDeclaration
    | BlockStatement of Block
    | Empty

and Block =
    { Content: ScopedStatement list }
    static member Create content = { Content = content }

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

type Program = Program of Statement list
