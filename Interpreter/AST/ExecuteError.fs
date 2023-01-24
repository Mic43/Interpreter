namespace Interpreter.AST

open FParsec


type AnalyserResult =
    | MisplacedReturnStatement
    | VariableAlreadyDefined of string
    | VariableNotDefined of string
    | FunctionNotDefined of string
    | FunctionAlreadyDefined of string
    | ExpressionMustBeLValue of string


type RuntimeError = {Message: string}

type StatementPosition = { Line: int64; Column: int64 }

type ErrorType =
    | ParseError
    | SemanticError of AnalyserResult
    | RuntimeError

type ExecuteError =
    { Message: string
      Type: ErrorType
      Position: StatementPosition }

module ExecuteError =
    //  let create errorType str = { Message = str; Type = errorType }
    //let createResult errorType str = (create errorType str) |> Error
    let create message errorType pos =
        { ExecuteError.Message = message
          ExecuteError.Position = pos
          ExecuteError.Type = errorType }

    let createRuntimeError str = {RuntimeError.Message = str}
    let createRuntimeErrorResult str = str |> createRuntimeError|> Result.Error
