namespace Interpreter.AST


type AnalyserResult =
    | MisplacedReturnStatement
    | VariableAlreadyDefined of string
    | VariableNotDefined of string
    | FunctionNotDefined of string
    | FunctionAlreadyDefined of string
    | ExpressionMustBeLValue of string
    
type StatementPosition = { Line: int64; Column: int64 }
type ErrorType =        
    | ParseError
    | SemanticError of (AnalyserResult * StatementPosition)
    | RuntimeError

type ExecuteError = { Message: string; Type: ErrorType }

module ExecuteError =    
    let create errorType str = { Message = str; Type = errorType } 
    let createResult errorType str = (create errorType str) |> Error