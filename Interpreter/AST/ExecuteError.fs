namespace Interpreter.AST

type ErrorType =        
    | ParseError
    | SemanticError
    | RuntimeError

type ExecuteError = { Message: string; Type: ErrorType }

module ExecuteError =    
    let create errorType str = { Message = str; Type = errorType } 
    let createResult errorType str = (create errorType str) |> Error