namespace Interpreter.AST

type ErrorType =
    | Evaluation
    | Other

type RunError = { Message: string; Type: ErrorType }

module Errors =    
    let create errorType str = { Message = str; Type = errorType }
    let createResult errorType str = (create errorType str) |> Result.Error