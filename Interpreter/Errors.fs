namespace Interpretr.AST.Types

module Errors =
    type ErrorType =
        | Evaluation
        | Other

    type RunError = { Message: string; Type: ErrorType }

    let create errorType str = { Message = str; Type = errorType }
    let createResult errorType str = Result.Error(create str errorType)