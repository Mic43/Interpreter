﻿namespace Interpreter.AST


type AnalyserResult =
    | MisplacedReturnStatement
    | VariableAlreadyDefined of string
    | VariableNotDefined of string
    | FunctionNotDefined of string
    | FunctionAlreadyDefined of string
    | ExpressionMustBeLValue of string
type ErrorType =        
    | ParseError
    | SemanticError of AnalyserResult
    | RuntimeError

type ExecuteError = { Message: string; Type: ErrorType }

module ExecuteError =    
    let create errorType str = { Message = str; Type = errorType } 
    let createResult errorType str = (create errorType str) |> Error