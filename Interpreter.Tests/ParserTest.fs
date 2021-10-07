module ParserTests

open Xunit
open Interpreter.AST
open FsCheck
open FsCheck.Xunit
open FSharpPlus
open Interpreter.AST
open Interpreter.AST


let (.=.) left right =
    left = right |@ sprintf "%A = %A" left right

