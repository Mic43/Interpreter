module Interpreter.Tests.Infrastructure.ExpressionHelper

open Interpreter.AST
open FsCheck

let (.=.) left right =
    left = right |@ sprintf "%A = %A" left right

let outProgram program =
    sprintf "\n%s" (program |> Printer.toStr)

let idFuncProgram inputVal =
    let varIdent = Identifier.create "x"
    let funIdent = Identifier.create "id"
    // let inputVal = input |> IntValue
    let actualParam = inputVal |> Constant

    let idFunDecl =
        { Name = funIdent
          Parameters = [ varIdent ]
          Body =
            [ varIdent |> Var |> Mutable |> ReturnStatement ]
            |> Block.Create }

    [ idFunDecl |> FunDeclaration
      { Name = funIdent
        ActualParameters = [ actualParam ] }
      |> FunCall
      |> ExpressionStatement
      |> ScopedStatement ]
    |> Statements

let blockize maxLevel statements =
    let rec blockizeRec maxLevel curLevel statements =

        if curLevel = maxLevel then
            statements
        else
            statements
            |> Block.Create
            |> BlockStatement
            |> List.singleton
            |> (blockizeRec maxLevel (curLevel + 1))

    blockizeRec maxLevel 0 statements


