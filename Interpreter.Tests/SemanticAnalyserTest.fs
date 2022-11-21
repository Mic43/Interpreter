module Interpreter.Tests.SemanticAnalyserTest

open Xunit
open Interpreter.AST
open FsCheck.Xunit
open FSharpPlus
open FSharpPlus.Data
open ExpressionHelper
open Generators
open FsCheck
open Interpreter.Parser
open FParsec.CharParsers
open SemanticAnalysers


module ExpressionAnalyserTest =
    [<Property>]
    let ``Assigning to constant causes ExpressionMustBeLValue error`` (constantValue: int) =
        let p =
            (fun rightSide ->
                let constant = constantValue |> Expression.intConstant
                let exp =
                    (constant, rightSide)
                    |> Assignment

                let expAnalyser =
                    analyseExpression (Environment.createEmpty ())

                let expected =
                    [ constant |> ExpressionMustBeLValue ]

                let actual = expAnalyser exp

                actual .=. expected)

        Prop.forAll (Expressions.constantExpressionTree |> Arb.fromGen) p
