module Interpreter.Tests.SemanticAnalyserTest

open Xunit
open Interpreter.AST
open FsCheck.Xunit
open FSharpPlus
open FSharpPlus.Data
open ExpressionHelper
open Generators
open FsCheck
open FParsec.CharParsers
open SemanticAnalysers
open Interpreter.Tests.Infrastructure.ParserHelper

module ExpressionAnalyserTest =


    let runParser str =
        let executionEnvironment =
            Environment.createEmpty ()

        let expParser =
            Interpreter.Parser.Expression.pExpr ()

        str
        |> run expParser
        |> map (executionEnvironment |> analyseExpression)
        |> toOption


    [<Property>]
    let ``Assigning to constant causes ExpressionMustBeLValue error`` (constantValue: int) =
        let p =
            (fun rightSide ->
                let constant =
                    constantValue |> Expression.intConstant

                let exp =
                    (constant, rightSide) |> Assignment

                let expAnalyser =
                    analyseExpression (Environment.createEmpty ())

                let expected =
                    [ constant |> ExpressionMustBeLValue ]

                let actual = expAnalyser exp

                actual .=. expected)

        Prop.forAll (Expressions.constantExpressionTree |> Arb.fromGen) p

    [<Fact>]
    let ``Not defined variable 1`` () =

        let actual = "7+4 * x" |> runParser

        let expected =
            "x"
            |> VariableNotDefined
            |> List.singleton
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Not defined variable 2`` () =

        let actual = "[1,z]" |> runParser

        let expected =
            "z"
            |> VariableNotDefined
            |> List.singleton
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Not defined variable i nested lists`` () =

        let actual = "[1,[z]]" |> runParser

        let expected =
            "z"
            |> VariableNotDefined
            |> List.singleton
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Not defined variable 3`` () =

        let actual = "a = 10" |> runParser

        let expected =
            "a"
            |> VariableNotDefined
            |> List.singleton
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Not defined variable 4`` () =

        let actual = "abc++" |> runParser

        let expected =
            "abc"
            |> VariableNotDefined
            |> List.singleton
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Not defined variable 5`` () =

        let actual = "-e" |> runParser

        let expected =
            "e"
            |> VariableNotDefined
            |> List.singleton
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Not defined variable 6`` () =

        let actual = "(7 > a)*(2+12)" |> runParser

        let expected =
            "a"
            |> VariableNotDefined
            |> List.singleton
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Not defined list access`` () =

        let actual = "12 + a[0]" |> runParser

        let expected =
            "a"
            |> VariableNotDefined
            |> List.singleton
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``multiple not defined variables`` () =

        let actual =
            "y * x" |> runParser |> Option.map Set.ofList

        let expected =
            [ "x"; "y" ]
            |> Set.ofList
            |> Set.map VariableNotDefined
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``multiple not defined variables 2`` () =

        let actual =
            "z[x]++" |> runParser |> Option.map Set.ofList

        let expected =
            [ "z"; "x" ]
            |> Set.ofList
            |> Set.map VariableNotDefined
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``multiple not defined variables 3`` () =

        let actual =
            "abc = cba+5*-z"
            |> runParser
            |> Option.map Set.ofList

        let expected =
            [ "abc"; "cba"; "z" ]
            |> Set.ofList
            |> Set.map VariableNotDefined
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``function not defined test`` () =

        let actual =
            "5*foo(5) + 17"
            |> runParser
            |> Option.map Set.ofList

        let expected =
            [ "foo" ]
            |> Set.ofList
            |> Set.map FunctionNotDefined
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``assignment to simple constant expression causes ExpressionMustBeLValue error`` () =

        let actual = "1 + 1 = 2" |> runParser

        let expected =
            [ Expression.add (1 |> Expression.intConstant) (1 |> Expression.intConstant)
              |> ExpressionMustBeLValue ]
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``assignment to simple constant and not defined variable test`` () =

        let actual = "9 * 1 = 2*a + 5" |> runParser |> Option.map Set.ofList

        let expected =
            [ Expression.mul (9 |> Expression.intConstant) (1 |> Expression.intConstant)
              |> ExpressionMustBeLValue;"a" |> VariableNotDefined ]
             |> Set.ofList |> Some

        Assert.Equal(expected, actual)
        
    [<Fact>]
    let ``simple constant expression not causes any errors`` () =

        let actual = "9 * 1 == 9" |> runParser 

        let expected =
            List.Empty |> Some

        Assert.Equal(expected, actual)
        
    [<Fact>]
    let ``simple list constant expression not causes any errors`` () =

        let actual = "[9]" |> runParser 

        let expected =
            List.Empty |> Some

        Assert.Equal(expected, actual)