module Interpreter.Tests.SemanticAnalyserTest

open Interpreter.Tests.Infrastructure
open Xunit
open Interpreter.AST
open FsCheck.Xunit
open FSharpPlus
open FSharpPlus.Data

open FsCheck
open FParsec.CharParsers
open SemanticAnalyser

open Interpreter.Tests.Infrastructure.Generators
open Interpreter.Tests.Infrastructure.ExpressionHelper
open Interpreter.Tests.Infrastructure.ParserHelper



module ExpressionAnalyserTest =
    let runAnalyser str =
        let executionEnvironment =
            Environment.createEmpty ()

        let expParser =
            Interpreter.Parser.Expression.pExpr ()

        str
        |> run expParser
        |> map (executionEnvironment |> analyseExpression)
        |> toOption


    [<Property>]
    let ``Assigning to constant expression causes ExpressionMustBeLValue error`` (constantValue: int) =
        let p =
            (fun (leftSide, rightSide) ->
                let exp =
                    (leftSide, rightSide) |> Assignment

                let expAnalyser =
                    analyseExpression (Environment.createEmpty ())

                let expected =
                    [ leftSide
                      |> Printer.expressionToStr
                      |> ExpressionMustBeLValue ]

                let actual = expAnalyser exp

                actual .=. expected)

        Prop.forAll
            (Expressions.constantExpressionTree
             |> Gen.two
             |> Arb.fromGen)
            p

    [<Fact>]
    let ``Not defined variable 1`` () =

        let actual = "7+4 * x" |> runAnalyser

        let expected =
            "x"
            |> VariableNotDefined
            |> List.singleton
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Not defined variable 2`` () =

        let actual = "[1,z]" |> runAnalyser

        let expected =
            "z"
            |> VariableNotDefined
            |> List.singleton
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Not defined variable i nested lists`` () =

        let actual = "[1,[z]]" |> runAnalyser

        let expected =
            "z"
            |> VariableNotDefined
            |> List.singleton
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Not defined variable 3`` () =

        let actual = "a = 10" |> runAnalyser

        let expected =
            "a"
            |> VariableNotDefined
            |> List.singleton
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Not defined variable 4`` () =

        let actual = "abc++" |> runAnalyser

        let expected =
            "abc"
            |> VariableNotDefined
            |> List.singleton
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Not defined variable 5`` () =

        let actual = "-e" |> runAnalyser

        let expected =
            "e"
            |> VariableNotDefined
            |> List.singleton
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Not defined variable 6`` () =

        let actual = "(7 > a)*(2+12)" |> runAnalyser

        let expected =
            "a"
            |> VariableNotDefined
            |> List.singleton
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Not defined list access`` () =

        let actual = "12 + a[0]" |> runAnalyser

        let expected =
            "a"
            |> VariableNotDefined
            |> List.singleton
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``multiple not defined variables`` () =

        let actual =
            "y * x" |> runAnalyser |> Option.map Set.ofList

        let expected =
            [ "x"; "y" ]
            |> Set.ofList
            |> Set.map VariableNotDefined
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``multiple not defined variables 2`` () =

        let actual =
            "z[x]++" |> runAnalyser |> Option.map Set.ofList

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
            |> runAnalyser
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
            |> runAnalyser
            |> Option.map Set.ofList

        let expected =
            [ "foo" ]
            |> Set.ofList
            |> Set.map FunctionNotDefined
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``assignment to simple constant expression causes ExpressionMustBeLValue error`` () =

        let actual = "1 + 1 = 2" |> runAnalyser

        let expected =
            [ Expression.add (1 |> Expression.intConstant) (1 |> Expression.intConstant)
              |> Printer.expressionToStr
              |> ExpressionMustBeLValue ]
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``assignment to simple constant and not defined variable test`` () =

        let actual =
            "9 * 1 = 2*a + 5"
            |> runAnalyser
            |> Option.map Set.ofList

        let expected =
            [ Expression.mul (9 |> Expression.intConstant) (1 |> Expression.intConstant)
              |> Printer.expressionToStr
              |> ExpressionMustBeLValue
              "a" |> VariableNotDefined ]
            |> Set.ofList
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``simple constant expression not causes any errors`` () =

        let actual = "9 * 1 == 9" |> runAnalyser

        let expected = List.Empty |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``simple list constant expression not causes any errors`` () =

        let actual = "[9]" |> runAnalyser

        let expected = List.Empty |> Some

        Assert.Equal(expected, actual)

module StatementAnalyserTest =

    let runAnalyser str =
        let executionEnvironment =
            Environment.createEmpty ()

        let pProgram =
            Interpreter.Parser.Statement.pProgram

        str
        |> run pProgram
        |> map (executionEnvironment |> analyseProgram)
        |> toOption

    [<Fact>]
    let ``single variable declaration works correctly`` () =

        let actual = "var x = 5;" |> runAnalyser

        let expected = List.empty |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``declaring same variable twice causes error`` () =

        let actual =
            "var x = 5; var x = 4;" |> runAnalyser

        let expected =
            [ "x" |> VariableAlreadyDefined ] |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``declaring same variable twice inside function causes error`` () =

        let actual =
            "fun foo() { var a = 5; var a = 4;}"
            |> runAnalyser

        let expected =
            [ "a" |> VariableAlreadyDefined ] |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``shadowing function parameter not causes duplicated variable error`` () =

        let actual =
            "fun foo(a) {var a = 5;}" |> runAnalyser

        let expected = List.empty |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``declaring variable twice inside false if branch causes error`` () =

        let actual =
            """
            var x = 1;
            if (x==2)
            {
                var ccc = 5;
                var ccc = 6;
            }
            """
            |> runAnalyser

        let expected =
            [ "ccc" |> VariableAlreadyDefined ] |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``calling not existing function causes error`` () =

        let actual =
            """
            foo(3);
            """
            |> runAnalyser

        let expected =
            [ "foo" |> FunctionNotDefined ] |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``calling not existing function from inside of other function causes error`` () =

        let actual =
            """
            fun foo(a)
            {
                boo(a);
            }
            
            """
            |> runAnalyser

        let expected =
            [ "boo" |> FunctionNotDefined ] |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``referring to not existing variable causes error`` () =

        let actual =
            """
            var k = 10;
            k = z;
            
            """
            |> runAnalyser

        let expected =
            [ "z" |> VariableNotDefined ] |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``referring to not existing variable inside function causes error`` () =

        let actual =
            """
            fun foo(a)
            {                
                var z = a;
                z = b;
            }
            
            """
            |> runAnalyser

        let expected =
            [ "b" |> VariableNotDefined ] |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``referring to not existing variable as a function actual parameter causes error`` () =

        let actual =
            """
            fun id(a)
            {                
                return a;
            }
            id(5 * x);
            """
            |> runAnalyser

        let expected =
            [ "x" |> VariableNotDefined ] |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``defining two functions with the same name causes error`` () =

        let actual =
            """
            fun id(a)
            {                
                return a;
            }
            fun id(a,b)
            {                
                return a + 1;
            }            
            """
            |> runAnalyser

        let expected =
            [ "id" |> FunctionAlreadyDefined ] |> Some

        Assert.Equal(expected, actual)

    [<Property>]
    let ``return statement in the global scope causes error`` () =

        (fun exp ->
            let actual =
                $"
                fun id(a)
                {{                
                    return a;
                }}              
                return {exp |> Printer.expressionToStr};
                "
                |> runAnalyser

            let expected =
                [ MisplacedReturnStatement ] |> Some

            expected .=. actual)
        |> Prop.forAll (
            Expressions.constantExpressionTree
            |> Arb.fromGen
        )
