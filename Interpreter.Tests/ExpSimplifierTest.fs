module Interpreter.Tests.ExpSimplifierTest

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
open Interpreter.Tests.Infrastructure.ParserHelper

let constExpEvaluator exp =
    ExpEvaluator.tryEvaluate
        (fun ident -> failwith "eval")
        ExpEvaluator.constEvaluator
        ExpEvaluator.binaryOpEvaluator
        ExpEvaluator.unaryOpEvaluator
        (fun a b -> failwith "eval")
        (fun a -> failwith "eval")
        exp

let simplify =
    ExpSimplifier.simplifyNode
    |> ExpSimplifier.simplify


module Binary =
    [<Property>]
    let ``Left zero addition simplifies correctly`` () =
        (fun value ->
            let valueExp = (value |> Constant)

            let exp =
                Expression.add valueExp (0 |> Expression.intConstant)

            let actual = exp |> simplify
            let expected = valueExp

            actual .=. expected)
        |> Prop.forAll (Values.numericValues |> Arb.fromGen)

    [<Property>]
    let ``Right zero addition simplifies correctly`` () =
        (fun value ->
            let valueExp = (value |> Constant)

            let exp =
                Expression.add (0 |> Expression.intConstant) valueExp

            let actual = exp |> simplify
            let expected = valueExp

            actual .=. expected)
        |> Prop.forAll (Values.numericValues |> Arb.fromGen)

    [<Property>]
    let ``Zero subtraction simplifies correctly`` () =
        (fun value ->
            let valueExp = (value |> Constant)

            let exp =
                Expression.sub valueExp (0 |> Expression.intConstant)

            let actual = exp |> simplify
            let expected = valueExp

            actual .=. expected)
        |> Prop.forAll (Values.numericValues |> Arb.fromGen)

    [<Property>]
    let ``Mul by right zero simplifies correctly`` () =
        (fun value ->
            let valueExp = (value |> Constant)

            let exp =
                Expression.mul valueExp (0 |> Expression.intConstant)

            let actual = exp |> simplify

            match actual with
            | Constant v when v.IsZero() -> true
            | _ -> false)
        |> Prop.forAll (Values.numericValues |> Arb.fromGen)

    [<Property>]
    let ``Mul by left zero simplifies correctly`` () =
        (fun value ->
            let valueExp = (value |> Constant)

            let exp =
                Expression.mul (0 |> Expression.intConstant) valueExp

            let actual = exp |> simplify

            match actual with
            | Constant v when v.IsZero() -> true
            | _ -> false)
        |> Prop.forAll (Values.numericValues |> Arb.fromGen)

    [<Property>]
    let ``Mul by one left does not change expression`` () =
        (fun value ->
            let valueExp = (value |> Constant)

            let exp =
                Expression.mul (1 |> Expression.intConstant) valueExp

            let actual = exp |> simplify
            let expected = valueExp
            actual .=. expected)
        |> Prop.forAll (Values.numericValues |> Arb.fromGen)

    [<Property>]
    let ``Mul by one right does not change expression`` () =
        (fun value ->
            let valueExp = (value |> Constant)

            let exp =
                Expression.mul valueExp (1 |> Expression.intConstant)

            let actual = exp |> simplify
            let expected = valueExp
            actual .=. expected)
        |> Prop.forAll (Values.numericValues |> Arb.fromGen)

module General =
    [<Property>]
    let ``Constant expression tree simplifies to its evaluated value`` () =

        (fun expression ->

            let actual = expression |> simplify |> Ok

            let expected =
                expression
                |> constExpEvaluator
                |> Result.map Constant

            match expected with
            | Ok (Constant (FloatValue v)) when System.Double.IsNaN(v) -> true |> Prop.ofTestable
            | _ -> actual .=. expected)
        |> Prop.forAll (Expressions.constantExpressionTree |> Arb.fromGen) 

module WithParsing =

    let expParser = Expression.pExpr ()

    let runParser str =
        str |> run expParser |> map simplify |> toOption

    [<Fact>]
    let ``simple simplification`` () =
        let actual = "(4 * 4) * x" |> runParser

        let expected =
            (16 |> Expression.intConstant, "x" |> Expression.var)
            ||> Expression.mul
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``simplify brackets`` () =
        let actual =
            "( x * 0 ) + ( y * 2 )" |> runParser

        let expected =
            ("y" |> Expression.var, 2 |> Expression.intConstant)
            ||> Expression.mul
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``simplify trailing 0`` () =
        let actual =
            "( a + 6 ) * 2 * 0" |> runParser

        let expected =
            0 |> Expression.intConstant |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``simplify leading 0`` () =
        let actual = "0 * ( x + y )" |> runParser

        let expected =
            0 |> Expression.intConstant |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``simplify advanced`` () =
        let actual =
            "( ( ( 23 * 2 ) + ( 2 * 9 ) ) + 34 + 4 ) * y"
            |> runParser

        let expected =
            (102 |> Expression.intConstant, "y" |> Expression.var)
            ||> Expression.mul
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``simplify subtract`` () =
        let actual =
            "( x + y + z ) - ( x + y + z )" |> runParser

        let expected =
            0 |> Expression.intConstant |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``simplify subtract nested`` () =
        let actual =
            "( x + y + z ) - ( ( x + y + z ) + 0 )"
            |> runParser

        let expected =
            0 |> Expression.intConstant |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``simplify nested`` () =
        let actual =
            "a + ( 0 + ( 0 + ( 0 + 0 ) ) )" |> runParser

        let expected = "a" |> Expression.var |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``simplify distributivity 1`` () =
        let actual =
            "( a * b ) + ( a * c )" |> runParser

        let expected =
            ("a" |> Expression.var,
             ("b" |> Expression.var, "c" |> Expression.var)
             ||> Expression.add)
            ||> Expression.mul
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``simplify distributivity 2`` () =
        let actual =
            "( b * ( 1 + x ) ) + ( ( 1 + x ) * c )"
            |> runParser

        let expected =
            ((1 |> Expression.intConstant, "x" |> Expression.var)
             ||> Expression.add,
             ("b" |> Expression.var, "c" |> Expression.var)
             ||> Expression.add)
            ||> Expression.mul
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``simplify distributivity 3`` () =
        let actual =
            "( ( x + 1 ) * 2 ) + ( 3 * ( x + 1 ) )"
            |> runParser

        let expected =
            (("x" |> Expression.var, 1 |> Expression.intConstant)
             ||> Expression.add,
             5 |> Expression.intConstant)
            ||> Expression.mul
            |> Some

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``simplify complex`` () =
        let actual =
            "( x - x ) + ( ( b + 0 ) * ( a + 0 ) + c * a ) - ( ( a * b ) + ( a * c ) + ( ( 12 + 7 ) * 0 ) )"
            |> runParser

        let expected =
            Expression.intConstant 0 |> Some

        Assert.Equal(expected, actual)
