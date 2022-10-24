module ExpSimplifierTest

open Xunit
open Interpreter.AST
open FsCheck.Xunit
open FSharpPlus
open FSharpPlus.Data
open ExpressionHelper
open Generators
open FsCheck

module Binary =
    let simplify =
        ExpSimplifier.simplifyNode
        |> ExpSimplifier.simplify

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
               Expression.mul (1.0 |> Expression.floatConstant) valueExp

           let actual = exp |> simplify

           match actual with
           | Constant v when v.IsZero() -> true
           | _ -> false)
       |> Prop.forAll (Values.numericValues |> Arb.fromGen)