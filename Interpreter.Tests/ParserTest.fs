module ParserTests

open Xunit
open Interpreter.AST
open FsCheck
open FsCheck.Xunit
open FSharpPlus
open FSharpPlus.Data
open Interpreter.AST


let (.=.) left right =
    left = right |@ sprintf "%A = %A" left right

module Run =
    [<Property>]
    let ``unindexed variables are read correctly`` (value: int) =
        let str =
            $"
            var z = {value};
            z;
        "

        let actual = Interpreter.Runner.run str
        let expected = value |> IntValue |> Ok
        actual = expected

    [<Property>]
    let ``single indexed arrays are read correctly`` (value: int) =
        let str =
            $"
            var z = [{value}];
            z[0];
        "

        let actual = Interpreter.Runner.run str
        let expected = value |> IntValue |> Ok
        actual = expected

    [<Property>]
    let ``strings are read correctly by index`` (value: PositiveInt) =
        let str =
            $"
            var z = \"{value}\";
            z[0];
        "

        let actual = Interpreter.Runner.run str

        let expected =
            value.ToString().[0].ToString()
            |> StringValue
            |> Ok

        actual .=. expected

    [<Property>]
    let ``arrays of arrays are read correctly by index`` (value: int) =
        let str =
            $"
                var z = [1,{value}];
                var v = [[9],z];
                v[1][1];
            "

        let actual = Interpreter.Runner.run str

        let expected = value |> IntValue |> Ok

        actual .=. expected

    [<Property>]
    let ``arrays of strings are read correctly by index`` (value: PositiveInt) =
        let str =
            $"
                var v = [\"{value}\"];
                v[0][0];
            "

        let actual = Interpreter.Runner.run str

        let expected =
            value.ToString().[0].ToString()
            |> StringValue
            |> Ok

        actual .=. expected

    [<Property>]
    let ``arrays of arays initialization works correctly`` (value: PositiveInt) =
        let str =
            $"
                var v = [[{value}]];
                v[0][0];
            "

        let actual = Interpreter.Runner.run str

        let expected = value.Get |> IntValue |> Ok

        actual .=. expected

    [<Property>]
    let ``simple variable assignment works correctly`` (before: int) (after: int) =
        let str =
            $"
                var v = {before};
                v = {after};
                v;
            "

        let actual = Interpreter.Runner.run str

        let expected = after |> IntValue |> Ok

        actual .=. expected

    [<Property>]
    let ``simple 1D array assignment by index works correctly`` (after: int) =

        let inBounds = Gen.elements [ 0 .. 2 ] |> Arb.fromGen

        Prop.forAll inBounds (fun index ->
            let str =
                $"
                var v = [1,2,3];
                v[{index}] = {after};
                v[{index}];
            "

            let actual = Interpreter.Runner.run str

            let expected = after |> IntValue |> Ok

            actual .=. expected)
    [<Property>]
    let ``simple 2D array assignment by index works correctly`` (after: int) =   
        let gen = Gen.elements [ 0 .. 2 ]

        gen
        |> Gen.apply (gen |> Gen.map (fun a b -> (a, b)))
        |> Arb.fromGen
        |> Prop.forAll
        <| (fun (i1, i2) ->
            let str =
                $"
                var v = [[1,2,3],[1,2,3],[1,2,3]];
                v[{i1}][{i2}] = {after};
                v[{i1}][{i2}];
            "

            let actual = Interpreter.Runner.run str

            let expected = after |> IntValue |> Ok

            actual .=. expected)
   