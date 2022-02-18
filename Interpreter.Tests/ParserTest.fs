module ParserTests

open Xunit
open Interpreter.AST
open FsCheck
open FsCheck.Xunit
open FSharpPlus
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
                var z = [{value}];
                var v = [z];
                v[0][0];
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

        let expected = value.ToString().[0].ToString() |> StringValue |> Ok

        actual .=. expected
