module ParserTests

open Xunit
open Interpreter.AST
open Interpreter.AST.Errors
open FsCheck
open FsCheck.Xunit
open FSharpPlus
open FSharpPlus.Data

let (.=.) left right =
    left = right |@ sprintf "%A = %A" left right

module Variables =
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
        <| fun (i1, i2) ->
            let str =
                $"
                var v = [[1,2,3],[1,2,3],[1,2,3]];
                v[{i1}][{i2}] = {after};
                v[{i1}][{i2}];
            "

            let actual = Interpreter.Runner.run str

            let expected = after |> IntValue |> Ok

            actual .=. expected

    [<Property>]
    let ``mutliple assignment works correctly for ints`` (before: int) (after: int) =
        let str =
            $"
            var z = {before};
            var v = z = {after};
            [z,v];
        "

        let actual = Interpreter.Runner.run str

        let expected =
            [ after |> IntValue; after |> IntValue ]
            |> List.map (fun v -> ref v)
            |> ListValue
            |> Ok

        actual .=. expected

    [<Property>]
    let ``preincremenation of array element works correctly`` (before: int) =
        let str =
            $"
            var z = [5,{before}];
            [++z[1],z[1]];
        "

        let actual = Interpreter.Runner.run str
        let after = before + 1

        let expected =
            [ after |> IntValue; after |> IntValue ]
            |> List.map (fun v -> ref v)
            |> ListValue
            |> Ok

        actual .=. expected

    [<Property>]
    let ``postincremenation of array element works correctly`` (before: int) =
        let str =
            $"
            var z = [{before}];
            [z[0]++,z[0]];
        "

        let actual = Interpreter.Runner.run str
        let after = before + 1

        let expected =
            [ before |> IntValue
              after |> IntValue ]
            |> List.map (fun v -> ref v)
            |> ListValue
            |> Ok

        actual .=. expected

    [<Property>]
    let ``uninitialized variable is set to void`` () =
        let str =
            $"
            var z;
            z;
        "

        let actual = Interpreter.Runner.run str

        let expected = Value.Void |> Ok

        actual .=. expected

    [<Property>]
    let ``accessing array with index equal to array len causes error`` (l: NonEmptyList<int>) =
        let listStr =
            l
            |> NonEmptyList.map string
            |> NonEmptyList.reduce (fun s v -> $"{s},{v}")

        let str =
            $"
            var z = [{listStr}];
            z[ {l.Length} ];
        "

        let actual = Interpreter.Runner.run str

        match actual with
        | Ok (_) -> false
        | Error (re) ->
            match re with
            | Interpreter.RunError _ -> true
            | _ -> false

module Structs =
    [<Property>]
    let ``declaration of empty struct is poosible`` () =
        let str =
            $"
                struct aaa1 {{   }}
            "

        let actual = Interpreter.Runner.run str
        let expected = Value.Void |> Ok

        actual .=. expected

    [<Property>]
    let ``declaration of one member struct without initializer is poosible`` () =
        let str =
            $"
                struct ssss2 {{ var inner;  }}
            "

        let actual = Interpreter.Runner.run str
        let expected = Value.Void |> Ok

        actual .=. expected

    [<Property>]
    let ``declaration of one member struct with simple initializer is poosible`` () =
        let str =
            $"
                struct ssss4 {{ var inner = 5;  }}
            "

        let actual = Interpreter.Runner.run str
        let expected = Value.Void |> Ok

        actual .=. expected

    [<Property>]
    let ``declaration of one member struct with  initializer referring to global var is poosible`` () =
        let str =
            $"
                var global = 10;
                struct ssss4 {{ var inner = global;  }}
            "

        let actual = Interpreter.Runner.run str
        let expected = Value.Void |> Ok

        actual .=. expected

    [<Property>]
    let ``declaration of struct with many members is posible`` () =
        let str =
            $"
                struct ssss4 {{
                     var inner ;
                     var iner2 = 5;
                     var iner3 = 11.4;
                     var inner4 = 5;
                }}
            "

        let actual = Interpreter.Runner.run str
        let expected = Value.Void |> Ok

        actual .=. expected

    [<Property>]
    let ``declaration of struct with many members with initializers is posible`` () =
        let str =
            $"
                var z = 12;
                struct ssss4 {{
                        var inner ;
                        var iner2 = 5;
                        var iner3 = 11.4;
                        var inner4 = z * z;
                }}
            "

        let actual = Interpreter.Runner.run str
        let expected = Value.Void |> Ok

        actual .=. expected

    [<Property>]
    let ``declaration of list member struct is possible`` () =
        let str =
            $"
                struct ssss4 {{
                    var inner = [3];
                }}
            "

        let actual = Interpreter.Runner.run str
        let expected = Value.Void |> Ok

        actual .=. expected

    [<Property>]
    let ``declaration of two unique named empty structs is possible`` () =
        let str =
            $"
                struct ssss4 {{
                }}
                struct ssss5 {{
                }}
            "

        let actual = Interpreter.Runner.run str
        let expected = Value.Void |> Ok

        actual .=. expected

    [<Property>]
    let ``instantiate of empty struct is possible`` () =
        let strName = "ss"

        let str =
            $"
                struct {strName} {{
                }}
                var z =  {strName}{{}};
                z;
            "

        let actual = Interpreter.Runner.run str

        let expected =
            Value.createEmptyStructInstance strName |> Ok

        actual .=. expected

    [<Property>]
    let ``instantiation struct without initialization list and field defaults is possible`` () =
        let strName = "aaa"

        let fields =
            [ "inner"
              "inner2"
              "inner3"
              "inner4" ]

        let str =
            $"
                struct {strName} {{
                        var {fields.[0]} ;
                        var {fields.[1]};
                        var {fields.[2]};
                        var {fields.[3]};
                }}
                var z =  {strName}{{}};
                z;
            "

        let actual = Interpreter.Runner.run str

        let expected =
            strName
            |> Value.createDefaultStructInstance fields
            |> Ok

        actual .=. expected

    [<Property>]
    let ``instantiation struct without initialization list is possible`` () =
        let strName = "aaa"

        let fields =
            [ "inner"
              "inner2"
              "inner3"
              "inner4" ]

        let fieldValues = [ 0; 5; 6; 4 ]

        let str =
            $"
                struct {strName} {{
                        var {fields.[0]}  = {fieldValues.[0]};
                        var {fields.[1]} = {fieldValues.[1]};
                        var {fields.[2]} = {fieldValues.[2]};
                        var {fields.[3]} = {fieldValues.[3]};
                }}
                var z =  {strName}{{}};
                z;
            "

        let actual = Interpreter.Runner.run str

        let expected =
            strName
            |> Value.createStructInstance (
                (fields |> List.map Identifier.create, fieldValues |> List.map (IntValue >> ref))
                ||> List.zip
                |> Map.ofList
            )
            |> Ok

        actual .=. expected

    [<Property>]
    let ``initialization list overrides default field values`` (oldValue: int) (newValue: int) =
        let strName = "aaa"
        let fieldName = "inner"

        let str =
            $"
                struct {strName} {{
                        var {fieldName}  = {oldValue};
                }}
                var z =  {strName}{{ {fieldName} = {newValue}}};
                z;
            "

        let actual = Interpreter.Runner.run str

        let expected =
            strName
            |> Value.createStructInstance (
                [ fieldName |> Identifier.create, newValue |> IntValue |> ref ]
                |> Map.ofList
            )
            |> Ok

        actual .=. expected

    [<Property>]
    let ``default field values are overriden only by values specified in initilization list``
        (oldValue: int)
        (newValue: int)
        (notChangedValue: int)
        =
        let strName = "aaa"
        let fieldName = "inner"
        let fieldNotChangedName = "notChanged"

        let str =
            $"
                struct {strName} {{
                        var {fieldName}  = {oldValue};
                        var {fieldNotChangedName}  = {notChangedValue};

                }}
                var z =  {strName}{{ {fieldName} = {newValue}}};
                z;
            "

        let actual = Interpreter.Runner.run str

        let expected =
            strName
            |> Value.createStructInstance (
                [ fieldName |> Identifier.create, newValue |> IntValue |> ref
                  fieldNotChangedName |> Identifier.create, notChangedValue |> IntValue |> ref ]
                |> Map.ofList
            )
            |> Ok

        actual .=. expected
