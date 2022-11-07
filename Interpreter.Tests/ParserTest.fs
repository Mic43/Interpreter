module ParserTests

open System.Globalization
open Xunit
open Interpreter.AST
open Interpreter.AST.Errors
open FsCheck
open FsCheck.Xunit
open FSharpPlus
open FSharpPlus.Data
open ExpressionHelper

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
            | Interpreter.ExecuteError _ -> true
            | _ -> false

module Structs =
    [<Property>]
    let ``declaration of empty struct is possible`` () =
        let str =
            $"
                struct aaa1 {{   }}
            "

        let actual = Interpreter.Runner.run str
        let expected = Value.Void |> Ok

        actual .=. expected

    [<Property>]
    let ``declaration of one member struct without initializer is possible`` () =
        let str =
            $"
                struct ssss2 {{ var inner;  }}
            "

        let actual = Interpreter.Runner.run str
        let expected = Value.Void |> Ok

        actual .=. expected

    [<Property>]
    let ``declaration of one member struct with simple initializer is possible`` () =
        let str =
            $"
                struct ssss4 {{ var inner = 5;  }}
            "

        let actual = Interpreter.Runner.run str
        let expected = Value.Void |> Ok

        actual .=. expected

    [<Property>]
    let ``declaration of one member struct with  initializer referring to global var is possible`` () =
        let str =
            $"
                var global = 10;
                struct ssss4 {{ var inner = global;  }}
            "

        let actual = Interpreter.Runner.run str
        let expected = Value.Void |> Ok

        actual .=. expected

    [<Property>]
    let ``declaration of struct with many members is possible`` () =
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
    let ``declaration of struct with many members with initializers is possible`` () =
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

    [<Property>]
    let ``Memmber access operator gets field value correctly`` (oldValue: int) =
        let strName = "aaa"
        let fieldName = "inner"

        let str =
            $"
                struct {strName} {{
                        var {fieldName}  = {oldValue};
                }}
                var z =  {strName}{{}};
                z.{fieldName};
            "

        let actual = Interpreter.Runner.run str
        let expected = oldValue |> Value.IntValue |> Ok

        actual .=. expected

    [<Property>]
    let ``Memmber access operator sets field value correctly`` (oldValue: int) (newValue: int) =
        let strName = "aaa"
        let fieldName = "inner"

        let str =
            $"
                struct {strName} {{
                        var {fieldName}  = {oldValue};
                }}
                var z =  {strName}{{}};
                z.{fieldName} = {newValue};
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
    let ``Memmber access operator gets nested field value correctly`` (oldValue: int) =
        let strName = "aaa"
        let fieldName = "inner"
        let nestedStrName = "aaaNested"
        let nestedFieldName = "innerNested"

        let str =
            $"
                struct {nestedStrName} {{
                    var {nestedFieldName} = {oldValue};
                }}
                struct {strName} {{
                        var {fieldName}  = {nestedStrName}{{}};
                }}
                var z =  {strName}{{}};
                z.{fieldName}.{nestedFieldName};
            "

        let actual = Interpreter.Runner.run str
        let expected = oldValue |> IntValue |> Ok

        actual .=. expected

    [<Property>]
    let ``Memmber access operator sets nested field value correctly`` (oldValue: int) (newValue: int) =
        let strName = "aaa"
        let fieldName = "inner"
        let nestedStrName = "aaaNested"
        let nestedFieldName = "innerNested"

        let str =
            $"
                struct {nestedStrName} {{
                    var {nestedFieldName} = {oldValue};
                }}
                struct {strName} {{
                        var {fieldName}  = {nestedStrName}{{}};
                }}
                var z =  {strName}{{}};
                z.{fieldName}.{nestedFieldName} = {newValue};
                z;
            "

        let actual = Interpreter.Runner.run str

        let expected =
            strName
            |> Value.createStructInstance (
                [ fieldName |> Identifier.create,
                  (nestedStrName
                   |> Value.createStructInstance (
                       [ (nestedFieldName |> Identifier.create, newValue |> IntValue |> ref) ]
                       |> Map.ofList
                   ))
                  |> ref ]
                |> Map.ofList
            )
            |> Ok

        actual .=. expected

    [<Property>]
    let ``declaration of struct with initializer referring to another member works correctly`` () =
        let str =
            $"
                struct ssss4 {{ var inner1 = 5; var inner2 = inner1;  }}
            "

        let actual = Interpreter.Runner.run str
        let expected = Value.Void |> Ok

        actual .=. expected

    [<Property>]
    let ``instantation of struct with initializer referring to another member is possible`` (fieldValue: int) =
        let strName = "aaa"
        let fieldName = "inner"
        let fieldName2 = "inner2"

        let str =
            $"
                struct {strName} {{ var {fieldName} = {fieldValue}; var {fieldName2} = {fieldName};  }}
                var z = {strName} {{}} ;
                z;
            "

        let actual = Interpreter.Runner.run str

        let expected =
            strName
            |> Value.createStructInstance (
                [ (fieldName |> Identifier.create, fieldValue |> IntValue |> ref)
                  (fieldName2 |> Identifier.create, fieldValue |> IntValue |> ref) ]
                |> Map.ofList
            )
            |> Ok

        actual .=. expected

module Functions =
    
    [<Fact>]
    let ``parameters are passed to functions by value for floats``() =                
        
        let nfi = NumberFormatInfo();
        nfi.NumberDecimalSeparator <- ".";
        
        let localParamValue:float = 0.1;
        
        let program =
            $"""         
            fun foo(x) {{x=0;}}
                    
            var localVar = {localParamValue.ToString(nfi)};   
            foo(localVar);
            localVar;
            """
        let actual = Interpreter.Runner.run program
        let expected = localParamValue |> FloatValue |> Ok
        
        Assert.Equal(expected , actual)
    
    [<Property>]
    let ``parameters are passed to functions by value for ints`` (localParamValue:int) =                
        let program =
            $"         
            fun foo(x) {{x=0;}}
                    
            var localVar = {localParamValue};   
            foo(localVar);
            localVar;
            "
        let actual = Interpreter.Runner.run program
        let expected = localParamValue |> IntValue |> Ok
        
        expected .=. actual

    [<Property>]
    let ``function parameters shadow global variables`` (actualParamValue:int) (globalVarValue:int) =                
        let program =
            $"
            var x = {globalVarValue};
            fun foo(x) {{x=5;}}
                       
            foo({actualParamValue});
            x;
            "
        let actual = Interpreter.Runner.run program
        let expected = globalVarValue |> IntValue |> Ok
        
        expected .=. actual
    
    [<Fact>]
    let ``accessing variable from function caller scope causes error``() =
        let program =
            $"fun foo() {{x=5;}}
            
            fun foo2() {{
                var x = 10;
                foo();
            }}
            foo2();
            "
        let actual = Interpreter.Runner.run program
      
        Assert.True (match actual with
                       | Ok _ -> false
                       | Error _ -> true)

    [<Fact>]      
    let ``accessing non existing variable from function causes error``() =
        let program =
            $"fun foo() {{y=5;}}
            var x = 10;
            foo();
            "
        let actual = Interpreter.Runner.run program
      
        Assert.True (match actual with
                       | Ok _ -> false
                       | Error _ -> true)
        
    [<Property>]  
    let ``accessing global variable from function works correctly`` v =        
        let program =            
            $"
            var z = 5;
            fun foo() {{z={v};}}            
            foo();
            z;
            "
        let actual = Interpreter.Runner.run program
        let expected = v |> Value.IntValue |> Ok
        
        actual .=.expected
       