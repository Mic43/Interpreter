module Generators

open FsCheck
open Interpreter.AST

module Values =
    let numericValues =
        [ Arb.Default.NormalFloat().Generator
          |> Gen.map (fun f -> f.Get |> FloatValue)
          Arb.Default.Int32().Generator |> Gen.map IntValue ]
        |> Gen.oneof

    let numericZeros =
        [ 0 |> Gen.constant |> Gen.map IntValue
          0.0 |> Gen.constant |> Gen.map FloatValue ]
        |> Gen.oneof
    
    let variableExpression = 
        [Arb.Default.NonWhiteSpaceString]