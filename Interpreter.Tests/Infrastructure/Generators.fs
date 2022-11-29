module Interpreter.Tests.Infrastructure.Generators

open FsCheck
open Interpreter.AST
open System.Linq.Expressions

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

    let variableExpression = [ Arb.Default.NonWhiteSpaceString ]
module Expressions = 
    let constantExpressionTree =
        let constant = Values.numericValues |> Gen.map Constant

        let rec inner s=
            match s with 
            | 0 -> constant
            | n when n > 0 ->
                let subTree = inner (n / 2)

                [ constant
                  (Gen.map3 Expression.binary) Arb.generate<BinaryOp> subTree subTree 
                  (Arb.generate<UnaryOp>,  subTree) ||> Gen.map2  (fun op exp -> (op,exp) |> SimpleUnary)
                  ]                  
                |> Gen.oneof
            | _-> invalidArg "s" "size must be positive"
        Gen.sized inner
