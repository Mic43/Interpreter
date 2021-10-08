namespace Interpreter.Parser

open FParsec
open Interpreter.AST

open Common
open Reserved
open Value
open Identifier

module Expression =

    let pConst = pValue |>> Expression.Constant
    let pVar = pIdentifier |>> (Var >> Mutable)

    let pExpList pExpr =
        sepBy (spaces >>. pExpr .>> spaces) pListSeparator

    let pfunCall pExpr =
        //spaces
        //>>. 
        (pipe2
                 (pIdentifier .>> spaces)
                 (pOpenBracket >>. spaces >>. (pExpList pExpr)
                  .>> spaces
                  .>> pCloseBracket)
                 (fun ident actualParams ->
                     { FunCall.Name = ident
                       ActualParameters = actualParams }))
        |>> FunCall

    let pTermExpr () =
        let exp, expImp =
            createParserForwardedToRef<Expression, unit> ()

        let pfunCall = pfunCall exp
        expImp := (pConst <|> (attempt pfunCall) <|> pVar)
        exp

    let opp =
        new OperatorPrecedenceParser<Expression, unit, unit>()

    do
        let pTermExpr = pTermExpr ()
        let termParser = pTermExpr .>> spaces
        opp.TermParser <- termParser

        //opp.AddOperator(InfixOperator("=", spaces, 0, Associativity.Right, (fun a b -> Expression.assignment a b)))

        opp.AddOperator(InfixOperator("&&", spaces, 2, Associativity.Left, (fun a b -> Expression.and_ a b)))
        opp.AddOperator(InfixOperator("||", spaces, 3, Associativity.Left, (fun a b -> Expression.or_ a b)))
        opp.AddOperator(InfixOperator("==", spaces, 4, Associativity.Left, (fun a b -> Expression.equals a b)))
        opp.AddOperator(InfixOperator("<", spaces, 5, Associativity.Left, (fun a b -> Expression.less a b)))

        opp.AddOperator(InfixOperator("+", spaces, 6, Associativity.Left, (fun a b -> Expression.add a b)))
        opp.AddOperator(InfixOperator("-", spaces, 6, Associativity.Left, (fun a b -> Expression.sub a b)))
        opp.AddOperator(InfixOperator("*", spaces, 7, Associativity.Left, (fun a b -> Expression.mul a b)))
    //  opp.AddOperator(InfixOperator("/", spaces, 7, Associativity.Left, (fun a b -> Expression.div a b)))
    let pExpr () = opp.ExpressionParser
