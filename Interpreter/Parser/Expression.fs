namespace Interpreter.Parser

open FParsec
open Interpreter.AST

open Common
open Reserved
open Value
open Identifier
open FSharpPlus.Control.TryBlock

module Expression =
    let pConst = pValue |>> Constant

    //TODO: not efficent, pIdentifier is common to both cases
    let pVar pExpr =
        attempt (
            pipe2
                (pIdentifier .>> (openSquareBracket |> trimmed))
                (pExpr .>> (closeSquareBracket |> trimmed))
                (fun ident exp -> (ident, exp) |> IndexedVar |> Mutable)
        )
        <|> (pIdentifier |>> (Var >> Mutable))

    let pExpList pExpr =
        sepBy (spaces >>. pExpr .>> spaces) pListSeparator

    let pfunCall pExpr =
        //spaces
        //>>.
        (pipe2
            (pIdentifier .>> spaces)
            (openBracket >>. spaces >>. (pExpList pExpr)
             .>> spaces
             .>> closeBracket)
            (fun ident actualParams ->
                { FunCall.Name = ident
                  ActualParameters = actualParams }))
        |>> FunCall

    let pListCreation pExpr =
        openSquareBracket >>. spaces >>. (pExpList pExpr)
        .>> spaces
        .>> closeSquareBracket
        |>> ListCreation

    let pTermExpr pExpr =

        let pfunCall = pfunCall pExpr
        pConst <|> (attempt pfunCall) <|> (pListCreation pExpr) <|> (pVar pExpr)

    let opp =
        new OperatorPrecedenceParser<Expression, unit, unit>()

    do
        let pTermExpr = pTermExpr opp.ExpressionParser
        let str_wspaces s = pstring s >>. spaces

        let termParser =
            (pTermExpr .>> spaces)
            <|> (between (str_wspaces "(") (str_wspaces ")") opp.ExpressionParser)

        opp.TermParser <- termParser

        opp.AddOperator(InfixOperator("=", spaces, 1, Associativity.Right, (fun x y -> (x, y) |> Assignment)))

        opp.AddOperator(InfixOperator("&&", spaces, 2, Associativity.Left, Expression.and_))
        opp.AddOperator(InfixOperator("||", spaces, 3, Associativity.Left, Expression.or_))

        opp.AddOperator(InfixOperator("==", spaces, 4, Associativity.Left, Expression.equals))
        opp.AddOperator(InfixOperator("<", spaces, 5, Associativity.Left, Expression.less))
        opp.AddOperator(InfixOperator(">", spaces, 5, Associativity.Left, Expression.greater))

        opp.AddOperator(InfixOperator("+", spaces, 6, Associativity.Left, Expression.add))
        opp.AddOperator(InfixOperator("-", spaces, 6, Associativity.Left, Expression.sub))
        opp.AddOperator(InfixOperator("*", spaces, 7, Associativity.Left, Expression.mul))
        opp.AddOperator(InfixOperator("/", spaces, 7, Associativity.Left, Expression.div))

        opp.AddOperator(PrefixOperator("!", spaces, 8, true, Expression.neg))
        opp.AddOperator(PrefixOperator("-", spaces, 8, true, Expression.unaryMinus))
        opp.AddOperator(PrefixOperator("++", spaces, 9, true, Expression.preIncr))
        opp.AddOperator(PostfixOperator("++", spaces, 9, true, Expression.postIncr))


    let pExpr () = opp.ExpressionParser
