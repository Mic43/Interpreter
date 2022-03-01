namespace Interpreter.Parser

open FParsec
open Interpreter.AST

open Common
open Reserved
open Value
open Identifier
//open FSharpPlus.Choice

module Expression =
    let pConst = pValue |>> Constant

    let pVar (pExpr: Parser<Expression, unit>) =

        let parseIndexesMembersList () =
            (openSquareBracket |> trimmed >>. pExpr
             .>> closeSquareBracket
             |> trimmed
             |>> Choice1Of2)
            <|> (memmberAccessOperator |> trimmed >>. pIdentifier|> trimmed
                 |>> Choice2Of2)
            |> many

        (pipe2 (pIdentifier .>> spaces |>> Var) (parseIndexesMembersList ())
         <| fun ident exprList ->
             exprList
             |> List.fold
                 (fun mutExp exp ->
                     match exp with
                     | Choice1Of2 indexExp -> (mutExp, indexExp) |> IndexedVar
                     | Choice2Of2 (fieldIdentifier) -> (mutExp, fieldIdentifier) |> MemmberAccess)
                 ident

        )
        |>> Mutable


    let pExpList pExpr = sepBy (pExpr |> trimmed) pListSeparator

    let pfunCall pExpr =
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

    let pInitializerList pExpr =
        sepBy
            (pipe2 (pIdentifier .>> (initVarOpKeyWord |> trimmed)) pExpr (fun i e -> (i, e))
             |> trimmed)
            pListSeparator

    let pStructCreation pExpr =
        pipe2 (pIdentifier .>> (pOpenCurlyBracket |> trimmed)) (pInitializerList pExpr) (fun ident initializeList ->
            { StructTypeName = ident
              FieldsInitialization = initializeList |> Map.ofList })
        .>> pCloseCurlyBracket
        |>> UserTypeCreation

    let pTermExpr pExpr =

        let pfunCall = pfunCall pExpr

        pConst
        <|> attempt pfunCall
        <|> pListCreation pExpr
        <|> attempt (pStructCreation pExpr)
        <|> pVar pExpr

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
