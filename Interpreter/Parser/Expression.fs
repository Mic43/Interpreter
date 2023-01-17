namespace Interpreter.Parser

open FParsec
open Interpreter.AST

open Common
open Reserved
open Value
open Identifier
//open FSharpPlus.Choice

module Expression =
    let private pConst = pValue |>> Constant

    let private pVar (pExpr: Parser<Expression, unit>) =

        let parseIndexesMembersList () =
            (openSquareBracket |> trimmed >>. pExpr
             .>> closeSquareBracket
             |> trimmed
             |>> Choice1Of2)
            <|> (memmberAccessOperator |> trimmed >>. pIdentifier
                 |> trimmed
                 |>> Choice2Of2)
            |> many

        (pipe2 (pIdentifier .>> spaces |>> Var) (parseIndexesMembersList ())
         <| fun ident exprList ->
             exprList
             |> List.fold
                 (fun mutExp exp ->
                     match exp with
                     | Choice1Of2 indexExp -> (mutExp, indexExp) |> IndexedVar
                     | Choice2Of2 fieldIdentifier -> (mutExp, fieldIdentifier) |> MemberAccess)
                 ident

        )
        |>> Mutable

    let private pExpList pExpr = sepBy (pExpr |> trimmed) pListSeparator

    let private pfunCall pExpr =
        (pipe2
            (pIdentifier .>> spaces)
            (openBracket >>. spaces >>. (pExpList pExpr)
             .>> spaces
             .>> closeBracket)
            (fun ident actualParams ->
                { FunCall.Name = ident
                  ActualParameters = actualParams }))
        |>> FunCall

    let private pListCreation pExpr =
        openSquareBracket >>. spaces >>. (pExpList pExpr)
        .>> spaces
        .>> closeSquareBracket
        |>> ListCreation

    let private pInitializerList pExpr =
        sepBy
            (pipe2 (pIdentifier .>> (initVarOpKeyWord |> trimmed)) pExpr (fun i e -> (i, e))
             |> trimmed)
            pListSeparator

    let private pStructCreation pExpr =
        pipe2 (pIdentifier .>> (pOpenCurlyBracket |> trimmed)) (pInitializerList pExpr) (fun ident initializeList ->
            { StructTypeName = ident
              FieldsInitialization = initializeList |> Map.ofList })
        .>> pCloseCurlyBracket
        |>> UserTypeCreation

    let private pTermExpr pExpr =

        let pfunCall = pfunCall pExpr

        pConst
        <|> attempt pfunCall
        <|> pListCreation pExpr
        <|> attempt (pStructCreation pExpr)
        <|> pVar pExpr

    let private opp =
        new OperatorPrecedenceParser<Expression, unit, unit>()

    do
        let pTermExpr =
            pTermExpr opp.ExpressionParser

        let str_wspaces s = pstring s >>. spaces

        let termParser =
            (pTermExpr .>> spaces)
            <|> (between (str_wspaces "(") (str_wspaces ")") opp.ExpressionParser)

        let infixOperators =
            [ InfixOperator("=", spaces, 1, Associativity.Right, (fun x y -> (x, y) |> Assignment))
              InfixOperator("&&", spaces, 2, Associativity.Left, Expression.and_)
              InfixOperator("||", spaces, 3, Associativity.Left, Expression.or_)
              InfixOperator("==", spaces, 4, Associativity.Left, Expression.equals)
              InfixOperator("<", spaces, 5, Associativity.Left, Expression.less)
              InfixOperator(">", spaces, 5, Associativity.Left, Expression.greater)
              InfixOperator("+", spaces, 6, Associativity.Left, Expression.add)
              InfixOperator("-", spaces, 6, Associativity.Left, Expression.sub)
              InfixOperator("*", spaces, 7, Associativity.Left, Expression.mul)
              InfixOperator("/", spaces, 7, Associativity.Left, Expression.div) ]

        let unaryOperators: Operator<Expression,unit,unit> list =
            [ PrefixOperator("!", spaces, 8, true, Expression.neg)
              PrefixOperator("-", spaces, 8, true, Expression.unaryMinus)
              PrefixOperator("++", spaces, 9, true, Expression.preIncr)
              PostfixOperator("++", spaces, 9, true, Expression.postIncr) ]

        opp.TermParser <- termParser
        
        infixOperators |> List.iter opp.AddOperator
        unaryOperators |> List.iter opp.AddOperator
              
    let pExpr () = opp.ExpressionParser
