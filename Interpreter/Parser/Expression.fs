namespace Interpreter.Parser

open FParsec
open Interpreter.AST


module Common =
    let pListSeparator = pchar ','

    let (<!>) (p: Parser<_, _>) label : Parser<_, _> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            reply

module Reserved =

    let pOpenBracket: Parser<char, unit> = pchar '('
    let pCloseBracket: Parser<char, unit> = pchar ')'
    let pOpenCurlyBracket: Parser<char, unit> = pchar '{'
    let pCloseCurlyBracket: Parser<char, unit> = pchar '}'
    let varKeyword: Parser<string, unit> = pstring "var"
    let funKeyword: Parser<string, unit> = pstring "fun"
    let initVarOpKeyWord: Parser<char, unit> = pchar '='
    let pSemicolon: Parser<char, unit> = pchar ';'

module Value =
    let pInt: Parser<Value, unit> = pint32 |>> IntValue
    let pFloat: Parser<Value, unit> = pfloat |>> FloatValue

    let private pTrue: Parser<Value, unit> =
        pstring "true" |>> (fun _ -> true |> BoolValue)

    let private pFalse: Parser<Value, unit> =
        pstring "false" |>> (fun _ -> true |> BoolValue)

    let pBool = pTrue <|> pFalse
    let pValue = pFloat <|> pInt <|> pBool

module Identifier =
    let isAsciiIdStart c = isAsciiLetter c || c = '_'

    let isAsciiIdContinue c =
        isAsciiLetter c
        || isDigit c
        || c = '_'
        || c = '\''

    let pIdentifier: Parser<Identifier, unit> =
        identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue))
        |>> Identifier.create

module Expression =
    open Value
    open Identifier
    open Common
    open Reserved

    let pConst = pValue |>> Expression.Constant
    let pVar = pIdentifier |>> (Var >> Mutable)

    let pExpList pExpr =
        sepBy (spaces >>. pExpr .>> spaces) pListSeparator

    let pfunCall pExpr =
        (pipe2
            (pIdentifier .>> spaces)
            (pOpenBracket >>. spaces >>. (pExpList pExpr)
             .>> spaces
             .>> pCloseBracket)
            (fun ident actualParams ->
                { FunCall.Name = ident
                  ActualParameters = actualParams }))
        |>> FunCall

    let pExpr() = 
        let exp, expImp =
                  createParserForwardedToRef<Expression, unit> ()
        let pfunCall = pfunCall exp
        expImp:= (pConst <|> (attempt pfunCall) <|> pVar )
        exp
