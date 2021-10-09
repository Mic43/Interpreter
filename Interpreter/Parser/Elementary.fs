namespace Interpreter.Parser

open FParsec
open Interpreter.AST

type ParserU<'T> = Parser<'T, unit>

module Common =
    let pListSeparator: ParserU<char> = pchar ','

    let (<!>) (p: Parser<_, _>) label : Parser<_, _> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            reply

module Reserved =
    let pOpenBracket: ParserU<char> = pchar '('
    let pCloseBracket: ParserU<char> = pchar ')'
    let pOpenCurlyBracket: ParserU<char> = pchar '{'
    let pCloseCurlyBracket: ParserU<char> = pchar '}'
    let varKeyword: ParserU<string> = pstring "var"
    let funKeyword: ParserU<string> = pstring "fun"
    let initVarOpKeyWord: ParserU<char> = pchar '='
    let pSemicolon: ParserU<char> = pchar ';'
    let ifKeyword: ParserU<string> = pstring "if"
    let elseKeyword: ParserU<string> = pstring "else"
    let forKeyword: ParserU<string> = pstring "for"
module Value =
    let pInt: Parser<Value, unit> = pint32 |>> IntValue

    let pFloat: Parser<Value, unit> =
        let numberFormat =
            NumberLiteralOptions.AllowMinusSign
            ||| NumberLiteralOptions.AllowFraction
            ||| NumberLiteralOptions.AllowExponent

        numberLiteral numberFormat "number"
        >>= fun nl ->
                if nl.IsInteger |> not then
                    (nl.String) |> float |> FloatValue |> preturn
                else
                    fail "not a float"

    let private pTrue: Parser<Value, unit> =
        pstring "true" |>> (fun _ -> true |> BoolValue)

    let private pFalse: Parser<Value, unit> =
        pstring "false" |>> (fun _ -> false |> BoolValue)

    let pBool = pTrue <|> pFalse

    let pValue = attempt pFloat <|> pInt <|> pBool

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
