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

    let trimmed p = spaces >>. p .>> spaces

module Reserved =
    let openBracket: ParserU<char> = pchar '('
    let closeBracket: ParserU<char> = pchar ')'
    let pOpenCurlyBracket: ParserU<char> = pchar '{'
    let pCloseCurlyBracket: ParserU<char> = pchar '}'
    let varKeyword: ParserU<string> = pstring "var"
    let funKeyword: ParserU<string> = pstring "fun"
    let initVarOpKeyWord: ParserU<char> = pchar '='
    let pSemicolon: ParserU<char> = pchar ';'
    let ifKeyword: ParserU<string> = pstring "if"
    let elseKeyword: ParserU<string> = pstring "else"
    let forKeyword: ParserU<string> = pstring "for"
    let whileKeyword: ParserU<string> = pstring "while"
    let openSquareBracket: ParserU<string> = pstring "["
    let closeSquareBracket: ParserU<string> = pstring "]"
    let returnKeyword:ParserU<string> = pstring "return"
    let comma: ParserU<char> = pchar ','

module Value =
    open Reserved
    open Common

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

    let pString: ParserU<Value> =
        let str s = pstring s

        let stringLiteral =
            let escape =
                anyOf "\"\\/bfnrt"
                |>> function
                    | 'b' -> "\b"
                    | 'f' -> "\u000C"
                    | 'n' -> "\n"
                    | 'r' -> "\r"
                    | 't' -> "\t"
                    | c -> string c // every other char is mapped to itself

            let unicodeEscape =
                /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
                let hex2int c = (int c &&& 15) + (int c >>> 6) * 9

                str "u"
                >>. pipe4
                        hex
                        hex
                        hex
                        hex
                        (fun h3 h2 h1 h0 ->
                            (hex2int h3) * 4096
                            + (hex2int h2) * 256
                            + (hex2int h1) * 16
                            + hex2int h0
                            |> char
                            |> string)

            let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)

            let normalCharSnippet =
                manySatisfy (fun c -> c <> '"' && c <> '\\')

            between (str "\"") (str "\"") (stringsSepBy normalCharSnippet escapedCharSnippet)
        //(manyTill anyChar (pchar '"')))

        stringLiteral |>> StringValue

    let pBool = pTrue <|> pFalse
    let private pValueInternal () =
        // let list, listImp =
        //     createParserForwardedToRef<Value, unit> ()

        let pVal =
          //  list
             attempt pFloat
            <|> pInt
            <|> pBool
            <|> pString

       // do listImp := pList pVal
        pVal

    let pValue = pValueInternal ()

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
