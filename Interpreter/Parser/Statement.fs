namespace Interpreter.Parser

open FParsec
open Interpreter.AST

module Statement =
    open Identifier
    open Reserved
    open Expression
    open Common

    let pIdentList =
        sepBy (spaces >>. pIdentifier .>> spaces) pListSeparator

    let pFunDecl pBlock =
        spaces
        >>. funKeyword
        >>. spaces
        >>. pipe3
                (pIdentifier .>> spaces .>> pOpenBracket)
                (spaces >>. pIdentList .>> spaces .>> pCloseBracket)
                (spaces >>. pBlock .>> spaces)
                (fun name parametrs body ->
                    { Name = name
                      Body = body
                      Parameters = parametrs })

    let pExpr = pExpr()
    
    let pVarDecl =
        pipe2
            (varKeyword >>. spaces1 >>. pIdentifier
             .>> spaces
             .>> initVarOpKeyWord
             .>> spaces)
            (pExpr .>> spaces .>> pSemicolon)
            (fun ident exp ->
                { VarDeclaration.Name = ident
                  InitExpression = exp })

    let pScopedStatement block blockImpl =
        //let block, blockImpl =
        //    createParserForwardedToRef<Block, unit> ()

        let pVarStmt = pVarDecl |>> VarDeclarationStatement

        let pExprStmt =
            pExpr .>> spaces .>> pSemicolon
            |>> ExpressionStatement

        let blockStmt = block |>> BlockStatement


        let scopedStatement =
            spaces >>. ( pVarStmt <|> pExprStmt <|> blockStmt)
            .>> spaces

        do
            blockImpl
            := pOpenCurlyBracket
               >>. spaces
               >>. (many scopedStatement)
               .>> spaces
               .>> pCloseCurlyBracket
               |>> Block.Create
               <!> "block"

        scopedStatement

    let pStatement () =
        let block, blockImpl =
            createParserForwardedToRef<Block, unit> ()

        let funDeclStmt = (pFunDecl block) |>> FunDeclaration
        let pScoped = pScopedStatement block blockImpl

        funDeclStmt <|> (pScoped |>> ScopedStatement)

    let pProgram =
        let stmt = pStatement ()

        (many1 stmt) .>> eof |>> Program.Of
