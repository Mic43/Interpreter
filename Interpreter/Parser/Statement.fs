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

    let pExpr = pExpr ()

    let pFunDecl pBlock =       
        funKeyword
        >>. spaces
        >>. pipe3
                (pIdentifier .>> spaces .>> pOpenBracket)
                (spaces >>. pIdentList .>> spaces .>> pCloseBracket)
                (spaces >>. pBlock .>> spaces)
                (fun name parametrs body ->
                    { Name = name
                      Body = body
                      Parameters = parametrs })

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

    let pIfStmt pScopedStmt =
        pipe3
            (ifKeyword
             >>. spaces
             >>. pOpenBracket
             >>. spaces
             >>. pExpr
             .>> spaces
             .>> pCloseBracket)
            (spaces >>. pScopedStmt .>> elseKeyword)
            (spaces >>. pScopedStmt)
            (fun cond trueStmt falseStmt ->
                { If.Condition = cond
                  OnTrue = trueStmt
                  OnFalse = falseStmt })

    let pScopedStatement block blockImpl =
        //let block, blockImpl =
        //    createParserForwardedToRef<Block, unit> ()
        let ifStmt, ifStmtImp = createParserForwardedToRef<If, unit> ()

        let pVarStmt = pVarDecl |>> VarDeclarationStatement

        let pExprStmt =
            pExpr .>> spaces .>> pSemicolon
            |>> ExpressionStatement

        let blockStmt = block |>> BlockStatement        

        let scopedStatement =            
            (pVarStmt <|> (ifStmt |>> IfStatement) <|> pExprStmt <|> blockStmt)
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

        do ifStmtImp := (pIfStmt scopedStatement)

        scopedStatement

    let pStatement () =
        let block, blockImpl =
            createParserForwardedToRef<Block, unit> ()

        let funDeclStmt = (pFunDecl block) |>> FunDeclaration
        let pScoped = pScopedStatement block blockImpl

        spaces >>. (funDeclStmt <|> (pScoped |>> ScopedStatement))

    let pProgram =
        let stmt = pStatement ()

        (many1 stmt) .>> eof |>> Program.Of
