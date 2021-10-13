﻿namespace Interpreter.Parser

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

    let pforInit =

        ((attempt pVarDecl) |>> VarDeclarationInit)
        <|> ((pExpr .>> spaces .>> pSemicolon)
             |>> ExpressionInit)

    let pForStmt pScopedStmt =
        pipe4
            (forKeyword
             >>. spaces
             >>. pOpenBracket
             >>. spaces
             >>. pforInit)
            (spaces >>. pExpr .>> spaces .>> pSemicolon)
            (spaces >>. pExpr .>> spaces .>> pCloseBracket)
            (spaces >>. pScopedStmt)
            (fun init cond incr body ->
                { Initializer = init
                  Condition = cond
                  Increment = incr
                  Body = body })

    let pWhileStmt pScopedStatement =
        pipe2
            (whileKeyword
             >>. spaces
             >>. pOpenBracket
             >>. spaces
             >>. pExpr
             .>> spaces
             .>> pCloseBracket)
            (spaces >>. pScopedStatement)
            (fun cond body -> { While.Condition = cond; Body = body })

    let pBlock pscopedStatement =
        pOpenCurlyBracket
        >>. spaces
        >>. (many pscopedStatement)
        .>> spaces
        .>> pCloseCurlyBracket
        |>> Block.Create
        <!> "block"

    let pScopedStatement block blockImpl =
        let ifStmt, ifStmtImp = createParserForwardedToRef<If, unit> ()

        let forStmt, forStmtImp =
            createParserForwardedToRef<For, unit> ()

        let whileStmt, whileStmtImp =
            createParserForwardedToRef<While, unit> ()

        let pVarStmt = pVarDecl |>> VarDeclarationStatement

        let pExprStmt =
            pExpr .>> spaces .>> pSemicolon
            |>> ExpressionStatement

        let blockStmt = block |>> BlockStatement

        let scopedStatement =
            (pVarStmt
             <|> (whileStmt |>> WhileStatement)
             <|> (forStmt |>> ForStatement)
             <|> (ifStmt |>> IfStatement)
             <|> pExprStmt
             <|> blockStmt)
            .>> spaces

        do blockImpl := (pBlock scopedStatement)
        do ifStmtImp := (pIfStmt scopedStatement)
        do forStmtImp := (pForStmt scopedStatement)
        do whileStmtImp := (pWhileStmt scopedStatement)

        scopedStatement

    let pStatement () =
        let block, blockImpl =
            createParserForwardedToRef<Block, unit> ()

        let funDeclStmt = (pFunDecl block) |>> FunDeclaration
        let pScoped = pScopedStatement block blockImpl

        spaces
        >>. (funDeclStmt <|> (pScoped |>> ScopedStatement))

    let pProgram =
        let stmt = pStatement ()

        (many1 stmt) .>> eof |>> Program.Of