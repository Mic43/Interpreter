namespace Interpreter.Parser

open FParsec
open Interpreter.AST

module Statement =
    open Identifier
    open Reserved
    open Expression
    open Common

    let private pIdentList =
        sepBy (spaces >>. pIdentifier .>> spaces) pListSeparator

    let private  pExpr = pExpr ()

    let private pFunDecl pBlock =
        funKeyword
        >>. spaces1
        >>. pipe3
            (pIdentifier .>> spaces .>> openBracket)
            (spaces >>. pIdentList .>> spaces .>> closeBracket)
            (spaces >>. pBlock .>> spaces)
            (fun name parameters body ->
                { Name = name
                  Body = body
                  Parameters = parameters })

    let private pVarDecl =
        pipe2
            (varKeyword >>. spaces1 >>. pIdentifier .>> spaces)
            (initVarOpKeyWord >>. spaces >>. pExpr |> opt
             .>> spaces
             .>> pSemicolon)

            (fun ident exp ->
                { VarDeclaration.Name = ident
                  InitExpression = exp })

    let private pStructDecl =
        pipe2
            (structKeyword
             >>. (spaces1 >>. pIdentifier .>> spaces))
            (pOpenCurlyBracket >>. ((many (pVarDecl |> trimmed)) |> trimmed)
             .>> pCloseCurlyBracket
             .>> spaces)
            (fun name declList ->
                { UserType.Kind =
                    { Struct.Members =
                        declList
                        |> List.map (fun vd -> (vd.Name, vd |> Field))
                        |> Map.ofList }
                    |> Struct
                  Name = name })

    let private pIfStmt pScopedStmt =

        let pElseBranch = elseKeyword >>. spaces >>. pScopedStmt

        pipe3
            (ifKeyword
             >>. spaces
             >>. openBracket
             >>. spaces
             >>. pExpr
             .>> spaces
             .>> closeBracket)
            (spaces >>. pScopedStmt .>> spaces)
            (opt pElseBranch)
            (fun cond trueStmt falseStmt ->
                { If.Condition = cond
                  OnTrue = trueStmt
                  OnFalse = falseStmt })

    let private pforInit =

        ((attempt pVarDecl) |>> VarDeclarationInit)
        <|> ((pExpr .>> spaces .>> pSemicolon)
             |>> ExpressionInit)

    let private pForStmt pScopedStmt =
        pipe4
            (forKeyword
             >>. spaces
             >>. openBracket
             >>. spaces
             >>. pforInit)
            (spaces >>. pExpr .>> spaces .>> pSemicolon)
            (spaces >>. pExpr .>> spaces .>> closeBracket)
            (spaces >>. pScopedStmt)
            (fun init cond incr body ->
                { Initializer = init
                  Condition = cond
                  Increment = incr
                  Body = body })

    let private pWhileStmt pScopedStatement =
        pipe2
            (whileKeyword
             >>. spaces
             >>. openBracket
             >>. spaces
             >>. pExpr
             .>> spaces
             .>> closeBracket)
            (spaces >>. pScopedStatement)
            (fun cond body -> { While.Condition = cond; Body = body })

    let private pReturnStmt =
        returnKeyword >>. spaces >>. opt pExpr
        .>> spaces
        .>> pSemicolon
        |>> fun exp ->
                exp
                |> Option.defaultValue (Value.Void |> Constant)
                |> ReturnStatement

    let private pBlock pscopedStatement =
        pOpenCurlyBracket
        >>. spaces
        >>. (many pscopedStatement)
        .>> spaces
        .>> pCloseCurlyBracket
        |>> Block.Create
        <!> "block"

    let private pScopedStatement block (blockImpl: Parser<Block, unit> ref) =
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
             <|> pReturnStmt
             <|> (whileStmt |>> WhileStatement)
             <|> (forStmt |>> ForStatement)
             <|> (ifStmt |>> IfStatement)
             <|> pExprStmt
             <|> blockStmt)
            .>> spaces

        blockImpl.Value <- pBlock scopedStatement
        ifStmtImp.Value <- pIfStmt scopedStatement
        forStmtImp.Value <- pForStmt scopedStatement
        whileStmtImp.Value <- pWhileStmt scopedStatement

        scopedStatement

    let private pStatement () =
        let block, blockImpl =
            createParserForwardedToRef<Block, unit> ()

        let funDeclStmt = pFunDecl block |>> FunDeclaration
        let userTypeDeclStmt = pStructDecl |>> UserTypeDeclaration
        let pScoped = pScopedStatement block blockImpl

        spaces
        >>. (funDeclStmt
             <|> userTypeDeclStmt
             <|> (pScoped |>> ScopedStatement))

    let pProgram =
        let stmt = pStatement ()

        (many1 stmt) .>> eof |>> Program.Of
