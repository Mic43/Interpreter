module RunnerTests

open Xunit
open Interpreter.AST
open FsCheck
open FsCheck.Xunit
open FSharpPlus
open Interpreter.AST

module Run =
    let (.=.) left right =
        left = right |@ sprintf "%A = %A" left right

    let idFuncProgram inputVal =
        let varIdent = Identifier.createIdentifier "x"
        let funIdent = Identifier.createIdentifier "id"
        // let inputVal = input |> IntValue
        let actualParam = inputVal |> Constant

        let idFunDecl =
            { Name = funIdent
              Parameters = [ varIdent ]
              Body =
                  [ varIdent |> Var |> ExpressionStatement ]
                  |> Block.Create }

        [ idFunDecl |> FunDeclaration
          { Name = funIdent
            ActualParameters = [ actualParam ] }
          |> FunCall
          |> ExpressionStatement
          |> ScopedStatement ]
        |> Program

    let blockize maxLevel statements =
        let rec blockizeRec maxLevel curLevel statements =

            if curLevel = maxLevel then
                statements
            else
                statements
                |> Block.Create
                |> BlockStatement
                |> List.singleton
                |> (blockizeRec maxLevel (curLevel + 1))

        blockizeRec maxLevel 0 statements

    [<Property>]
    let ``identity function returns input when called with int argument`` (input: int) =
        let inputVal = input |> IntValue

        //let sut = new Runner()

        let actual = Runner.run (idFuncProgram inputVal)

        let expected = inputVal |> Ok

        actual .=. expected

    [<Property>]
    let ``identity function returns input when called with float argument`` (input: NormalFloat) =
        let inputVal = input |> float |> FloatValue

        let actual = Runner.run (idFuncProgram inputVal)

        let expected = inputVal |> Result.Ok

        actual .=. expected

    [<Property>]
    let ``There can be only one function with specific name``
        (funName: NonEmptyString)
        (paramName: NonEmptyString)
        input
        (len: PositiveInt)
        =
        let varIdent =
            Identifier.createIdentifier (paramName |> string)

        let funIdent =
            Identifier.createIdentifier (funName |> string)

        let inputVal = input |> IntValue
        let actualParam = inputVal |> Constant

        let idFunDecl =
            { Name = funIdent
              Parameters = [ varIdent ]
              Body =
                  [ varIdent |> Var |> ExpressionStatement ]
                  |> Block.Create }

        let program =
            ((fun _ -> idFunDecl |> FunDeclaration)
             |> List.init (len.Get + 1))
            @ [ { Name = funIdent
                  ActualParameters = [ actualParam ] }
                |> FunCall
                |> ExpressionStatement
                |> ScopedStatement ]
            |> Program

        let actual = Runner.run program

        (match actual with
         | Ok _ -> false
         | Error _ -> true)

    [<Property>]
    let ``variable names in the same scope must be unique``
        (varName: NonEmptyString)
        varInit
        (repeatsCount: PositiveInt)
        =

        let varIdent =
            Identifier.createIdentifier (varName |> string)

        let vDecl =
            { Name = varIdent
              Initializer = varInit |> Constant }

        let program =
            (fun _ -> vDecl |> VarDeclaration |> ScopedStatement)
            |> List.init (repeatsCount.Get + 1)
            |> Program

        let actual = Runner.run program

        (match actual with
         | Ok _ -> false
         | Error _ -> true)

    [<Property>]
    let ``variable names in the different scopes can have same names``
        (varName: NonEmptyString)
        varInit
        (level: PositiveInt)
        =

        let varIdent =
            Identifier.createIdentifier (varName.Get)

        let vDecl =
            { Name = varIdent
              Initializer = varInit |> Constant }
            |> VarDeclaration

        let blockize maxLevel statement =
            let rec blockizeRec maxLevel curLevel statement =

                if curLevel = maxLevel then
                    []
                else
                    [ statement ]
                    @ [ (statement |> (blockizeRec maxLevel (curLevel + 1)))
                        |> Block.Create
                        |> BlockStatement ]

            blockizeRec maxLevel 0 statement

        let program =
            vDecl
            |> (blockize level.Get)
            |> List.map ScopedStatement
            |> Program

        let actual = Runner.run program

        actual |> Option.ofResult |> Option.isSome

    [<Property>]
    let ``it is possible to access variable in the same scope`` (varName: NonEmptyString) varInit (level: PositiveInt) =

        let varIdent =
            Identifier.createIdentifier (varName.Get)

        let vDecl =
            { Name = varIdent
              Initializer = varInit |> Constant }
            |> VarDeclaration

        let program =
            [ vDecl
              (varIdent |> Var |> ExpressionStatement) ]
            |> (blockize (level.Get - 1))
            |> List.map ScopedStatement
            |> Program

        let actual = Runner.run program
        actual |> Option.ofResult |> Option.isSome


    let outProgram program = 
        sprintf "\n%s" (program |> Printer.toStr)

    [<Property(Verbose = true)>]
    let ``it is possible to access variable from ancestor scope``
        (varName: NonEmptyString)
        varInit
        (level: PositiveInt)
        =

        let varIdent =
            Identifier.createIdentifier (varName.Get)

        let vDecl =
            { Name = varIdent
              Initializer = varInit |> Constant }
            |> VarDeclaration

        let varUsage = (varIdent |> Var |> ExpressionStatement)

        let program =
            [ vDecl ] @ blockize (level.Get - 1) [ varUsage ]
            |> List.map ScopedStatement
            |> Program

        let actual = Runner.run program

        // actual  .=. (varInit |> Result.Ok)
        actual |> Option.ofResult |> Option.isSome       
