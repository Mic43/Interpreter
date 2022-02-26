module RunnerTests

open Xunit
open Interpreter.AST
open FsCheck
open FsCheck.Xunit
open FSharpPlus

let (.=.) left right =
    left = right |@ sprintf "%A = %A" left right

let outProgram program =
    sprintf "\n%s" (program |> Printer.toStr)

let idFuncProgram inputVal =
    let varIdent = Identifier.create "x"
    let funIdent = Identifier.create "id"
    // let inputVal = input |> IntValue
    let actualParam = inputVal |> Constant

    let idFunDecl =
        { Name = funIdent
          Parameters = [ varIdent ]
          Body =
              [ varIdent |> Var |> Mutable |> ReturnStatement ]
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

module Variables =

    [<Property>]
    let ``There can be only one function with specific name``
        (funName: NonEmptyString)
        (paramName: NonEmptyString)
        input
        (len: PositiveInt)
        =
        let varIdent = Identifier.create (paramName |> string)

        let funIdent = Identifier.create (funName |> string)

        let inputVal = input |> IntValue
        let actualParam = inputVal |> Constant

        let idFunDecl =
            { Name = funIdent
              Parameters = [ varIdent ]
              Body =
                  [ varIdent |> Var |> Mutable |> ExpressionStatement ]
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

        let actual = Interpreter.run program

        (match actual with
         | Ok _ -> false
         | Error _ -> true)

    [<Property>]
    let ``variable names in the same scope must be unique``
        (varName: NonEmptyString)
        varInit
        (repeatsCount: PositiveInt)
        =

      //  let varIdent = Identifier.create (varName |> string)

        let vDecl = Statement.varDeclare (varName |> string) (varInit |> Constant)
            // { Name = varIdent
            //   InitExpression = varInit |> Constant }

        let program =
            (fun _ -> vDecl)
            |> List.init (repeatsCount.Get + 1)
            |> Program

        let actual = Interpreter.run program

        (match actual with
         | Ok _ -> false
         | Error _ -> true)

    [<Property>]
    let ``variable names in the different scopes can have same names``
        (varName: NonEmptyString)
        varInit
        (level: PositiveInt)
        =

        let varIdent = Identifier.create (varName.Get)
            
        let vDecl =
            { Name = varIdent
              InitExpression = varInit |> Constant |> Some}
            |> VarDeclarationStatement

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

        let actual = Interpreter.run program

        actual |> Option.ofResult |> Option.isSome

    [<Property>]
    let ``it is possible to access variable in the same scope`` (varName: NonEmptyString) varInit (level: PositiveInt) =

        let varIdent = Identifier.create (varName.Get)

        let vDecl =
            { Name = varIdent
              InitExpression = varInit |> Constant |> Some}
            |> VarDeclarationStatement

        let program =
            [ vDecl
              (varIdent |> Var |> Mutable |> ExpressionStatement) ]
            |> (blockize (level.Get - 1))
            |> List.map ScopedStatement
            |> Program

        let actual = Interpreter.run program
        actual |> Option.ofResult |> Option.isSome

    [<Property(Verbose = true)>]
    let ``it is possible to access variable from ancestor scope``
        (varName: NonEmptyString)
        varInit
        (level: PositiveInt)
        =

        let varIdent = Identifier.create (varName.Get)

        let vDecl =
            { Name = varIdent
              InitExpression = varInit |> Constant |> Some }
            |> VarDeclarationStatement

        let varUsage =
            (varIdent |> Var |> Mutable |> ExpressionStatement)

        let program =
            [ vDecl ] @ blockize (level.Get - 1) [ varUsage ]
            |> List.map ScopedStatement
            |> Program

        let actual = Interpreter.run program

        // actual  .=. (varInit |> Result.Ok)
        actual |> Option.ofResult |> Option.isSome

    [<Property(Verbose = true)>]
    let ``assignment returns its left operand``
        (varName: NonEmptyString)
        varInit
        varAssignmentValue
        (maxNestCount: PositiveInt)
        =

        let varIdent = Identifier.create (varName.Get)

        let vDecl = Statement.varDeclare varName.Get (Expression.intConstant varInit)
            // { Name = varIdent
            //   InitExpression = Expression.intConstant varInit }
            // |> VarDeclarationStatement
            // |> ScopedStatement

        let rec varAssignment curNestCount maxNestCount =
            if curNestCount = maxNestCount then
                (Expression.intConstant varAssignmentValue)
            else
                Expression.assignment varIdent (varAssignment (curNestCount + 1) maxNestCount)

        let program =
            [ vDecl
              (varAssignment 0 maxNestCount.Get)
              |> Statement.FromExpression ]
            |> Program

        let actual = Interpreter.run program

        let expected =
            varAssignmentValue |> IntValue |> Result.Ok

        actual .=. expected

    [<Property>]
    let ``increment variable in while loop works correctly`` (n: PositiveInt) =

        let init = 0
        let iVar = "i" |> Identifier.create

        let vDecl =
            { Name = iVar
              InitExpression = Expression.intConstant init |> Some}
            |> VarDeclarationStatement

        let whileStmt =
            { While.Condition = Expression.less (Expression.var "i") (Expression.intConstant n.Get)
              Body =
                  (Expression.assignment iVar (Expression.add (Expression.var "i") (Expression.intConstant 1)))
                  |> ExpressionStatement }
            |> WhileStatement

        let program =
            [ vDecl
              whileStmt
              (Expression.var "i") |> ExpressionStatement ]
            |> List.map ScopedStatement
            |> Program

        let expected = n.Get |> IntValue |> Result.Ok
        let actual = Interpreter.run program

        expected .=. actual

module Programs =

    [<Property>]
    let ``identity function returns input when called with int argument`` (input: int) =
        let inputVal = input |> IntValue

        //let sut = new Runner()

        let actual = Interpreter.run (idFuncProgram inputVal)

        let expected = inputVal |> Ok

        actual .=. expected

    [<Property>]
    let ``identity function returns input when called with float argument`` (input: NormalFloat) =
        let inputVal = input |> float |> FloatValue

        let actual = Interpreter.run (idFuncProgram inputVal)

        let expected = inputVal |> Result.Ok

        actual .=. expected

    [<Property>] //Replay = "1877725774, 296945321"
    let ``addition fun works properly for integers `` (a) b (funName: NonEmptyString) =

        let funName = funName.Get

        let addFunDecl =
            { Name = funName |> Identifier.create
              Parameters =
                  [ "a" |> Identifier.create
                    "b" |> Identifier.create ]
              Body =
                  Block.Create [ Expression.add ("a" |> Expression.var) ("b" |> Expression.var)
                                 |> ReturnStatement ] }
            |> FunDeclaration

        let funCall =
            Expression.funCall
                (funName |> Identifier.create)
                [ a |> Expression.intConstant
                  b |> Expression.intConstant ]
            |> Statement.FromExpression

        let program = [ addFunDecl; funCall ] |> Program

        let actual = Interpreter.run program
        let expected = (a + b) |> IntValue |> Result.Ok

        actual .=. expected

    let fibParams: obj [] seq =
        seq {
            yield [| 0 |]
            yield [| 1 |]
            yield [| 2 |]
            yield [| 3 |]
            yield [| 4 |]
            yield [| 5 |]
            yield [| 6 |]
            yield [| 7 |]
        }

    [<Theory; MemberData("fibParams")>]
    let ``fibonacci series function works correctly`` (n: int) =
        let rec fib n =
            match n with
            | 0 -> 1
            | 1 -> 1
            | _ -> fib (n - 1) + fib (n - 2)

        let varExp = Expression.var "n"
        let zeroConst = Expression.intConstant 0
        let oneConst = Expression.intConstant 1
        let twoConst = Expression.intConstant 2

        let funName = "fib" |> Identifier.create

        let fibDecl =
            { Name = funName
              Parameters = [ "n" |> Identifier.create ]
              Body =
                  [ { Condition =
                          (Expression.equals varExp zeroConst)
                          |> Expression.or_ (Expression.equals varExp oneConst)
                      OnTrue = oneConst |> ReturnStatement
                      OnFalse =
                          (Expression.funCall funName [ (Expression.sub varExp oneConst) ])
                          |> Expression.add (Expression.funCall funName [ (Expression.sub varExp twoConst) ])
                          |> ReturnStatement |> Some}
                    |> IfStatement ]
                  |> Block.Create }
            |> FunDeclaration

        let program =
            [ fibDecl
              (Expression.funCall funName [ Expression.intConstant n ])
              |> Statement.FromExpression ]
            |> Program

        let expected = fib (n) |> IntValue |> Result.Ok
        let actual = Interpreter.run program

        Assert.Equal(expected, actual)
