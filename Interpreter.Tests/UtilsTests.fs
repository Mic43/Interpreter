module UtilsTests

open Xunit
open Interpreter.AST
open FsCheck
open FsCheck.Xunit
open FSharpPlus


module Traverse =   
    let (.=.) left right =
        left = right |@ sprintf "%A = %A" left right
    //[<Fact>]
    //let ``traverse test`` () =
    //    let input = [1;2;3]
    //    input |> Utils.traverseA (fun i ->
    //        printf "%A" i
    //        i |> Result.Ok)
    //    Assert.True(false)

    let isEven i =
        if i % 2 = 0 then
            i |> Result.Ok
        else
            Result.Error()


    [<Property>]
    let ``Tail recursive and standard versions yield same results for Ints and no preserving errors`` (lst: int list) =

        (lst |> Traversable.traverseATail isEven)
        .=. (lst |> Traversable.traverseA isEven)

    [<Property>]
    let ``Moandic and applicative versions yields same results for ints`` (lst: int list) =
        (lst |> Traversable.traverseM isEven)
        .=. (lst |> Traversable.traverseA isEven)

    [<Property>]
    let ``Custom and standard versions yield same results for Ints`` (lst: int list) =

        (lst |> Traversable.traverseResultA isEven)
        .=. (lst |> Traversable.traverseA isEven)

    [<Property>]
    let ``Monadic and tailrecusive yields same results for Ints`` (lst: int list) =

        (lst |> Traversable.traverseM isEven)
        .=. (lst |> Traversable.traverseMTail isEven)

    // [<Property>]
    // let ``Monadic tail recurisve returns empty when stop condition is always true and mapper retuns ok, for ints``
    //     (lst: int list)
    //     =
    //     let mapper i = i |> Result.Ok

    //     (lst
    //      |> Utils.traverseMTailUntil (fun _ -> true) mapper)
    //     .=. ([] |> Result.Ok)

    [<Property>]
    let ``Aplicative traverse using fold is equal to traverse for ints``
        (lst: int list)
        =       
        (lst |> Traversable.traverseA isEven)
              .=. (lst |> Traversable.traverseAUsingFold isEven)

    // [<Property>]
    // let ``Monadic tail recurisve stops in correct position, for ints`` (lst: int list) =
    //     let mapper i = i |> Result.Ok

    //     let condtion i =
    //         match isEven i with
    //         | Result.Ok _ -> true
    //         | _ -> false

    //     let r =
    //         monad' {
    //             let! r = (lst |> Utils.traverseMTailUntil condtion mapper)

    //             return
    //                 (r
    //                  |> (List.take r.Length)
    //                  |> List.forall (condtion))
    //         }

    //     match r with
    //     | Ok a -> a
    //     | _ -> false



//[<Property>]
//let ``Custom and standard versions yield same results for Statements`` (lst: Statement list) =
//    let mapper s = s |> Result.Ok

//    (lst |> Utils.traverseResultA mapper)
//    .=. (lst |> Utils.traverseA mapper)
