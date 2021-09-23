namespace Interpreter.AST

open FSharpPlus

module Utils =    
    let traverseAUsingFold<'T, 'U, 'Error> (mapper: 'T -> Result<'U, 'Error>) (lst: 'T list) =
        (List.foldBack
            (fun t s ->
                s
                |> Result.apply (t |> mapper |> Result.map (List.singleton >> (@))))
            lst
            ([] |> Result.Ok))

    let rec traverseA<'T, 'U, 'Error> (mapper: 'T -> Result<'U, 'Error>) (lst: 'T list) : Result<'U list, 'Error> =
        let traverseARec lst = traverseA mapper lst

        match lst with
        | [] -> Result.Ok []
        | head :: rest ->
            let restRes = traverseARec rest

            let op =
                head
                |> mapper
                |> (Result.map List.singleton)
                |> Result.map (@)

            restRes |> (Result.apply op) 

    let traverseATail<'T, 'U, 'Error> (mapper: 'T -> Result<'U, 'Error>) (lst: 'T list) : Result<'U list, 'Error> =
        let rec traverseARec mapper lst acc =
            match lst with
            | [] -> acc
            | head :: rest ->
                let tmp =
                    head
                    |> mapper
                    |> (Result.map List.singleton)
                    |> Result.map (fun l1 l2 -> l2 @ l1)

                acc
                |> (Result.apply tmp)
                |> (traverseARec mapper rest)

        traverseARec mapper lst ([] |> Result.Ok)

    let rec traverseResultA f list =
        // define the applicative functions
        let (<*>) = Result.apply
        let retn = Result.Ok

        // define a "cons" function
        let cons head tail = head :: tail

        // loop through the list
        match list with
        | [] ->
            // if empty, lift [] to a Result
            retn []
        | head :: tail ->
            // otherwise lift the head to a Result using f
            // and cons it with the lifted version of the remaining list
            let tmp = retn cons
            let tmp2 = tmp <*> (f head)
            // tmp2 <*> (traverseResultA f tail)
            (traverseResultA f tail) |> (Result.apply tmp2)

    let rec traverseM<'T, 'U, 'Error> (mapper: 'T -> Result<'U, 'Error>) (lst: 'T list) : Result<'U list, 'Error> =
        let traverseRec lst = traverseM mapper lst

        match lst with
        | [] -> Result.Ok []
        | head :: rest ->
            monad' {
                let! headR = (mapper head)
                let! tailR = (traverseRec rest)

                return ([ headR ] @ tailR)

            }


    let traverseMTailUntil<'T, 'U, 'Error>
        stopCondition
        (mapper: 'T -> Result<'U, 'Error>)
        (lst: 'T list)
        : Result<'U list, 'Error> =
        let rec traverseMTailUntilRec lst acc =
            match lst with
            | [] -> acc
            | head :: rest ->
                monad' {
                    let! accR = acc
                    let! headR = (mapper head)
                    let newAcc = (accR @ [ headR ]) |> Result.Ok

                    if stopCondition headR then
                        return! newAcc
                    else
                        return! traverseMTailUntilRec rest newAcc
                }

        traverseMTailUntilRec lst ([] |> Result.Ok)

    let traverseMTail<'T, 'U, 'Error> (mapper: 'T -> Result<'U, 'Error>) (lst: 'T list) : Result<'U list, 'Error> =
        traverseMTailUntil (fun _ -> false) mapper lst
