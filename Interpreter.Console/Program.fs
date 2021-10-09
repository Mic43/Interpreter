open System
// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp




// let test p str =
//     match run p str with
//     | Success (result, _, _) -> printfn "Success: %A" result
//     | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg
// let interpret p str =
//     match run p str with
//     | Success (result, _, _) -> Interpreter.run result
//     | Failure (errorMsg, _, _) -> 
//         printfn "Failure: %s" errorMsg
//         Errors.createResult ErrorType.Other "error parsing"

[<EntryPoint>]
let main argv =      
    let str = "
        fun fib ( n ) 
        {            
            if (!!(n == 0 || n == 1)) 
                1;
            else 
                fib( n - 1) + fib (n-2);
        } 
        fun silnia(n)
        {
            
            if (n < 2 && n > -1)
              { 1;}
            else
                n * silnia(n-1);
        }
        fun aa(x)
        {     
            {
              //  var x = 5;
                for(var i=0;i<x;++i)            
                    println(i);            
            }
        }

       // println(silnia (4));
       // println(fib(8));
        var n = 3;
        var k = n = 6;
               
        //println (2.0/5);
        //println(n);
        aa(n++);
        "
    Interpreter.Runner.run str
    Console.ReadLine() |> ignore
    0 
