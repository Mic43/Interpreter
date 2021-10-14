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
            
              //  var x = 5;
            var i = 0;
            while (i < x)            
            {
                for(var i=0;i<x;++i)                            
                    println(i);                                                        
                i++;
            }   
        }
        fun printTab(array)
        {
            for(var i=0;i<len(array);++i)                            
                print(\" \" + array[i] + \" \");      
        }

       println(silnia (4));
       println(fib(8));
        var str = \"kwakwa\";
        var n = 3;
        var k = n = 6;
        var aa = [   k, 4,5, true , 3.4 , \"aaaa\" ,1 ];      
        //println (\"\");
        //println((\"12 2 aa \"));
        println (aa);
        //println(aa[0]);
        printTab(aa);
        //aa(n++);
        var bb = 1;
        bb = [ aa[0],[silnia(3),2 + bb] ,str,str[1]];
        println (aa);
        println (bb);
        println(3 + aa  + 1 + false + \"dsds\");
        "
    Interpreter.Runner.run str
    Console.ReadLine() |> ignore
    0 
