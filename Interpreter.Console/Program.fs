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
//         Errors.createResult ErrorType.RuntimeError "error parsing"

[<EntryPoint>]
let main argv =
    let str =
        "
        var global = \"dsds\";
        
        fun fib ( n ) 
        {     
            //var re = 4;       
            if (n == 0 || n == 1) 
                return 1;            
            return fib( n - 1) + fib (n-2);
        } 
        fun silnia(n)
        {
           
            if (n < 2 && n > -1)
               return 1;                   
            return n * silnia(n-1);
        }
        fun aa(x)
        {     
            
              //  var x = 5;
             println(global);
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
            println(global);
            for(var i;i<len(array);++i)                            
                print(\" \" + array[i] + \" \");      
        }
        fun foo(array)
        {
              for(var i=0;i<len(array);++i)
                {
                    print(array[i] + \" \");
                    if(array[i] < 0)
                        return true;

                }
                return false;
                println(\"xx\");
        }
        var v = [1,2,-4];
              
        struct Point
        {
            var x = 0;
            var y = 0;
        }
                    
        struct Triangle
        {
            var p1 = Point{};
            var p2;
            var p3 = Point{};
        }
        struct Poly
        {
            var points = [Point{},Point{x=10,y=15}];
        }

        fun addPoints(p1,p2)
        {
            return Point{x = p1.x + p2.x,y = p1.y+p2.y};
        }
        fun createTriangle(p1,p2,p3)
        {
            return Triangle{p1=p1,p2=p2,p3=p3};
        }

        var p = Point{x=3};
        var p2 = Point{x = 10,y=15};
        var tr = Triangle { p1=Point{x=1,y=2} };
        tr = createTriangle(p,p2,p);
        println(tr);
        p.x = 99;
        println(tr);
        var poly = Poly {};
        println(poly);
//        println(p);
      //  println(p2);

        //println(poly.points[1].x);
       // var p3 = addPoints(p,p2);
        //println(p3);
        //println(tr);
        //println(poly);

        
        //println(foo(v));
        //printTab(v);
       // v[1] = 9;
        //println(v[0]);
        //var i = 5;
        //i = 10;
        //println(i);

        //var zz = 5;
        //var zzz = zz = 10;
        //println (zz);
        //println (zzz);

            //aa(10);
        println(fib(1));
        //println(foo([1,2,5,-9,7]));
       // var str = \"kwakwa\";
       // var n = 3;
       // var k = n = 6;
       // var aa = [   k, 4,5, true , 3.4 , \"aaaa\" ,1 ];      
       // //println (\"\");
       // //println((\"12 2 aa \"));
       //// println (aa);
       // //println(aa[0]);
       // printTab(aa);
       // //aa(n++);
       //// var bb = 1;
       // //bb = [ aa[0],[silnia(3),2 + bb] ,str,str[1]];
       //// println (aa);
       //// println (bb);
       //// println(3 + aa  + 1 + false + \"dsds\");
      //  var    z = [0];
       // z[0]++;
       "
    // let v = 5
    // let z = ref v |> List.singleton

    // printfn "%A" z.[0]

    // z.[0].Value <- 6

    // printfn "%A" z.[0]

    // let str =
    //     """
    //      fun fib ( n ) 
    //     {     
    //         //var re = 4;       
    //         if (n == 0 || n == 1) 
    //             return 1;            
    //         return fib( n - 1) + fib (n-2);
    //     } 
    //
    //            print(fib(5));         
    //             """

    Interpreter.Executor.run str |> printf "%A"
    Console.ReadLine() |> ignore
    0
