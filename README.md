# Intepreter

C-like syntax, dynamically typed language interpreter written entirely in F# <BR>
Support for basic control statements, operators, defining functions and C-like structures. <BR>
Comments and basic error messages are supported, as well as some basic semantic analysis. <BR> <BR>
Sample code: <BR> <BR>
<code>fun fib (n) {     
           <t> //var re = 4;       
           <t> if (n == 0 || n == 1) 
          <t>      return 1;            
          <t>  return fib( n - 1) + fib (n-2);
       }  
        println(fib(5));
        </code>
