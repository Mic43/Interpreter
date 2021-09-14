module Tests

open System
open Xunit
open Interpretr.AST.Runner
open Interpretr.AST.Types


[<Fact>]
let ``Dict test`` () =
    
        //let env = Environment.create ()    
        //match env with 
        //    | Environment.Specific.Global g -> g.Functions.Add(Interpretr.AST.Types.Identifier.tryCreateIdentifier "aa",Value.IntValue 5)
    let r = new Runner();
    r.Run
    Assert.True(true)
