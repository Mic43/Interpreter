namespace Interpretr.AST.Environment

open System.Collections.Generic
open Interpretr.AST.Types

type Global =
    { Functions: Dictionary<Identifier, Function> }

type Scoped = { Parent: Environment }

and EnvironmentKind =
    | Global of Global
    | Scoped of Scoped

and Environment =
    { Variables: Dictionary<Identifier, Value>
      Kind: EnvironmentKind }

module Environment =
    let createEmptyGlobal () =
        { Variables = new Dictionary<Identifier, Value>()
          Kind =
              { Functions = new Dictionary<Identifier, Function>() }
              |> EnvironmentKind.Global }

    let createNested parent variables =
        { Variables = new Dictionary<Identifier, Value>(variables |> Map.toSeq |> dict)
          Kind = { Parent = parent } |> EnvironmentKind.Scoped }
