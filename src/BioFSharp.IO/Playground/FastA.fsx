#r "../../../packages/FSharpAux/lib/netstandard2.0/FSharpAux.dll"
#r "../../../packages/FSharpAux.IO/lib/netstandard2.0/FSharpAux.IO.dll"
#r "../../../bin/BioFSharp/netstandard2.0/BioFSharp.dll"

#load "../FastA.fs"
open BioFSharp.IO.FastA

let test = createFastaItem "Test" "Sequence"

[test] |> toString id |> Array.ofSeq

open System.IO

let str = new MemoryStream ()

writeToStream id str [test]

let r = new StreamReader(str)

r.ReadToEnd()