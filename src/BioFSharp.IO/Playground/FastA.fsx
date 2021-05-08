#r "nuget: FSharpAux"
#r "nuget: FSharpAux.IO"
#r "../../../bin/BioFSharp/netstandard2.0/BioFSharp.dll"

#load "../FastA.fs"

open BioFSharp.IO.FastA

// check if we can still read the stream created by writeToStream

let test = createFastaItem "Test" "Sequence"

[test] |> toString id |> Array.ofSeq

open System.IO

let str = new MemoryStream ()

writeToStream id str [test]

let r = new StreamReader(str)

r.ReadToEnd()

// check if encoding is without BOM

[test]
|> write id @"D:\scratch\test.fsa"