#r "../../../packages/FSharpAux/lib/netstandard2.0/FSharpAux.dll"
#r "../../../packages/FSharpAux.IO/lib/netstandard2.0/FSharpAux.IO.dll"
#r "../../../bin/BioFSharp/netstandard2.0/BioFSharp.dll"

#load "../Obo.fs"

open FSharpAux
open FSharpAux.IO
open BioFSharp.IO.Obo


let oboPath = __SOURCE_DIRECTORY__ + @"../../../../docsrc/content/data/testTerm.obo"

let testTerm =
    Seq.fromFile oboPath
    |> parseOboTerms
    |> Array.ofSeq
    |> Array.item 0

testTerm.Synonyms

let msOntologyTerms = 
    __SOURCE_DIRECTORY__ + @"../../../../docsrc/content/data/ms.obo"
    |> Seq.fromFile
    |> parseOboTerms
    |> Array.ofSeq