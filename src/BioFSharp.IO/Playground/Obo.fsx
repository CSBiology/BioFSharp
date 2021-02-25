#r "nuget: FSharpAux"
#r "nuget: FSharpAux.IO"
#r "../../../bin/BioFSharp/netstandard2.0/BioFSharp.dll"

#load "../Obo.fs"

open FSharpAux
open FSharpAux.IO
open BioFSharp.IO.Obo

let oboPath = __SOURCE_DIRECTORY__ + @"../../../../docsrc/content/data/ms.obo"

let testTerm =
    Seq.fromFile oboPath
    |> parseOboTerms
    |> Seq.find( fun obo ->
        obo.Id = "MS:1002062"
    )

testTerm.Synonyms

let msOntologyTerms = 
    __SOURCE_DIRECTORY__ + @"../../../../docsrc/content/data/ms.obo"
    |> Seq.fromFile
    |> parseOboTerms
    |> Array.ofSeq