#r "../../../packages/FSharpAux/lib/netstandard2.0/FSharpAux.dll"
#r "../../../packages/FSharpAux.IO/lib/netstandard2.0/FSharpAux.IO.dll"
#r "../../../bin/BioFSharp/netstandard2.0/BioFSharp.dll"

#load "../SOFT.fs"
open FSharpAux
open FSharpAux.IO
open BioFSharp.IO.SOFT

let s = 
    Series.fromFile 
        @"D:\Datascience\projects\EntropyDataAnalysis\data\Ostreococcus\GSE16422_family.soft"

Seq.fromFile @"D:\Datascience\projects\EntropyDataAnalysis\data\Ostreococcus\GSE16422_family.soft"
|> Seq.map Tokenization.tokenizeSOFTLine
|> Seq.choose ( fun token ->
    match token with
    | Tokenization.Attribute (a,v) when a.Contains("abel") -> Some (a,v)
    | _ -> None
)
|> Array.ofSeq