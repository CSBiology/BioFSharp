#light
#time
#r @"C:\Users\Kevin-Laptop\Documents\GitHub\BioFSharp\bin\BioFSharp.dll"
#r @"C:\Users\Kevin-Laptop\Documents\GitHub\BioFSharp\bin\BioFSharp.IO.dll"


open BioFSharp
open BioFSharp.IO
open BioFSharp.Algorithm


let pattern = "lol" |> Array.ofSeq
let text = "ololllllllllllllllllllllllllllllllllllllllolllllllllllllllllllllllllllllolollllllol" |> Array.ofSeq

let rabinKarpSearch = BioFSharp.Algorithm.RabinKarp.SearchAll
let kMpSearch = BioFSharp.Algorithm.KnuthMorrisPratt.initFindAllMatches pattern

rabinKarpSearch pattern text 0 []
kMpSearch text
