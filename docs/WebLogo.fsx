(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I @"../../bin/BioFSharp/net47/"
#I @"../../bin/BioFSharp.BioDB/net47/"
#I @"../../bin/BioFSharp.ImgP/net47"
#I @"../../bin/BioFSharp.IO/net47/"
#I @"../../bin/BioFSharp.Parallel/net47/"
#I @"../../bin/BioFSharp.Stats/net47/"
#I @"../../bin/BioFSharp.Vis/net47/"
#r @"../../packages/formatting/FSharp.Plotly/lib/netstandard2.0/FSharp.Plotly.dll"
(**
BioFSharp
=========


*)

#r "FSharpAux.dll"

open FSharpAux


#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"
#r "BioFSharp.Vis.dll"
open BioFSharp
open BioFSharp.Vis



let a = set [ "Earth"; "Jupiter"; "Mars"; "Mercury"; ]
let b = set [ "Earth";  "Mercury"; ]
let c = set [ "Jupiter"; "Mars"; "Mercury"; ]

let label  = [|"A"; "B"; "C"|]
let colors = 
    [| 
        Colors.Table.Office.grey;
        Colors.Table.Office.blue ;
        Colors.Table.Office.yellow;
    |] |> Array.map Colors.toWebColor


let venn = Venn.ofSetList label [|a; b; c|]


let chord = 
    Venn.toChordConnections label venn
    |> Venn.chordConnectionsToString


BioFSharp.Vis.Chord.show 1 450 450  label  colors chord


BioFSharp.Vis.Chord.showInBrowser 1 450 450  label  colors chord




(**
Example
-------

The following example shows how easy it is to start working with sequences:
*)
