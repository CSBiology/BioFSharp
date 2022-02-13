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
#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"
#r "FSharpAux.dll"
#r "FSharpAux.IO.dll"
(**
OBO format is a popular ontology file format. It can express a subset of the description logic language
OWL-DL 2.0 but in addition has standard syntax for representing important classes of meta-data including
as synonyms, references to publications and deprecated IDs. It is designed to be human readable and 
editable.

Reading Obo files
-------------------
*)


open FSharpAux.IO
open BioFSharp.IO

let fileDir = __SOURCE_DIRECTORY__ + "/data/"  

let psiMs = 
    Seq.fromFile (fileDir + "Psi-MS.obo")
    |> Obo.parseOboTerms true
    |> Seq.toArray


