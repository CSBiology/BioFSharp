(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
// #I "../../bin"
// #r "../../bin/BioFSharp.dll"
// #r "../../bin/BioFSharp.BioDB.dll"
// #r "../../bin/BioFSharp.IO.dll"
// #r "../../bin/FSharp.Care.dll"
// #r "../../bin/FSharp.Care.IO.dll"


#r "../../bin/SwaggerProvider.dll"
#r "../../bin/SwaggerProvider.DesignTime.dll"
#r "../../bin/SwaggerProvider.Runtime.dll"


(**
Accessing online databases
==============================
BioFSharp contains a set of reader that facilitat the access to different biological online resources. This documentation aims to give an introduction for them.


<a name="UniProt"></a>
UniProt's Proteins REST API online access
-----------------------------------------
*)

(*** hide ***)
// open System
// open BioFSharp
// open BioFSharp.IO
// open BioFSharp.BioDB
// open FSharp.Care
// open FSharp.Care.IO

(**
The Proteins REST API provides access to key biological data from UniProt and data from Large Scale Studies data mapped to UniProt. 
The services provide sequence feature annotations from UniProtKB, variation data from UniProtKB and mapped from Large Scale data sources (1000 Genomes, ExAC and COSMIC), 
proteomics data mapped from Large Scale sources (PeptideAtlas, MaxQB and EPD) and genome coordinate mappings

*)

/// 
open SwaggerProvider
let [<Literal>]schemaURL = "D:/Source/BioFSharp/src/BioFSharp.BioDB/Resources/ebiProteinsAPIswagger.json"

type ProteinsAPIschema = SwaggerProvider<schemaURL>
let proteinsAPI = ProteinsAPIschema()


let protein = proteinsAPI.GetByAccession2 ("Q64446") 
protein.Sequence









(**


<hr>


<a name="BioDB"></a>
BioDB
------------------------------
This documentation is not yet here. Hopefully it will be soon =)

*)






