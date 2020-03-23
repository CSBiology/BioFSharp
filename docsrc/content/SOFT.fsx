(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I @"../../bin/BioFSharp/net47/"
#I @"../../bin/BioFSharp.BioDB/net45/"
#I @"../../bin/BioFSharp.ImgP/net47"
#I @"../../bin/BioFSharp.IO/net47/"
#I @"../../bin/BioFSharp.Parallel/net47/"
#I @"../../bin/BioFSharp.Stats/net47/"
#I @"../../bin/BioFSharp.Vis/net47/"
#r @"../../lib/Formatting/FSharp.Plotly.dll"
#r "FSharpAux.dll"
#r "FSharpAux.IO.dll"
#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"

open BioFSharp

open FSharpAux

(**
Parsing SOFT formatted family files
===================================

GEO (Gene Expression Omnibus) is a data repository of high-throughput gene expression and hybridization array data. All record metadata are provided in the [soft format](https://www.ncbi.nlm.nih.gov/geo/info/soft.html) ,
which can be parsed and analyzed using the `BioFSharp.IO.SOFT` module.

SOFT types are very large and hard to read from standard FSI output, especially the nested top level types, so we will omitt the standard output for readability and use 
the FSI printers provided in `BioFSharp.IO.FSIPrinters`.
*)

open BioFSharp.IO

//register the desired printers
fsi.AddPrinter(FSIPrinters.prettyPrintGPL)
fsi.AddPrinter(FSIPrinters.prettyPrintGSE)

fsi.AddPrinter(FSIPrinters.prettyPrintSampleRecord)
fsi.AddPrinter(FSIPrinters.prettyPrintSeriesRecord)
fsi.AddPrinter(FSIPrinters.prettyPrintPlatformRecord)


(**
Parsing platform (GPL) files
----------------------------

Soft formatted platform family files can be parsed using the `SOFT.Platform.fromFile` function. 

As the format (.soft) does not specify the type of record (GSE or GPL), please make sure that you only 
parse GPL*.soft files with this functions, as other files may return errors
*)

let testPlatform = SOFT.Platform.fromFile (__SOURCE_DIRECTORY__ + "/data/GPL15922_family.soft")

(**
Parsing series (GSE) files
--------------------------

Soft formatted series family files can be parsed using the `SOFT.Platform.fromFile` function. 

As the format (.soft) does not specify the type of record (GSE or GPL), please make sure that you only 
parse GSE*.soft files with this functions, as other files may return errors
*)

let testSeries = SOFT.Series.fromFile (__SOURCE_DIRECTORY__ + "/data/GSE71469_family.soft")

(** 
Convenience functions
---------------------

We implemented some convenience functions for `SOFT.Platform.GPL` and `SOFT.Series.GSE`.

`Platform.getAssociatedSampleAccessions` for example retrieves all associated sample accessions. This could be usefull for batch downloads of these files.
*)

let sampleAccessions =
    testPlatform |> SOFT.Platform.getAssociatedSampleAccessions

(*** include-value:sampleAccessions ***)

(**
This can be especially usefull to retrieve all samples that are associated with this platform (e.g. for example for meta analysis of the files).

The full sample records can also be retrieved, which makes it possible to access even more metadata:
*)

let relations =
    testPlatform 
    |> SOFT.Platform.getAssociatedSamples
    |> List.map (fun x -> x.Relation)
    //showing only the first 5 relations for ease of view
    |> fun x -> x.[0..4]

(*** include-value:relations ***)
