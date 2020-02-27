#nowarn "211"
//Adapted from https://github.com/fslaborg/Deedle/blob/master/src/Deedle/Deedle.fsx

// Standard NuGet or Paket location
#I "."
#I "lib/net45"

// Try various folders that people might like
#I "bin/net45"
#I "../bin/net45"
#I "../../bin/net45"
#I "../../bin/BioFSharp.IO/net45"
#I "../../../bin/net45"
#I "lib"

// Reference BioFSharp.IO
#r "BioFSharp.IO.dll"

open BioFSharp.IO

do fsi.AddPrinter(FSIPrinters.prettyPrintBioCollection)
do fsi.AddPrinter(FSIPrinters.prettyPrintBioItem)
do fsi.AddPrinter(FSIPrinters.prettyPrintClustal)
do fsi.AddPrinter(FSIPrinters.prettyPrintGFF3)
do fsi.AddPrinter(FSIPrinters.prettyPrintGPL)
do fsi.AddPrinter(FSIPrinters.prettyPrintGSE)
