#load "CNTKLoadscript.fsx"
open CNTKLoadscript

CNTKLoadscript.resolveCNTKDependencies ()

#r @"..\..\packages\FSharpAux\lib\netstandard2.0\FSharpAux.dll"
#r @"..\..\packages\FSharpAux.IO\lib\netstandard2.0\FSharpAux.IO.dll"
#r @"..\..\packages\FSharp.Stats\lib\netstandard2.0\FSharp.Stats.dll"
#I @"..\..\bin\BioFSharp.ML\netstandard2.0"
#r @"BioFSharp.dll"
#r "BioFSharp.IO"
#r "BioFSharp.ML"

open BioFSharp.ML.DPPOP
open BioFSharp.IO
open BioFSharp
open BioFSharp.IO.FastA
open FSharpAux
