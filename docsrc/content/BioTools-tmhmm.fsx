(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "netstandard"
#r "../../packages/Newtonsoft.Json.10.0.3/lib/netstandard1.3/Newtonsoft.Json.dll"
#r "../../packages/System.Buffers/lib/netstandard2.0/System.Buffers.dll"
#r "../../packages/Docker.DotNet/lib/netstandard2.0/Docker.DotNet.dll"
#r "../../packages/SharpZipLib/lib/netstandard2.0/ICSharpCode.SharpZipLib.dll"
#r "../../packages/FSharpAux.IO/lib/netstandard2.0/FSharpAux.dll"
#r "../../packages/FSharpAux.IO/lib/netstandard2.0/FSharpAux.IO.dll"
#r @"../../lib/Formatting/FSharp.Plotly.dll"

#I @"../../bin/BioFSharp/netstandard2.0/"
#I @"../../bin/BioFSharp.IO/netstandard2.0/"
#I @"../../bin/BioFSharp.BioTools/netstandard2.0/"
#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"
#r "BioFSharp.BioTools.dll"


open System.IO
open BioFSharp.BioTools


let client = Docker.connect "npipe://./pipe/docker_engine"



//let tmhmm = Docker.ImageName "tmhmm"

//let bcContextUbuntu =
//    BioContainer.initBcContextWithMountAsync client ubuntu "C:/tmp" 
//    |> Async.RunSynchronously



//BioContainer.disposeAsync bcContextUbuntu
//|> Async.Start


let bcContext =
    BioContainer.initBcContextLocalDefaultAsync Tmhmm.ImageTmhmm
    |> Async.RunSynchronously


let stream = new FileStream("C:/tmp/seq.fasta",FileMode.Open)

let res = Tmhmm.run bcContext stream


BioContainer.disposeAsync bcContext
|> Async.Start



res |> Seq.head |> fun tp -> tp.Topology





//let bcContext =
//    BioContainer.initBcContextLocalDefaultAsync TargetP.ImageTagetP
//    |> Async.RunSynchronously


//let stream = new FileStream("C:/tmp/twelve.fsa",FileMode.Open)

//let res = TargetP.run bcContext (TargetP.NonPlant) stream



//BioContainer.disposeAsync bcContext
//|> Async.Start



//res |> Seq.head |> fun tp -> tp.Mtp