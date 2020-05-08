(*** hide ***)
#r @"C:\Users\kevin\source\repos\CSBiology\BioFSharp\packages\Docker.DotNet\lib\netstandard2.0\Docker.DotNet.dll"
#r @"../../packages\Newtonsoft.Json\lib\netstandard2.0\Newtonsoft.Json.dll"
#r "../../packages/System.Buffers/lib/netstandard2.0/System.Buffers.dll"
#r "../../packages/Docker.DotNet/lib/netstandard2.0/Docker.DotNet.dll"

#r "../../packages/SharpZipLib/lib/netstandard2.0/ICSharpCode.SharpZipLib.dll"

#I @"../../bin/BioFSharp/netstandard2.0/"
#I @"../../bin/BioFSharp.IO/netstandard2.0/"
#I @"../../bin/BioFSharp.BioContainers/netstandard2.0/"

#r @"../../packages/formatting/FSharp.Plotly/lib/netstandard2.0/FSharp.Plotly.dll"
#r @"../../packages/FSharpAux/lib/netstandard2.0/FSharpAux.dll"
#r @"../../packages/FSharpAux.IO/lib/netstandard2.0/FSharpAux.IO.dll"

#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"
#r "BioFSharp.BioContainers.dll"

(** 
TargetP 1.1 is a widely used tool to predict subcellular localization of proteins by predicting N-terminal presequences.
We can leverage the power of targetP from F# by using it in a docker container. To get academical access to the targetP software,
please contact the friendly people at [DTU](https://services.healthtech.dtu.dk/software.php).

After aquiring the softare you can create a dockerfile at the packages root and run 

`docker build . -t nameOfYourContainer:yourTag` 

to get the image needed.

```(docker)
################## BASE IMAGE ######################

FROM biocontainers/biocontainers

################## METADATA ######################
LABEL base_image="biocontainers:v1.0.0_cv4"
LABEL version="3"
LABEL software="TargetP"
LABEL software.version="1.1.0"
LABEL about.summary="TargetP 1.1 predicts the subcellular location of eukaryotic proteins"
LABEL about.home="http://www.cbs.dtu.dk/services/TargetP/"
LABEL about.documentation="http://www.cbs.dtu.dk/services/TargetP/instructions.php"
LABEL about.license_file="http://www.cbs.dtu.dk/cgi-bin/nph-sw_request?targetp"
LABEL extra.identifiers.biotools="TargetP"
LABEL about.tags="Sequence analysis"

#################### INSTALL ########################

ENV PATH="/usr/local/bin:${PATH}"
# TargetP perl script
ADD ./targetp_BioFSharp /usr/local/bin/targetp
# TargetP install
ADD ./targetp-1.1b.Linux.tar /opt/
# ChloroP install
ADD ./chlorop-1.1.Linux.tar /opt/
# SignalP install
ADD ./signalp-4.1f.Linux.tar.gz /opt/
```
*)

(** 
As always, we need to define the docker client endpoint, image, and container context to run:
*)
(***do-not-eval***)
open BioFSharp.BioContainers

///docker daemon endpoint on windows
let client = Docker.connect "npipe://./pipe/docker_engine"

///image to create containers from
let image  = Docker.DockerId.ImageId "nameOfYourContainer:yourTag"

///The container context we will use to execute targetP

(***do-not-eval***)
let imageContext = 
    BioContainer.initBcContextWithMountAsync client image "path/to/your/directory"
    |> Async.RunSynchronously


(** 
To analyze a file with the container, we can use the `runWithMountedFile` function to work on a fasta file in the mounted directory. 
The file can be either coming from outside or upstream analysis pipelines using BioFSharp and written to disk by `FastA.write`.

__Note: this function is available from version 2.0.0 onwards.__
*)

(***do-not-eval***)
let results = 
    TargetP.runWithMountedFile
        imageContext 
        TargetP.TargetpParams.NonPlant
        @"C:\Users\kevin\Desktop\TargetPdocker\targetp-1.1b.Linux\targetp-1.1\test\one.fsa"
    |> Seq.item 0

//dont forget to get rid of the container after using it

imageContext
|> BioContainer.disposeAsync
|> Async.RunSynchronously

(**

Here is an example result for the pathogenesis-related (homeodomain protein)[https://www.uniprot.org/uniprot/P48786.fasta]:

<pre>
val it : TargetP.TargetpItem = { 
    Name = "P48786;"
    Len = 1088
    Mtp = 0.054
    Ctp = nan
    SP = 0.068
    Other = 0.943
    Loc = "_"
    RC = 1
    TPlen = "" 
}
</pre>
*)

(**
It may not always be convenient to analyze files on the disk. To use a memory stream of a 
`FastAItem` instead, we can write it to a stream and analyze it using the `runWithStream` function

__Note: in versions before 2.0.0, this function is named `run`__
*)
open System.IO
open BioFSharp.IO

let testFasta = 
    FastA.createFastaItem "Header" "Sequence"

(***include-value:testFasta***)

let stream = new MemoryStream()

[testFasta]
|> FastA.writeToStream id stream

//reset position of the memory stream
stream.Position <- 0L

(***do-not-eval***)
let streamRes = 
    TargetP.runWithStream imageContext TargetP.TargetpParams.NonPlant stream
    |> Array.ofSeq

(**
<pre>
val it : TargetP.TargetpItem = { 
    Name = "Header"
    Len = 8
    Mtp = 0.056
    Ctp = nan
    SP = 0.033
    Other = 0.975
    Loc = "_"
    RC = 1
    TPlen = "" 
}
</pre>
*)
