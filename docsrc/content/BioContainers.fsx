(*** hide ***)
#I @"../../bin/BioFSharp/net47/"
#I @"../../bin/BioFSharp.BioDB/net45/"
#I @"../../bin/BioFSharp.ImgP/net47"
#I @"../../bin/BioFSharp.IO/net47/"
#I @"../../bin/BioFSharp.Parallel/net47/"
#I @"../../bin/BioFSharp.Stats/net47/"
#I @"../../bin/BioFSharp.Vis/net47/"
#r @"../../lib/Formatting/FSharp.Plotly.dll"
#I @"../../bin/BioFSharp.BioContainers/net47/"
#r @"C:\Users\kevin\source\repos\CSBiology\BioFSharp\packages\Docker.DotNet\lib\netstandard2.0\Docker.DotNet.dll"
#r "BioFSharp.dll"
#r "BioFSharp.BioContainers.dll"

(**

BioFSharp.BioContainers
=======================

BioFSharp.BioContainers is all about connecting BioFSharp and F# itself with the Bioinformatics community and integrating 
common bioinformatic workflows.There are many established tools and software suites in bioinformatics, and most of the time 
there is no point in rewriting them for a specific programming language. Containerized applications make it possible to use 
all these tools while being OS and programming language agnostic - meaning you can use containerized linux tools in your
windows based pipeline, python applications in containers followed by a containerized C++ application and so on. Take a look 
at this graphic to get an overview of how BioFSharp.BioContainers works in principle:

![BioContainers_Overview](img/BioContainers_Overview.png)

BioFSharp.BioContainers gives you the possibility to leverage containerized applications without leaving you F# environment.
We build on the fondation of [Docker.DotNet][dockerdotnet] to programmatically access the the REST API on top of the docker daemon 
**(1 + 2)**. The daemon then does its thing, executing the commands given, for example creating containers, executing computations
in a container, etc **(3)**. We provide special functions to use with [biocontainers][biocontainers], which is a standardized way to create 
containerized bioinformatic software **(4)**. The results are then returned via the daemon and REST API back to you F# interactive **(5)**

[biocontainers]: https://biocontainers-edu.readthedocs.io/en/latest/what_is_biocontainers.html
[dockerdotnet]: https://github.com/microsoft/Docker.DotNet

The project is in its early stages and many features provided by the REST API are not fully implemented yet, so if you would like
to help out, this is a great place to do so!

Prerequisites
-------------

**Windows**

 - Install [Docker Desktop for windows][docker-win]

[docker-win]: https://docs.docker.com/docker-for-windows/install/

**Linux**

 - Not tested yet, but If there is a way to use a named pipe as in windows, everything should work as it does on windows.

General Usage
-------------

lets say we have Docker for Windows set up and pulled an ubuntu image (docker pull ubuntu). To connect with the Rest API, first use the 
`Docker.connect` function to initialize a Docker Client.

*)

(*** do-not-eval ***)
open BioFSharp.BioContainers

///npipe://./pipe/docker_engine is the named pipe for the docker engine under windows.
let client = Docker.connect "npipe://./pipe/docker_engine"

(** 
Some vanilla docker commands are already implemented. here is an example of listing information about all images from F# interactive 
(equivalent to `docker images ls -a` in docker cli)
*)

(*** do-not-eval ***)

client
|> Docker.Image.listImages
|> fun images ->
    printfn "Repository/Tags\tImageId\tCreated\t Size"
    images 
    |> Seq.iter (fun img -> printfn "%A\t%s\t%A\t %A" img.RepoTags img.ID img.Created img.Size)

(**
Output:

```
    Repository/Tags         ImageId                                                                     Created                 Size
    seq ["ubuntu:latest"]   sha256:cf0f3ca922e08045795f67138b394c7287fbc0f4842ee39244a1a1aaca8c5e1c     10/18/2019 6:48:51 PM   64192156L

```

Creating containers and executing commands in them
--------------------------------------------------

To create a container from an existing image, initialite a `BioContainer.BcContext`. That way, a new
container will not only be spawned, but kept running to receive your commands.

*)
(***do-not-eval***)

///Create a representation of your image on the F# side
let ubuntuImage = Docker.DockerId.ImageId "ubuntu:latest"

///Create a container from the image and keep it running (the container context for all future commands)
let bcContext = 
    BioContainer.initBcContextAsync client ubuntuImage
    |> Async.RunSynchronously


(**
To run a command in the container, use either `BioContainer.execAsync` to run the command, only printing
stdout/stderr to the F# interactive, or `BioCOntainer.execReturnAsync` to get stdout/stderr as a string that 
you can bind to a name
*)

(***do-not-eval***)

//just run the command and print the results
BioContainer.execAsync bcContext ["echo";"hello world"]
|> Async.RunSynchronously

///bind the results of the command to a value
let returnValue =
    BioContainer.execReturnAsync bcContext ["echo";"hello world"]
    |> Async.RunSynchronously

(** 
Don't forget, the container will be kept running, so dispose it if you do not need it anymore to
prevent it from eating up ressources
*)

(***do-not-eval***)
bcContext
|> BioContainer.disposeAsync
|> Async.RunSynchronously


(**


Using actual biocontainers
--------------------------

To leverage the advantages of F#, namingly its type safety, passing commands as strings is not enough.
The tools in BioContainers come with extensive documentation of input commands and allowed/parsed types.
We here propose a type safe modelling of the commands, with a little bit of overhead to ensure correct
paths (going from windows to linux paths in the containers). We already provide API wrappers for some tools,
such as BLAST, TMHMM, or intaRNA. If you want to create your own, please refer to the [design guide]().

Here is a short example for BLAST (indepth information about this the type modelling of this tool can be found [here]():

First you have to either pull the BLAST BioContainer image either via docker cli or by building them from the docker file
A way to do this from F# is in the making.

The protein fasta used here can be found [here]()
*)

(***do-not-eval***)
open BioFSharp.BioContainers.Blast

let ImageBlast = Docker.DockerId.ImageId "blast"

///this time, we set the container up using a mount, making sure that it can access data from the file system we want to use.
let blastContext = 
    BioContainer.initBcContextWithMountAsync 
        client 
        ImageBlast 
        @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data"
    |> Async.RunSynchronously

///parameters for search DB creation
let makeDBParams =
    [
        MakeDbParams.DbType Protein
        MakeDbParams.Input @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data\Chlamy_Cp.fastA"
        MakeDbParams.Output@"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data\Chlamy_Cp.fastA"
    ]

///parameters for blastP
let blastPParams = [
    BlastParams.SearchDB @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data\Chlamy_Cp.fastA"
    BlastParams.Query @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data\testQuery.fastA"
    BlastParams.Output @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data\Output.txt"
    BlastParams.OutputType OutputType.TabularWithComments
]
