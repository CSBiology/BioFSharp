#r "netstandard"
#r "../../packages/Newtonsoft.Json.11.0.2/lib/netstandard2.0/Newtonsoft.Json.dll"
#r "../../packages/System.Buffers/lib/netstandard2.0/System.Buffers.dll"
#r "../../packages/Docker.DotNet/lib/netstandard2.0/Docker.DotNet.dll"

#r "../../packages/SharpZipLib/lib/netstandard2.0/ICSharpCode.SharpZipLib.dll"

#r "../../packages/SharpZipLib/lib/netstandard2.0/ICSharpCode.SharpZipLib.dll"
#r "../../packages/FSharpAux.IO/lib/netstandard2.0/FSharpAux.dll"
#r "../../packages/FSharpAux.IO/lib/netstandard2.0/FSharpAux.IO.dll"

#load "Docker.fs"
#load "BioContainerIO.fs"
#load "BioContainer.fs"
#load "TargetP.fs"
#load "Blast.fs"
#load "ClustalO.fs"
#load "HMMER.fs"
#load "LastAlign.fs"

open System.Threading
open System.Threading
open Docker.DotNet
open System.Threading
open System.Buffers
open System.Threading.Tasks

open BioFSharp.BioContainers
open System.Collections.Generic
open Docker.DotNet.Models
open System.IO


open ICSharpCode.SharpZipLib.GZip
open ICSharpCode.SharpZipLib.Tar
open Newtonsoft.Json.Serialization
open System

let client = Docker.connect "npipe://./pipe/docker_engine"

Docker.Image.exists client (Docker.Tag (@"quay.io/biocontainers/fastp:0.20.0--hdbcaa40_0","0.20.0--hdbcaa40_0"))

type soos () = 
    
    static member InitImagesCreateParams
        (
            ?FromImage,
            ?FromSrc,
            ?Repo,
            ?Tag
        ) = 

        let param = new ImagesCreateParameters()                
        FromImage   |> Option.iter param.set_FromImage
        FromSrc     |> Option.iter param.set_FromSrc
        Repo        |> Option.iter param.set_Repo
        Tag         |> Option.iter param.set_Tag

        param

type saas<'T> () =
    interface IProgress<Docker.DotNet.Models.JSONMessage> with
        member _.Report(x) = 
            let status,error,progress =
                x.Status,x.ErrorMessage,x.ProgressMessage
            [status;error;progress]
            |> List.iter (fun m -> match m with |null -> () |message -> printfn "%s" message)



let pullImageAsync (connection:DockerClient) (imageName: string) =
    let param = soos.InitImagesCreateParams(FromSrc=imageName)
    async {
        let! tmp = 
            let myAuth = AuthConfig()
            myAuth.Password         <- ""
            myAuth.Username         <- "Mutagene"
            myAuth.Email    <- "kevin-schneider@mutagene.de"
            connection.Images.CreateImageAsync(param,myAuth,saas())
            |> Async.AwaitTask
        return tmp
        }

pullImageAsync client "registry-1.docker.io/v1/library/ubuntu/manifests/latest"
|> Async.RunSynchronously



let ubuntu = Docker.ImageName "ubuntu"

let bcContextUbuntu =
    BioContainer.initBcContextWithMountAsync client ubuntu "C:/tmp" 
    |> Async.RunSynchronously



BioContainer.disposeAsync bcContextUbuntu
|> Async.Start


let bcContext =
    BioContainer.initBcContextLocalDefaultAsync TargetP.ImageTargetP
    |> Async.RunSynchronously


let stream = new FileStream("C:/tmp/twelve.fsa",FileMode.Open)

let res = TargetP.run bcContext (TargetP.NonPlant) stream



BioContainer.disposeAsync bcContext
|> Async.Start



res |> Seq.head |> fun tp -> tp.Mtp





//// https://github.com/Microsoft/Docker.DotNet/issues/223 -> write
//// https://github.com/Microsoft/Docker.DotNet/issues/212 -> read

////-i, --interactive=false Keep STDIN open even if not attached
////-t, --tty=false Allocate a pseudo-TTY

//let exe = 
//    async {
//        //let! container =
//        //    let param = Docker.Container.ContainerParams.InitCreateContainerParameters(Image=dockerid.ToString(),Cmd=cmd,OpenStdin=true)
//        //    Docker.Container.createContainerWithAsync connection param      
        
//        //let! isRunning =
//        //    let param = 
//        //        Docker.Container.ContainerParams.InitContainerStartParameters()

//        //    Docker.Container.startContainerWithAsync connection param container.ID

//        let! execContainer =
//            let param = 
//                Docker.Container.ContainerParams.InitContainerExecCreateParameters(                                        
//                    AttachStderr=true,
//                    AttachStdout=true,                
//                    AttachStdin=false,
//                    Cmd=cmd',
//                    Detach=false
//                    //Tty=false
//                    )

//            Docker.Container.execCreateContainerAsync connection param (cont)
//        return tmp
//        }

//    |> Async.RunSynchronously


//////docker stop $(docker ps -a -q)

////Docker.Container.removeContainerAsync connection (Docker.DockerId.ContainerId (container.ID))  
////|> Async.RunSynchronously


//let ms = 
//    async {
        
//        let! execContainer =
//            let param = 
//                Docker.Container.ContainerParams.InitContainerExecCreateParameters(                                        
//                    AttachStderr=true,
//                    AttachStdout=true,                
//                    AttachStdin=false,
//                    Cmd=cmd,
//                    Detach=false                    
//                    )

//            Docker.Container.execCreateContainerAsync connection param (cont)

//        let! stream =
//            let param = 
//                Docker.Container.ContainerParams.InitContainerExecStartParameters(
//                    AttachStderr=true,
//                    AttachStdout=true,                
//                    AttachStdin=false,                   
//                    Cmd=cmd
//                    )                
//            Docker.Container.startContainerWithExecConfigAsync connection param cont // startContainerExecAsync connection exe.ID // 
            
//        printfn "Start Exec"
        
//        //let stopParam = new ContainerStopParameters()        
//        //let! st =  connection.Containers.StopContainerAsync(cont,stopParam) |> Async.AwaitTask
        
//        //printfn "Stop: %b" st
        
//        let stdOutputStream = new System.IO.MemoryStream()
//        let streamTask =
//            stream.CopyOutputToAsync(null,stdOutputStream,null,CancellationToken.None)             

                
//        do! streamTask |> Async.AwaitTask

//        printfn "Streamed"

//        //let! wait = 
//        //    Docker.Container.waitContainerAsync connection container.ID

//        let result =        
//            stdOutputStream.Position <- 0L
//            readFrom stdOutputStream
                    
//        //do! Docker.Container.removeContainerAsync connection (Docker.DockerId.ContainerId container.ID)  
    
//        return result
    
//    } 
//    |> Async.RunSynchronously



//let ms = 
//    async {
//        let! container =
//            let param = 
//                Docker.Container.ContainerParams.InitCreateContainerParameters(
//                    ArgsEscaped=false,
//                    AttachStderr=true,
//                    AttachStdout=true,                
//                    AttachStdin=false,
//                    Image=string dockerid,
//                    Cmd=cmd
//                    )

//            Docker.Container.createContainerWithAsync connection param              

//        //let! isRunning =
//        //    let param = Docker.Container.ContainerParams.InitContainerStartParameters()
//        //    Docker.Container.startContainerWithAsync connection param container.ID

//        let! stream = 
//            let param = Docker.Container.ContainerParams.InitContainerAttachParameters (Stdout=true,Stderr=true,Stdin=false,Stream=true)
//            connection.Containers.AttachContainerAsync(container.ID,false,param)
//            |> Async.AwaitTask
    
//        let stdOutputStream = new System.IO.MemoryStream()
//        let streamTask =
//            stream.CopyOutputToAsync(null,stdOutputStream,null,CancellationToken.None) 

//        let! isRunning =
//            let param = 
//                Docker.Container.ContainerParams.InitContainerExecStartParameters(
//                    AttachStderr=true,
//                    AttachStdout=true,                
//                    AttachStdin=false,                   
//                    Cmd=cmd
//                    )                
//            Docker.Container.startContainerWithExecConfigAsync connection param container.ID
                
//        do! streamTask |> Async.AwaitTask

//        let! wait = 
//            Docker.Container.waitContainerAsync connection container.ID

//        let result =        
//            stdOutputStream.Position <- 0L
//            readFrom stdOutputStream
                    
//        do! Docker.Container.removeContainerAsync connection (Docker.DockerId.ContainerId container.ID)  
    
//        return result
    
//    } 
//    |> Async.RunSynchronously



////let tmp =
////    BioContainer.runCmdAsync client (Docker.DockerId.ImageName "ubuntu") ["echo"; "hello world"]
////    |> Async.RunSynchronously
////    |> readFrom



//Docker.Image.exists client (Docker.DockerId.ImageName "targetp_image")


//Docker.Image.listImages client
//|> Seq.map (fun i -> i.ID )
//|> Seq.toArray


//Docker.Container.existsByImage client (Docker.DockerId.ImageName "targetp_image")


////ancestor=(<image-name>[:<tag>], <image id> or <image@digest>)

//let filters = 
//    Docker.Container.ContainerParams.InitContainerListParameters(All=true,Filters=Docker.Filters.InitContainerFilters(Ancestor=Docker.DockerId.ImageName "ubuntu"))


//Docker.Container.listContainersWithAsync client filters
//|> Async.RunSynchronously
//|> Seq.map (fun x -> x.Command,x.Image,x.Labels)
//|> Seq.toArray

////client.Containers.StartWithConfigContainerExecAsync
//let p = Docker.DotNet.Models.ContainerExecStartParameters()


//let ap = Docker.DotNet.Models.ContainerAttachParameters()




//Docker.Container.existsByImage client (Docker.DockerId.ImageName "targetp_image")


//let idtp = "61fbfbc30382e83dd585c99583c036ef8c5ced4eb10e1b274f199da6b6969588"

////let pipe = System.Uri("npipe://./pipe/docker_engine")

////let config = new DockerClientConfiguration(pipe)
////let client = config.CreateClient()

////let createByImage (client:DockerClient) imageName =
////    async {
////        let param = Models.CreateContainerParameters()
////        param.Image <- imageName
////        param.Cmd <- System.Collections.Generic.List(["echo"; "hello world"])
////        let! container =  
////            client.Containers.CreateContainerAsync (param,CancellationToken.None)
////            |> Async.AwaitTask
////        return container.ID
////    }


////let result =
////    async {
////        let paramLog = Models.ContainerLogsParameters() // (Stdout = System.Nullable<bool>(true),Stdin = System.Nullable<bool>(true))
////        paramLog.ShowStdout <- System.Nullable<bool>(true)
////        let paramRun = Models.ContainerStartParameters ()
        
////        //let id = 
////        //    "4243adc7f3832ea35bdaad79aabe86f8e1c54f5c3a799cc72e060a8402bc24cb"
        
////        let! id = createByImage client "ubuntu"

////        let! isRunnig =  
////            client.Containers.StartContainerAsync(id,paramRun,CancellationToken.None)
////            |> Async.AwaitTask
        
////        let! wait = 
////            client.Containers.WaitContainerAsync(id,CancellationToken.None)
////            |> Async.AwaitTask
        
////        let! logs =
////            client.Containers.GetContainerLogsAsync (id,paramLog,CancellationToken.None)
////            |> Async.AwaitTask

            
        
////        return logs
////    } 
////    |> Async.RunSynchronously


////let tmp : array<byte> = Array.zeroCreate 1024
////result.Read(tmp,0,1024)

////System.Text.Encoding.UTF8.GetString(tmp,0,1024)

// Include CsbScaffold
//#load "../../.env/CsbScaffold.fsx"
//#r @"C:\Users\Kevin\source\repos\CSBiology\BioFSharp\bin\BioFSharp.BioTools\net47\BioFSharp.BioTools.dll"

//open BioFSharp.BioTools

//open BioFSharp.IO
//open BioFSharp.IO.BlastNCBI
//open BioFSharp.IO.BlastNCBI.Parameters

//let typeOfDatabase = Parameters.MakeDbParams.DbType Parameters.Protein


//BlastWrapper(@"C:\Users\Kevin\source\repos\CSBiology\BioFSharp\lib\ncbi-blast\bin").makeblastdb @"C:\Users\Kevin\Source\Repos\CsbScaffold\Docker\data\Chlamy_Cp.fastA"  ([typeOfDatabase;] |> seq<Parameters.MakeDbParams>)

//let outputFormat= 
    
//    [   
//        OutputCustom.Query_SeqId; 
//        OutputCustom.Subject_SeqId;
//        OutputCustom.Query_Length;
//        OutputCustom.Subject_Length;
//        OutputCustom.AlignmentLength;
//        OutputCustom.MismatchCount;
//        OutputCustom.IdentityCount;
//        OutputCustom.PositiveScoringMatchCount;
//        OutputCustom.Evalue;
//        OutputCustom.Bitscore;
//    ] 
//    |> List.toSeq

//let outputType = OutputType.TabularWithComments

//let customOutputFormat = OutputTypeCustom(outputType , outputFormat)



//BlastNCBI.BlastWrapper(@"C:\Users\Kevin\source\repos\CSBiology\BioFSharp\lib\ncbi-blast\bin")
//    .blastP 
//        @"C:\Users\Kevin\Source\Repos\CsbScaffold\Docker\data\Chlamy_Cp.fastA" 
//        @"C:\Users\Kevin\Source\Repos\CsbScaffold\Docker\data\testQuery.fastA"
//        @"C:\Users\Kevin\Source\Repos\CsbScaffold\Docker\data/Output.txt"
//        ([customOutputFormat;] |> seq<BlastParams>)



open FSharpAux
open FSharpAux.IO
open FSharpAux.IO.SchemaReader.Attribute
open System.IO
open BioFSharp.BioContainers.BioContainer
open BioFSharp.BioContainers.BioContainerIO
open Blast

let client = Docker.connect "npipe://./pipe/docker_engine"

let ImageBlast = Docker.DockerId.ImageId "blast"

let blastContext = 
    BioContainer.initBcContextWithMountAsync client ImageBlast @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data"
    |> Async.RunSynchronously

let paramz =
    [
        MakeDbParams.DbType Protein
        MakeDbParams.Input @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data\Chlamy_Cp.fastA"
        MakeDbParams.Output@"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data\Chlamy_Cp.fastA"
    ]

let outputFormat= 
    
    [   
        OutputCustom.Query_SeqId; 
        OutputCustom.Subject_SeqId;
        OutputCustom.Query_Length;
        OutputCustom.Subject_Length;
        OutputCustom.AlignmentLength;
        OutputCustom.MismatchCount;
        OutputCustom.IdentityCount;
        OutputCustom.PositiveScoringMatchCount;
        OutputCustom.Evalue;
        OutputCustom.Bitscore;
    ] 

let blastPParamz = [
    BlastParams.SearchDB @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data\Chlamy_Cp.fastA"
    BlastParams.Query @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data\testQuery.fastA"
    BlastParams.Output @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data\Output.txt"
    OutputTypeCustom
        (
             OutputType.TabularWithComments,
             [   
                OutputCustom.Query_SeqId; 
                OutputCustom.Subject_SeqId;
                OutputCustom.Query_Length;
                OutputCustom.Subject_Length;
                OutputCustom.AlignmentLength;
                OutputCustom.MismatchCount;
                OutputCustom.IdentityCount;
                OutputCustom.PositiveScoringMatchCount;
                OutputCustom.Evalue;
                OutputCustom.Bitscore;
             ] 
        )
]

runMakeBlastDBAsync blastContext paramz
|> Async.RunSynchronously

runMakeBlastDB blastContext paramz

runBlastPAsync blastContext blastPParamz
|> Async.RunSynchronously


BioContainer.execAsync blastContext ["makeblastdb"; "-dbtype"; "prot" ;"-in"; "/data/C/Users/Kevin/Source/Repos/CsbScaffold/Docker/data/Chlamy_Cp.fastA"; "-out"; "/data/C/Users/Kevin/Source/Repos/CsbScaffold/Docker/data/Chlamy_Cp.fastA"]
|> Async.RunSynchronously

BioContainer.disposeAsync blastContext
|> Async.RunSynchronously


open ClustalO

let clustalImage = Docker.ImageName "clustal-omega"

let clustalContext = 
    BioContainer.initBcContextWithMountAsync client clustalImage @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data"
    |> Async.RunSynchronously

// ClustalO tests
let clustalOParamz = [
    ClustalOParams.Input 
        (
            FileInput.SequenceFile @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data\Chlamy_Cp.fastA",
            [
                InputCustom.Format FileFormat.FastA
            ]
        )
    ClustalOParams.Output 
        (
            @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data\Chlamy_Cp.aln",
            []
        )
    ClustalOParams.Miscellaneous 
        [
            MiscellaneousCustom.Force
        ]
]

runClustalO clustalContext clustalOParamz

open HMMER
open HMMER.HMMbuild

let HMMERImage =  Docker.ImageName "hmmer"

let hmmerContext = 
    BioContainer.initBcContextWithMountAsync client HMMERImage @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data"
    |> Async.RunSynchronously

let hmmbuildParamz = 
    [
        InputMSAFile @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data\hmmer_testfiles\globins4.sto"
        OutputHMMFile @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data\hmmer_testfiles\testOutput.hmm"
    ]

runHMMbuild hmmerContext hmmbuildParamz



let imageLastAlign = Docker.ImageName "last-align"

let lastAlignContext = 
    BioContainer.initBcContextWithMountAsync client imageLastAlign @"C:\Users\kevin\Desktop\Microbiology_CrossGenomics\Data\Genomes"
    |> Async.RunSynchronously


open LastAlign

let lastDBParameters =
    [
        LastDBParams.Input
            (InputOptions.Single @"C:\Users\kevin\Desktop\Microbiology_CrossGenomics\Data\Genomes\GCF_000091205.1_ASM9120v1_genomic.fna")
        LastDBParams.OutputName
            @"C:\Users\kevin\Desktop\Microbiology_CrossGenomics\Data\Genomes\CyanidioschyzonDB"

    ]

runLastDBAsync lastAlignContext lastDBParameters
|> Async.RunSynchronously

BioContainer.disposeAsync lastAlignContext |> Async.RunSynchronously

let alignParams = 
    [
        LastAlignParameters.DBName
            @"C:\Users\kevin\Desktop\Microbiology_CrossGenomics\Data\Genomes\GaldieriaDB"
        LastAlignParameters.Query
            (QueryOptions.SingleFastaFile @"C:\Users\kevin\Desktop\Microbiology_CrossGenomics\Data\Genomes\GCF_000091205.1_ASM9120v1_genomic.fna")
        LastAlignParameters.OutputFileName
            @"C:\Users\kevin\Desktop\Microbiology_CrossGenomics\Data\Genomes\GenomeAlignment.maf"
        LastAlignParameters.Verbose
    ]

runLastAlignAsync lastAlignContext alignParams
|> Async.RunSynchronously
|> fun x -> File.WriteAllLines(@"C:\Users\kevin\Desktop\Microbiology_CrossGenomics\Data\Genomes\GenomeAlignment.maf",x.Split([|"\r\n";"\r";"\n"|],StringSplitOptions.None))