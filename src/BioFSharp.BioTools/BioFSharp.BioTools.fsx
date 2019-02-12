#r "netstandard"
#r "../../packages/Newtonsoft.Json.10.0.3/lib/netstandard1.3/Newtonsoft.Json.dll"
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

open System.Threading
open System.Threading
open Docker.DotNet
open System.Threading
open System.Buffers
open System.Threading.Tasks

open BioFSharp.BioTools
open System.Collections.Generic
open Docker.DotNet.Models
open System.IO


open ICSharpCode.SharpZipLib.GZip
open ICSharpCode.SharpZipLib.Tar
open Newtonsoft.Json.Serialization
open System



let client = Docker.connect "npipe://./pipe/docker_engine"



let ubuntu = Docker.ImageName "ubuntu"

let bcContextUbuntu =
    BioContainer.initBcContextWithMountAsync client ubuntu "C:/tmp" 
    |> Async.RunSynchronously



BioContainer.disposeAsync bcContextUbuntu
|> Async.Start


let bcContext =
    BioContainer.initBcContextLocalDefaultAsync TargetP.ImageTagetP
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
open BioFSharp.BioTools.BioContainer
open BioFSharp.BioTools.BioContainerIO

type DbType =
    | Protein 
    | Nucleotide

    static member make = function
        | Protein       -> "prot"
        | Nucleotide    -> "nucl"


type MakeDbParams =
    | Input  of string
    | Output of string
    | DbType of DbType
    | MaskData of string
    | ParseSeqIds    

    static member makeCmdWith (m: MountInfo) = function
        | Input  (path)     -> ["-in"  ;(MountInfo.containerPathOf m path)]
        | Output (path)     -> ["-out" ;(MountInfo.containerPathOf m path)]
        | DbType (dbt)      -> ["-dbtype"; (DbType.make dbt)]
        | MaskData (path)   -> ["-mask_data"; (sprintf "%s.asnb") (MountInfo.containerPathOf m path)]
        | ParseSeqIds       -> ["-parse_seqids"] 


type OutputType = 
    | Pairwise                        
    | Query_anchored                  
    | Query_anchored_NoIdentities     
    | Query_anchored_Flat             
    | Query_anchored_Flat_NoIdentities
    | XML                             
    | Tabular                         
    | TabularWithComments             
    | TextASN1                        
    | BinaryASN1                      
    | CSV                             
    | BLAST_ArchiveFormat             
    | JSON_Seqalign                   
    | JSON_Blast                      
    | XML2_Blast                      

        static member make = function 
            | Pairwise                          ->  0
            | Query_anchored                    ->  1
            | Query_anchored_NoIdentities       ->  2
            | Query_anchored_Flat               ->  3
            | Query_anchored_Flat_NoIdentities  ->  4
            | XML                               ->  5
            | Tabular                           ->  6
            | TabularWithComments               ->  7
            | TextASN1                          ->  8
            | BinaryASN1                        ->  9
            | CSV                               -> 10
            | BLAST_ArchiveFormat               -> 11
            | JSON_Seqalign                     -> 12
            | JSON_Blast                        -> 13
            | XML2_Blast                        -> 14

//When not provided, the default value is:
//'qseqid sseqid pident length mismatch gapopen qstart qend sstart send
//evalue bitscore', which is equivalent to the keyword 'std'
type OutputCustom = 
    | Query_SeqId               
    | Query_GI                  
    | Query_Accesion            
    | Query_Accesion_Version    
    | Query_Length              
    | Subject_SeqId             
    | Subject_All_SeqIds        
    | Subject_GI                
    | Subject_All_GIs           
    | Subject_Accession         
    | Subject_Accession_Version 
    | Subject_All_Accession     
    | Subject_Length            
    | Query_StartOfAlignment    
    | Query_EndOfAlignment      
    | Subject_StartOfAlignment  
    | Subject_EndOfAlignment    
    | Query_AlignedPartOf       
    | Subject_AlignedPartOf     
    | Evalue                    
    | Bitscore                  
    | RawScore                  
    | AlignmentLength           
    | Identity                  
    | IdentityCount             
    | MismatchCount             
    | PositiveScoringMatchCount 
    | GapOpeningCount           
    | GapCount                  
    | PositiveScoringMatch      
    //means Query and subject frames separated by a '/'
    | Frames                   
    | Query_Frames             
    | Subject_Frames           
    //means Blast traceback operations (BTOP)
    | BTOP                      
    | Subject_TaxonomyIDs       
    | Subject_Scientific_Names  
    | Subject_Common_Names      
    | Subject_Blast_Names       
    | Subject_Super_Kingdoms    
    | Subject_Title             
    | Subject_All_Titles        
    | Subject_Strand            
    | Query_CoveragePerSubject  
    | Query_CoveragePerHSP    

    static member make = function
        | Query_SeqId               -> "qseqid"
        | Query_GI                  -> "qgi"
        | Query_Accesion            -> "qacc"
        | Query_Accesion_Version    -> "qaccver"
        | Query_Length              -> "qlen"
        | Subject_SeqId             -> "sseqid"
        | Subject_All_SeqIds        -> "sallseqid"
        | Subject_GI                -> "sgi"
        | Subject_All_GIs           -> "sallgi"
        | Subject_Accession         -> "sacc"
        | Subject_Accession_Version -> "saccver"
        | Subject_All_Accession     -> "sallacc"
        | Subject_Length            -> "slen"
        | Query_StartOfAlignment    -> "qstart"
        | Query_EndOfAlignment      -> "qend"
        | Subject_StartOfAlignment  -> "sstart"
        | Subject_EndOfAlignment    -> "send"
        | Query_AlignedPartOf       -> "qseq"
        | Subject_AlignedPartOf     -> "sseq" 
        | Evalue                    -> "evalue"
        | Bitscore                  -> "bitscore"
        | RawScore                  -> "score"
        | AlignmentLength           -> "length"
        | Identity                  -> "pident" 
        | IdentityCount             -> "nident"
        | MismatchCount             -> "mismatch"
        | PositiveScoringMatchCount -> "positive"
        | GapOpeningCount           -> "gapopen"
        | GapCount                  -> "gaps"
        | PositiveScoringMatch      -> "ppos"
        //means Query and subject frames separated by a '/'
        | Frames                    -> "frames" 
        | Query_Frames              -> "qframe"
        | Subject_Frames            -> "sframe"
        //means Blast traceback operations (BTOP)
        | BTOP                      -> "btop" 
        | Subject_TaxonomyIDs       -> "staxids" 
        | Subject_Scientific_Names  -> "sscinames"
        | Subject_Common_Names      -> "scomnames"
        | Subject_Blast_Names       -> "sblastnames"
        | Subject_Super_Kingdoms    -> "sskingdoms"
        | Subject_Title             -> "stitle"
        | Subject_All_Titles        -> "salltitles"
        | Subject_Strand            -> "sstrand"
        | Query_CoveragePerSubject  -> "qcovs"
        | Query_CoveragePerHSP      -> "qcovhsp"


type BlastParams =
    | SearchDB of string
    | Query    of string
    | Output   of string
    | OutputType of OutputType
    | OutputTypeCustom of OutputType * seq<OutputCustom>
    | Num_threads of int
    | Max_Hits of int

    static member makeCmdWith (m: MountInfo) = function
        | SearchDB  (path)      -> ["-db"    ; (MountInfo.containerPathOf m path)]
        | Query     (path)      -> ["-query" ; (MountInfo.containerPathOf m path)]
        | Output    (path)      -> ["-out"   ; (MountInfo.containerPathOf m path)]
        | OutputType(format)    -> ["-outfmt"; string (format |> OutputType.make)]
        | OutputTypeCustom(t,p) ->  let tmp = 
                                        p 
                                        |> Seq.map OutputCustom.make 
                                        |> String.concat " "
                                    match t with
                                    | OutputType.Tabular             -> ["-outfmt"; sprintf "%s %s" (string (t |> OutputType.make)) tmp]
                                    | OutputType.TabularWithComments -> ["-outfmt"; sprintf "%s %s" (string (t |> OutputType.make)) tmp]
                                    | OutputType.CSV                 -> ["-outfmt"; sprintf "%s %s" (string (t |> OutputType.make)) tmp]
                                    | _ -> failwithf "Output format %A does not support custom columns." t                                
        | Num_threads(i)        -> ["-num_threads"; string i]
        | Max_Hits (i)          -> ["-max_target_seqs"; string i]


let isSubPathOf (sub:string) (source:string) =
    let sub', source' = toUnixDirectorySeparator sub, toUnixDirectorySeparator source
    sub'.StartsWith ("./") <> sub'.StartsWith(source')

let client = Docker.connect "npipe://./pipe/docker_engine"

let ImageBlast = Docker.DockerId.ImageId "blast"

let blastContext = 
    BioContainer.initBcContextWithMountAsync client ImageBlast @"C:\Users\Kevin\source\repos\CsbScaffold\Docker\data"
    |> Async.RunSynchronously

Path.GetFullPath(@"C:\Users\Kevin\Source\Repos\CsbScaffold\Docker\data")

let runMakeBlastDBAsync (bcContext:BioContainer.BcContext) (opt:MakeDbParams list) = 

    let tp = "makeblastdb"::((opt |> List.map (MakeDbParams.makeCmdWith bcContext.Mount) |> List.concat))
    printfn "%A" tp
    async {
            let! res = BioContainer.execAsync bcContext tp           
            return res
 
    }

let runBlastPAsync (bcContext:BioContainer.BcContext) (opt:BlastParams list) = 
    let tp = "blastp"::((opt |> List.map (BlastParams.makeCmdWith bcContext.Mount) |> List.concat))
    printfn "%A" tp
    async {
            let! res = BioContainer.execAsync bcContext tp           
            return res
 
    }

let runBlastNAsync (bcContext:BioContainer.BcContext) (opt:BlastParams list) = 

    let tp = "blastn"::((opt |> List.map (BlastParams.makeCmdWith bcContext.Mount) |> List.concat))
    printfn "%A" tp
    async {
            let! res = BioContainer.execAsync bcContext tp           
            return res
 
    }



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

let outputType = OutputType.TabularWithComments

let customOutputFormat = OutputTypeCustom(outputType , outputFormat)

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

MountInfo.getContainerPath blastContext.Mount

runBlastPAsync blastContext blastPParamz
|> Async.RunSynchronously

runMakeBlastDBAsync blastContext paramz
|> Async.RunSynchronously

BioContainer.execAsync blastContext ["makeblastdb"; "-dbtype"; "prot" ;"-in"; "/data/C/Users/Kevin/Source/Repos/CsbScaffold/Docker/data/Chlamy_Cp.fastA"; "-out"; "/data/C/Users/Kevin/Source/Repos/CsbScaffold/Docker/data/Chlamy_Cp.fastA"]
|> Async.RunSynchronously

BioContainer.disposeAsync blastContext
|> Async.RunSynchronously