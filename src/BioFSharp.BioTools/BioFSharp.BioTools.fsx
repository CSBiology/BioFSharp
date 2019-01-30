#r "netstandard"
#r "../../packages/Newtonsoft.Json.10.0.3/lib/netstandard1.3/Newtonsoft.Json.dll"
#r "../../packages/System.Buffers/lib/netstandard2.0/System.Buffers.dll"
#r "../../packages/Docker.DotNet/lib/netstandard2.0/Docker.DotNet.dll"

#load "Docker.fs"
#load "BioContainer.fs"


open System.Threading
open System.Threading
open Docker.DotNet
open System.Threading
open System.Buffers
open System.Threading.Tasks

open BioFSharp.BioTools
open System.Collections.Generic
open Docker.DotNet.Models


let client = Docker.connect "npipe://./pipe/docker_engine"



let cmd  = ["bash"] //;"echo"; "hello world";]
let cmd' = ["echo"; "hello you";]
let dockerid =  (Docker.DockerId.ImageName "ubuntu") 
let connection = client

let cont = "1940820e0486"

let readFrom (stream:System.IO.Stream) =
    let length = (stream.Length-1L) |> int
    let tmp : array<byte> = Array.zeroCreate length
    stream.Read(tmp,0,length) |> ignore

    System.Text.Encoding.UTF8.GetString(tmp,0,length)


// https://github.com/Microsoft/Docker.DotNet/issues/223 -> write
// https://github.com/Microsoft/Docker.DotNet/issues/212 -> read

//-i, --interactive=false Keep STDIN open even if not attached
//-t, --tty=false Allocate a pseudo-TTY

let container,exe = 
    async {
        let! container =
            let param = Docker.Container.ContainerParams.InitCreateContainerParameters(Image=dockerid.ToString(),Cmd=cmd,OpenStdin=true)
            Docker.Container.createContainerWithAsync connection param      
        
        let! isRunning =
            let param = 
                Docker.Container.ContainerParams.InitContainerStartParameters()

            Docker.Container.startContainerWithAsync connection param container.ID

        let! tmp =
            let param = 
                Docker.Container.ContainerParams.InitContainerExecCreateParameters(                                        
                    AttachStderr=true,
                    AttachStdout=true,                
                    AttachStdin=false,
                    Cmd=cmd',
                    Detach=false,
                    Tty=true
                    )

            Docker.Container.execCreateContainerAsync connection param (container.ID)
        return container,tmp
        }

    |> Async.RunSynchronously


////docker stop $(docker ps -a -q)

//Docker.Container.removeContainerAsync connection (Docker.DockerId.ContainerId (container.ID))  
//|> Async.RunSynchronously


let ms = 
    async {

        //    Docker.Container.startContainerWithAsync connection param container.ID



        let! isRunning1 =
            let param1 = 
                Docker.Container.ContainerParams.InitContainerStartParameters()

            Docker.Container.startContainerWithAsync connection param1 container.ID
        
        let! isRunning =
            let param = 
                Docker.Container.ContainerParams.InitContainerExecStartParameters(
                    AttachStderr=true,
                    AttachStdout=true,                
                    AttachStdin=false,                   
                    Cmd=cmd' // cmd
                    )                
            Docker.Container. startContainerExecAsync connection exe.ID // startContainerWithExecConfigAsync connection param container.ID 
            
        let! stream = 
            let param = Docker.Container.ContainerParams.InitContainerAttachParameters (Stdout=true,Stderr=true,Stdin=false,Stream=true)
            connection.Containers.AttachContainerAsync(exe.ID,true,param)
            |> Async.AwaitTask
    
        let stdOutputStream = new System.IO.MemoryStream()
        let streamTask =
            stream.CopyOutputToAsync(null,stdOutputStream,null,CancellationToken.None)             

                
        do! streamTask |> Async.AwaitTask

        let! wait = 
            Docker.Container.waitContainerAsync connection container.ID

        let result =        
            stdOutputStream.Position <- 0L
            readFrom stdOutputStream
                    
        //do! Docker.Container.removeContainerAsync connection (Docker.DockerId.ContainerId container.ID)  
    
        return result
    
    } 
    |> Async.RunSynchronously



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








































let tmp =
    BioContainer.runCmdAsync client (Docker.DockerId.ImageName "ubuntu") ["echo"; "hello world"]
    |> Async.RunSynchronously
    |> readFrom



Docker.Image.exists client (Docker.DockerId.ImageName "targetp_image")


Docker.Image.listImages client
|> Seq.map (fun i -> i.ID )
|> Seq.toArray


Docker.Container.existsByImage client (Docker.DockerId.ImageName "targetp_image")


//ancestor=(<image-name>[:<tag>], <image id> or <image@digest>)

let filters = 
    Docker.Container.ContainerParams.InitContainerListParameters(All=true,Filters=Docker.Filters.InitContainerFilters(Ancestor=Docker.DockerId.ImageName "ubuntu"))


Docker.Container.listContainersWithAsync client filters
|> Async.RunSynchronously
|> Seq.map (fun x -> x.Command,x.Image,x.Labels)
|> Seq.toArray

//client.Containers.StartWithConfigContainerExecAsync
let p = Docker.DotNet.Models.ContainerExecStartParameters()


let ap = Docker.DotNet.Models.ContainerAttachParameters()




Docker.Container.existsByImage client (Docker.DockerId.ImageName "targetp_image")


let idtp = "61fbfbc30382e83dd585c99583c036ef8c5ced4eb10e1b274f199da6b6969588"

//let pipe = System.Uri("npipe://./pipe/docker_engine")

//let config = new DockerClientConfiguration(pipe)
//let client = config.CreateClient()

//let createByImage (client:DockerClient) imageName =
//    async {
//        let param = Models.CreateContainerParameters()
//        param.Image <- imageName
//        param.Cmd <- System.Collections.Generic.List(["echo"; "hello world"])
//        let! container =  
//            client.Containers.CreateContainerAsync (param,CancellationToken.None)
//            |> Async.AwaitTask
//        return container.ID
//    }


//let result =
//    async {
//        let paramLog = Models.ContainerLogsParameters() // (Stdout = System.Nullable<bool>(true),Stdin = System.Nullable<bool>(true))
//        paramLog.ShowStdout <- System.Nullable<bool>(true)
//        let paramRun = Models.ContainerStartParameters ()
        
//        //let id = 
//        //    "4243adc7f3832ea35bdaad79aabe86f8e1c54f5c3a799cc72e060a8402bc24cb"
        
//        let! id = createByImage client "ubuntu"

//        let! isRunnig =  
//            client.Containers.StartContainerAsync(id,paramRun,CancellationToken.None)
//            |> Async.AwaitTask
        
//        let! wait = 
//            client.Containers.WaitContainerAsync(id,CancellationToken.None)
//            |> Async.AwaitTask
        
//        let! logs =
//            client.Containers.GetContainerLogsAsync (id,paramLog,CancellationToken.None)
//            |> Async.AwaitTask

            
        
//        return logs
//    } 
//    |> Async.RunSynchronously


//let tmp : array<byte> = Array.zeroCreate 1024
//result.Read(tmp,0,1024)

//System.Text.Encoding.UTF8.GetString(tmp,0,1024)

