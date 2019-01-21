#r "netstandard"
#r "../../packages/Newtonsoft.Json.10.0.3/lib/netstandard1.3/Newtonsoft.Json.dll"
#r "../../packages/System.Buffers/lib/netstandard2.0/System.Buffers.dll"
#r "../../packages/Docker.DotNet/lib/netstandard2.0/Docker.DotNet.dll"


open System.Threading
open System.Threading
// Include CsbScaffold
// If you want to use the wrappers for unmanaged LAPACK functions from of FSharp.Stats 
// include the path to the .lib folder manually to your PATH environment variable and make sure you set FSI to 64 bit

// use the following lines of code to ensure that LAPACK functionalities are enabled if you want to use them
// fails with "MKL service either not available, or not started" if lib folder is not included in PATH.
//open FSharp.Stats
//FSharp.Stats.Algebra.LinearAlgebra.Service()

open Docker.DotNet
open System.Threading
open System.Buffers
open System.Threading.Tasks

let pipe = System.Uri("npipe://./pipe/docker_engine")

let config = new DockerClientConfiguration(pipe)
let client = config.CreateClient()

let createByImage (client:DockerClient) imageName =
    async {
        let param = Models.CreateContainerParameters()
        param.Image <- imageName
        param.Cmd <- System.Collections.Generic.List(["echo"; "hello world"])
        let! container =  
            client.Containers.CreateContainerAsync (param,CancellationToken.None)
            |> Async.AwaitTask
        return container.ID
    }


let result =
    async {
        let paramLog = Models.ContainerLogsParameters() // (Stdout = System.Nullable<bool>(true),Stdin = System.Nullable<bool>(true))
        paramLog.ShowStdout <- System.Nullable<bool>(true)
        let paramRun = Models.ContainerStartParameters ()
        
        //let id = 
        //    "4243adc7f3832ea35bdaad79aabe86f8e1c54f5c3a799cc72e060a8402bc24cb"
        
        let! id = createByImage client "ubuntu"

        let! isRunnig =  
            client.Containers.StartContainerAsync(id,paramRun,CancellationToken.None)
            |> Async.AwaitTask
        
        let! wait = 
            client.Containers.WaitContainerAsync(id,CancellationToken.None)
            |> Async.AwaitTask
        
        let! logs =
            client.Containers.GetContainerLogsAsync (id,paramLog,CancellationToken.None)
            |> Async.AwaitTask

            
        
        return logs
    } 
    |> Async.RunSynchronously


let tmp : array<byte> = Array.zeroCreate 1024
result.Read(tmp,0,1024)

System.Text.Encoding.UTF8.GetString(tmp,0,1024)

//let stream = 
//    async {
//        let paramAttach = Models.ContainerAttachParameters(Stdout = System.Nullable<bool>(true),Stdin = System.Nullable<bool>(true))
//        let paramRun = Models.ContainerStartParameters ()
        
//        let msg = System.Text.Encoding.UTF8.GetBytes("sh -c echo hello world\n")
        
//        let! isRunnig =  
//            client.Containers.StartContainerAsync("4243adc7f3832ea35bdaad79aabe86f8e1c54f5c3a799cc72e060a8402bc24cb",paramRun,CancellationToken.None)
//            |> Async.AwaitTask
//        if isRunnig then 
//            let! stream =  
//                client.Containers.AttachContainerAsync("4243adc7f3832ea35bdaad79aabe86f8e1c54f5c3a799cc72e060a8402bc24cb",true,paramAttach,CancellationToken.None) 
//                |> Async.AwaitTask
//            //do! stream.WriteAsync(msg,0,msg.Length,CancellationToken.None) |> Async.AwaitTask
//            //let! tmp = stream. ReadOutputToEndAsync(CancellationToken.None)|> Async.AwaitTask
//            //let! tmp = stream. CopyOutputToAsync()|> Async.AwaitTask
            
//            return ("","")//(tmp.Item1,tmp.Item2)
//        else
//            return ("och nöö","och nöö")
//    } 
//    |> Async.RunSynchronously


// async {
//     let msg = System.Text.Encoding.UTF8.GetBytes("echo 'hello world'")
//     do! stream.WriteAsync(msg,0,msg.Length,CancellationToken.None) |> Async.AwaitTask
//     let! tmp = stream.ReadOutputToEndAsync(CancellationToken.None)|> Async.AwaitTask
//     return tmp
// }|> Async.RunSynchronously