namespace BioFSharp.BioTools

open System
open System.Threading
open Docker.DotNet
open Docker.DotNet.Models

/// BioContainer helper
module BioContainer =
    
    open Docker

    type BcContext = {
        Id          : Guid
        Connection  : DockerClient
        ImageName   : string
        ContainerId : string
        }

   
    /// Connect to docker engine (docker deamon)
    let connect str =
        (new DockerClientConfiguration(new Uri(str)) ).CreateClient()

    
    let private readFrom (stream:System.IO.Stream) =
        let length = (stream.Length) |> int
        let tmp : array<byte> = Array.zeroCreate length
        stream.Read(tmp,0,length) |> ignore

        System.Text.Encoding.UTF8.GetString(tmp,0,length)

    /// Runs a container of a specified image and keeps it running
    let initBcContextAsync (connection:DockerClient)  (image: DockerId) =
        if not (Docker.Image.exists connection image) then failwithf "Image %s does not exists! Please pull the image first." (string image )    
        async {
            let! container =
                let param = Docker.Container.ContainerParams.InitCreateContainerParameters(Image=string image,OpenStdin=true)
                Docker.Container.createContainerWithAsync connection param      
        
            let! isRunning =
                let param = 
                    Docker.Container.ContainerParams.InitContainerStartParameters()

                Docker.Container.startContainerWithAsync connection param container.ID
                
            return {Id=Guid.NewGuid();Connection=connection;ImageName=string image;ContainerId=container.ID}
            } 


    let execAsync (bc:BcContext) cmd =
        async {
        
            let! execContainer =
                let param = 
                    Docker.Container.ContainerParams.InitContainerExecCreateParameters(                                        
                        AttachStderr=true,
                        AttachStdout=true,                
                        AttachStdin=false,
                        Cmd=cmd,
                        Detach=false                    
                        )

                Docker.Container.execCreateContainerAsync bc.Connection param (bc.ContainerId)

            let! stream =
                let param = 
                    Docker.Container.ContainerParams.InitContainerExecStartParameters(
                        AttachStderr=true,
                        AttachStdout=true,                
                        AttachStdin=false,                   
                        Cmd=cmd
                        )                
                Docker.Container.startContainerWithExecConfigAsync bc.Connection param execContainer.ID


            let stdOutputStream = new System.IO.MemoryStream()
            let streamTask =
                stream.CopyOutputToAsync(null,stdOutputStream,null,CancellationToken.None)             
                
            do! streamTask |> Async.AwaitTask


            let result =        
                stdOutputStream.Position <- 0L
                readFrom stdOutputStream
                    
            return result
    
        } 
        


    let disposeAsync (bc:BcContext) =
        let param = Docker.Container.ContainerParams.InitContainerRemoveParameters(Force=true)
        Docker.Container.removeContainerWithAsync bc.Connection param (Docker.DockerId.ContainerId bc.ContainerId)


    //let runCmdAsync (connection:DockerClient) (dockerid: DockerId) cmd =
    //    // Function creates and deletes new container all the time 
    //    // !maybe use  Containers.StartWithConfigContainerExecAsync (Docker.DotNet.Models.ContainerExecStartParameters()) in the future

    //    if not (Docker.Image.exists connection dockerid) then failwithf "Image %s does not exists! Please pull the image first." (dockerid.ToString())
    //    async {
    //        let! container =
    //            let param = Docker.Container.ContainerParams.InitCreateContainerParameters(Image=dockerid.ToString(),Cmd=cmd)
    //            Docker.Container.createContainerWithAsync connection param              

    //        let! isRunning =
    //            let param = Docker.Container.ContainerParams.InitContainerStartParameters()
    //            Docker.Container.startContainerWithAsync connection param container.ID
                
    //        let! wait = 
    //            Docker.Container.waitContainerAsync connection container.ID
        
    //        let! logs =
    //            let param = Docker.Container.ContainerParams.InitContainerLogsParameters(ShowStdout=true)
    //            Docker.Container.getContainerLogsAsync connection param container.ID
                    
    //        do! Docker.Container.removeContainerAsync connection (DockerId.ContainerId container.ID)
                
    //        return logs
    //    } 




    ///// Run = create + start (or only start if available)
    //let tryRunCmd (connection:DockerClient) (dockerid: DockerId) cmd =
    //    if Docker.Image.exists connection dockerid then
    //        let res = Docker.Container.createContainerByImage connection (dockerid.ToString())
    //        if Docker.Container.startContainer connection res.ID then
    //            Some 
    //        else
    //            None

    //    else
    //        None
    
    ///// Run = create + start (or only start if available)
    //let tryRun (connection:DockerClient) (dockerid: DockerId) =
    //    if Docker.Image.exists connection dockerid then
            
    //        if Docker.Container.existsByImage connection dockerid then
                
    //        else
    //            if Docker.Container.startContainer connection (dockerid.ToString()) then
    //                Some DockerId
    //            else
    //                None
    //    else
    //        None
        
        





