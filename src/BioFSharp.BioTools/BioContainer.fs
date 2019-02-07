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


    /// Connect to default local docker engine (docker deamon: "npipe://./pipe/docker_engine")
    //let connectLocalDefault () =


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


    /// Executes a command in the biocontainer context
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
                BioContainerIO.readFrom stdOutputStream
                    
            return result
    
        } 
        

    /// Disposes the biocontainer context (stops and removes the underlying container)
    let disposeAsync (bc:BcContext) =
        let param = Docker.Container.ContainerParams.InitContainerRemoveParameters(Force=true)
        Docker.Container.removeContainerWithAsync bc.Connection param (Docker.DockerId.ContainerId bc.ContainerId)


    /// Copies file from a container (only single file is supported)
    let getFileAsync (bc:BcContext) (filePath) =
        async {
            let param = Docker.Container.ContainerParams.InitGetArchiveFromContainerParameters(Path=filePath)
            let! res = Docker.Container.getArchiveFromContainerAsync  bc.Connection param false bc.ContainerId 
            return BioContainerIO.tarToStream res.Stream
            }
    
    /// Puts a stream in a container (only single file is supported)
    let putStreamAsync (bc:BcContext) (sourceStream:System.IO.Stream) targetFileName  =
        async {
            let targetPath = BioContainerIO.directoryName targetFileName
            let targetName = BioContainerIO.fileName targetFileName

            // ! Set the target filename as tar-entry name to make renameing possible
            let stream = BioContainerIO.tarOfStream targetName sourceStream
    
            let param = Docker.Container.ContainerParams.InitContainerPathStatParameters(AllowOverwriteDirWithFile=true, Path=targetPath)
            do!
                Docker.Container.extractArchiveToContainerAsync bc.Connection param (bc.ContainerId ) stream
        
            sourceStream.Close()
            }

    /// Copies file into a container (only single file is supported)
    let putFileAsync (bc:BcContext) (sourceFileName:string) targetFileName  =
        async {
            let fileStream = new System.IO.FileStream(sourceFileName,System.IO.FileMode.Open)
            do!
                putStreamAsync bc fileStream targetFileName
            }


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
        
        





