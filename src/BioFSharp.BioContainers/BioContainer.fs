namespace BioFSharp.BioContainers

open System
open System.Threading
open Docker.DotNet
open Docker.DotNet.Models

/// BioContainer helper
module BioContainer =
    
    open Docker
    open System.IO

    //[<StructuredFormatDisplay("{AsString}")>]
    type MountInfo =        
        | NoMount        
        | HostDir of string
        
        override this.ToString() =
            match this with
            | NoMount              -> "NoMount"
            | HostDir _ -> sprintf "%s:%s" (MountInfo.getHostDir this) (MountInfo.getContainerPath this)      
         
        ///get the full mounted unix path used in the container 
        static member getContainerPath (hd:MountInfo) =
            match hd with
            | NoMount               -> failwithf "No mount directory set."
            | HostDir hostdirectory -> 
                if hostdirectory.Contains(" ") then 
                    failwithf "paths mounted to docker cannot contain spaces.\r\nThe path %s contains spaces." hostdirectory
                else
                    sprintf "/data/%s" ((Path.GetFullPath(hostdirectory).Replace(":","")) |> BioContainerIO.toUnixDirectorySeparator )

        ///get the path of the windows host directory used to mount in the container
        static member getHostDir (hd:MountInfo) =
            match hd with
            | NoMount               -> failwithf "No mount directory set."
            | HostDir hostdirectory -> Path.GetFullPath (hostdirectory)

        ///get the container full mounted unix path of a file in a subfolder of the mounted host directory
        static member containerPathOf (m:MountInfo) (filePath:string) =
            let winDir          = MountInfo.getHostDir m
            let containerBase   = MountInfo.getContainerPath m

            //spaces not supported in unix paths
            if filePath.Contains(" ") then
                failwithf "paths mounted to docker cannot contain spaces.\r\nThe path %s contains spaces." filePath
            else
                //the given path is relative
                if filePath.StartsWith(".") then
                    let fullFilePath = 
                        //get absolute combined path
                        Path.Combine(containerBase,filePath)
                        |> Path.GetFullPath
                        |> BioContainerIO.toUnixDirectorySeparator

                    //check that combined path does not go above base (eg base/../../)
                    if (fullFilePath.StartsWith(containerBase)) then
                        fullFilePath |> BioContainerIO.toUnixDirectorySeparator
                    else
                        failwithf ("the relative path \r\n%s\r\n escapes the scope of the container base path \r\n%s\r\n. the combined path is:\r\n%s\r\n") filePath containerBase fullFilePath

                else
                    //Path is not relative. Use Path functions to resolve ../ and check if absolute path is a subpath of the windows base path
                    // TO-DO: make subpath matchin non-case-sensitive because that works on the windows side
                    let fullFilePath = filePath |> Path.GetFullPath
                    if fullFilePath.StartsWith(winDir) then
                        //if absolute windows path is correct, replace it with the containerbase
                        fullFilePath.Replace(winDir,containerBase)
                        |> fun x -> x.Replace(":","")
                        |> BioContainerIO.toUnixDirectorySeparator
                    else 
                        failwithf "The given path \r\n%s\r\n is not a subpath of the mounted host directory \r\n%s\r\n. If you want to use relative paths start them with ./" fullFilePath winDir
                        


            


            


    type BcContext = {
        Id          : Guid
        Connection  : DockerClient
        ImageName   : string
        ContainerId : string
        Mount       : MountInfo
        }
        
   
    /// Connect to docker engine (docker deamon)
    let connect str =
        (new DockerClientConfiguration(new Uri(str)) ).CreateClient()


    /// Connect to default local docker engine (docker deamon: "npipe://./pipe/docker_engine")
    let connectLocalDefault () =
        // TODO: Use System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(OSPlatform.Linux)
        connect "npipe://./pipe/docker_engine"
        


    /// Runs a container of a specified image and keeps it running
    let initBcContextAsync (connection:DockerClient)  (image: DockerId) =
        if not (Docker.Image.exists connection image) then failwithf "Image %s does not exists! Please pull the image first." (string image )    
        async {
            let! container =
                let param = Docker.Container.ContainerParams.InitCreateContainerParameters(User="root",Image=string image,OpenStdin=true)
                Docker.Container.createContainerWithAsync connection param      
        
            let! isRunning =
                let param = 
                    Docker.Container.ContainerParams.InitContainerStartParameters()

                Docker.Container.startContainerWithAsync connection param container.ID
                
            return {Id=Guid.NewGuid();Connection=connection;ImageName=string image;ContainerId=container.ID;Mount=MountInfo.NoMount}
            } 


    /// Runs a container of a specified image and keeps it running on the local default docker engine
    let initBcContextLocalDefaultAsync  (image: DockerId) =
        let client = connectLocalDefault () 
        initBcContextAsync client image


    /// Runs a container of a specified image and keeps it running. Bind mounts the host directory under /data/ (without ':' and lower letter according to BioContainer standards).
    let initBcContextWithMountAsync (connection:DockerClient) (image: DockerId) (hostdirectory:string) =
        if not (Docker.Image.exists connection image) then failwithf "Image %s does not exists! Please pull the image first." (string image )    
        let hd = MountInfo.HostDir hostdirectory
        async {
            let! container = // volume  bind
                
                let hostdirectory' = MountInfo.getHostDir hd 
                let target = MountInfo.getContainerPath hd  //sprintf "/data/%s" (hostdirectory'.ToLower().Replace(":",""))
                let mount = Docker.Container.ContainerParams.InitMount(Type="bind",Source=hostdirectory',Target=target,ReadOnly=false)
                let hc    = Docker.Container.ContainerParams.InitHostConfig(Mounts=[mount])
                let param = Docker.Container.ContainerParams.InitCreateContainerParameters(User="root",HostConfig=hc,Image=string image,OpenStdin=true)
                Docker.Container.createContainerWithAsync connection param      
        
            let! isRunning =
                let param = 
                    Docker.Container.ContainerParams.InitContainerStartParameters()

                Docker.Container.startContainerWithAsync connection param container.ID
                
            return {Id=Guid.NewGuid();Connection=connection;ImageName=string image;ContainerId=container.ID;Mount=hd}
            } 

    /// Executes a command in the biocontainer context and returns the either the standard output of the container or the standard error of the container if stdout is empty
    let execReturnAsync (bc:BcContext) cmd =
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
            let stdErrStream = new System.IO.MemoryStream()
            let streamTask =
                stream.CopyOutputToAsync(null,stdOutputStream,stdErrStream,CancellationToken.None)             
                
            do! streamTask |> Async.AwaitTask


            let result =        
                if stdOutputStream.Length < 1L then
                    stdErrStream.Position <- 0L
                    BioContainerIO.readFrom stdErrStream
                else
                    stdOutputStream.Position <- 0L
                    BioContainerIO.readFrom stdOutputStream
                    
            if stdErrStream.Length > 0L then
                stdErrStream.Position <- 0L
                System.Console.Error.Write(BioContainerIO.readFrom stdErrStream)

            return result
    
        } 
  
    ///Executes a command in the biocontainer context. Passes stdout and stderr of the container to stoud/stderr.
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
            let stdErrStream = new System.IO.MemoryStream()
            let streamTask =
                stream.CopyOutputToAsync(null,stdOutputStream,stdErrStream,CancellationToken.None)             
                
            do! streamTask |> Async.AwaitTask



            if stdErrStream.Length > 0L then
                stdErrStream.Position <- 0L
                System.Console.Error.Write(BioContainerIO.readFrom stdErrStream)

            if stdOutputStream.Length > 0L then
                stdOutputStream.Position <- 0L
                System.Console.Write(BioContainerIO.readFrom stdOutputStream)
                    
            return ()
    
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
        
        





