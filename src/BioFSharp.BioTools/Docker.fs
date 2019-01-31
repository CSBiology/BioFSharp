namespace BioFSharp.BioTools

open System
open System.Threading
open System.Collections.Generic
open Docker.DotNet
open Docker.DotNet.Models


// https://docs.docker.com/engine/api/v1.24/

/// Docker helper
module Docker =


    /// Dockerfile > (Build) > Image > (Create/Run) > Container <- start/stop
    type DockerId =         
        | ImageId of string         
        | ImageName of string         
        | ContainerId of string        
        | ContainerName of string
        | Tag of string*string
    
        override this.ToString() =
            match this with
            | ImageId  s      -> s
            | ImageName  s    -> s
            | ContainerId  s  -> s
            | ContainerName s -> s
            | Tag (s,t)       -> sprintf "%s:%s" s t

    /// Dockerfile > (Build) > Image > (Create/Run) > Container <- start/stop
    type ContainerStatus =         
        | Created
        | Restarting
        | Running
        | Paused
        | Exited
        | Dead
    
    
        override this.ToString() =
            match this with
            | Created       -> "created"    
            | Restarting    -> "restarting"
            | Running       -> "running"    
            | Paused        -> "paused"     
            | Exited        -> "exited"     
            | Dead          -> "dead"       


    /// Provides a set of static methods for creating Image parameter.
    type Filters =
    
        static member private DicOfSeq (s:('k * 'v) seq) = new Dictionary<'k,'v>(s |> Map.ofSeq) :> IDictionary<'k,'v>

        /// Creates ImagesListParameters for pre-filtering list function
        static member InitContainerFilters
            (
                ?Ancestor:DockerId,
                ?Status:ContainerStatus,
                // Containers that exited with status code
                ?Exited:int,                    
                ?Label,                
                ?Isolation,
                ?Before:DockerId,
                ?Since:DockerId,
                ?Volume,
                ?Network
            ) = 

            let filter = System.Collections.Generic.Dictionary()            
            Ancestor            |> Option.iter (fun nv -> filter.Add( "ancestor", [nv.ToString(),true] |> Filters.DicOfSeq) )
            Status              |> Option.iter (fun nv -> filter.Add( "status", [nv.ToString(),true] |> Filters.DicOfSeq) )
            Exited              |> Option.iter (fun nv -> filter.Add( "exited", [nv.ToString(),true] |> Filters.DicOfSeq) )                   
            Label               |> Option.iter (fun nv -> filter.Add( "label", [nv.ToString(),true] |> Filters.DicOfSeq) )            
            Isolation           |> Option.iter (fun nv -> filter.Add( "isolation", [nv.ToString(),true] |> Filters.DicOfSeq) )
            Before              |> Option.iter (fun nv -> filter.Add( "before", [nv.ToString(),true] |> Filters.DicOfSeq) )
            Since               |> Option.iter (fun nv -> filter.Add( "since", [nv.ToString(),true] |> Filters.DicOfSeq) )
            Volume              |> Option.iter (fun nv -> filter.Add( "volume", [nv.ToString(),true] |> Filters.DicOfSeq) )
            Network             |> Option.iter (fun nv -> filter.Add( "network", [nv.ToString(),true] |> Filters.DicOfSeq) )                
        
            filter

    
    /// Connect to docker engine (docker deamon)
    let connect str =
        (new DockerClientConfiguration(new Uri(str)) ).CreateClient()
    
    module Image = 
        
        
        /// Provides a set of static methods for creating Image parameter.
        type ImagesParams =

            /// Creates ImagesListParameters for pre-filtering list function
            static member InitImagesListParameters
                (
                    ?All,
                    ?Filters,
                    ?MatchName
                ) = 

                let param = new ImagesListParameters()                
                All       |> Option.iter (fun nv -> param.set_All (Nullable<bool>(nv)))
                Filters   |> Option.iter param.set_Filters
                MatchName |> Option.iter param.set_MatchName
                
                param

            /// Creates ImagesCreateParameters
            static member InitImagesCreateParameters
                (
                    ?FromImage,
                    ?FromSrc,
                    ?Repo,
                    ?Tag
                ) = 
                
                let param = new ImagesCreateParameters()                                
                FromImage |> Option.iter param.set_FromImage
                FromSrc   |> Option.iter param.set_FromSrc
                Repo      |> Option.iter param.set_Repo
                Tag       |> Option.iter param.set_Tag

                param

            ///// Creates ImageBuildParameters
            //static member InitImageBuildParameters
            //    (
            //        ?FromImage,
            //        ?FromSrc,
            //        ?Repo,
            //        ?Tag
            //    ) = 
                
            //    let param = new ImageBuildParameters()    
                
            //    FromImage |> Option.iter param.set_FromImage
            //    FromSrc   |> Option.iter param.set_FromSrc
            //    Repo      |> Option.iter param.set_Repo
            //    Tag       |> Option.iter param.set_Tag

            //    param        
        
        /// Lists available images with ImagesListParameters for filtering (async)
        let listImagesWithAsync (connection:DockerClient) (param:ImagesListParameters) =
            //  ImagesListParameters are only for filtering
            async {                
                let! tmp = 
                    connection.Images. ListImagesAsync(param)              
                    |> Async.AwaitTask                    
                return (tmp |> Seq.map id)
                }


        /// Lists all available images (async)
        let listImagesAsync (connection:DockerClient) =
            listImagesWithAsync connection (ImagesParams.InitImagesListParameters())
        
        
        /// Lists all available images
        let listImages (connection:DockerClient) =
            listImagesAsync connection
            |> Async.RunSynchronously      


        /// Returns true if an image matches the dockerid (async) 
        let existsAsync (connection:DockerClient) (dockerid:DockerId) =
            let param = ImagesParams.InitImagesListParameters(MatchName=dockerid.ToString())
            async {
                let! tmp = listImagesWithAsync connection param                
                 
                return (Seq.length tmp > 0)
                }
        
        
        /// Returns true if an image matches the name 
        let exists (connection:DockerClient) (dockerid:DockerId) =
            existsAsync connection dockerid
            |> Async.RunSynchronously 

        /// Lists available images with ImagesListParameters for filtering (async)
        let buildImageFromDockerfileAsync (connection:DockerClient) (param) (stream) =
            //  ImagesListParameters are only for filtering
            async {                
                let! stream = 
                    connection.Images.BuildImageFromDockerfileAsync(stream,param)              
                    |> Async.AwaitTask                    
                return stream
                }
    
    //#####################################################
    //#####################################################
    //#####################################################

            
    module Container =        
        
 
        /// Provides a set of static methods for creating Image parameter.
        type ContainerParams =

            /// Creates ContainersListParameters for pre-filtering list function
            static member InitContainerListParameters
                (
                    ?All,
                    ?Before,                    
                    ?Filters,
                    ?Limit,
                    ?Since,
                    ?Size
                ) = 

                let param = new ContainersListParameters()                
                All       |> Option.iter (fun nv -> param.set_All (Nullable<bool>(nv)))
                Before    |> Option.iter param.set_Before
                Filters   |> Option.iter param.set_Filters
                Limit     |> Option.iter (fun nv -> param.set_Limit (Nullable<int64>(nv)))
                Since     |> Option.iter param.set_Since
                Size      |> Option.iter (fun nv -> param.set_Size (Nullable<bool>(nv)))
                
                param

            /// Creates ContainersListParameters for pre-filtering list function
            static member InitContainerListParameters
                (
                    ?WaitBeforeKillSeconds
                ) = 

                let param = new ContainerStopParameters()
                
                WaitBeforeKillSeconds       |> Option.iter (fun nv -> param.set_WaitBeforeKillSeconds (Nullable<uint32>(nv)))                
                
                param

            /// Creates ContainerAttachParameters
            static member InitContainerAttachParameters
                (
                    ?DetachKeys,
                    ?Logs,
                    ?Stderr,
                    ?Stdin,
                    ?Stdout,                    
                    ?Stream
                ) = 
                
                let param = new ContainerAttachParameters()                                
                DetachKeys |> Option.iter param.set_DetachKeys
                Logs       |> Option.iter param.set_Logs
                Stderr     |> Option.iter (fun v -> param.set_Stderr (Nullable<bool>(v) ) )
                Stdin      |> Option.iter (fun v -> param.set_Stdin  (Nullable<bool>(v) ) )
                Stdout     |> Option.iter (fun v -> param.set_Stdout (Nullable<bool>(v) ) )                
                Stream     |> Option.iter (fun v -> param.set_Stream (Nullable<bool>(v) ) )

                param

            /// Creates ContainerExecStartParameters
            static member InitContainerExecStartParameters
                (
                    
                    ?AttachStderr,
                    ?AttachStdin,
                    ?AttachStdout,                    
                    ?Cmd:seq<string>,
                    ?Detach,
                    ?DetachKeys,
                    ?Env:seq<string>,
                    ?Privileged,
                    ?Tty,
                    ?User
                ) = 
                
                let param = new ContainerExecStartParameters()                                
                
                AttachStderr     |> Option.iter param.set_AttachStderr
                AttachStdin      |> Option.iter param.set_AttachStdin 
                AttachStdout     |> Option.iter param.set_AttachStdout
                Cmd              |> Option.iter (fun v -> param.set_Cmd (Collections.Generic.List(v)) )   
                Detach           |> Option.iter param.set_Detach
                DetachKeys       |> Option.iter param.set_DetachKeys
                Env              |> Option.iter (fun v -> param.set_Env (Collections.Generic.List(v)) )   
                Privileged       |> Option.iter param.set_Privileged
                Tty              |> Option.iter param.set_Tty
                User             |> Option.iter param.set_User

                param

            /// Creates ContainerExecStartParameters
            static member InitContainerExecCreateParameters
                (
                    
                    ?AttachStderr,
                    ?AttachStdin,
                    ?AttachStdout,                    
                    ?Cmd:seq<string>,
                    ?Detach,
                    ?DetachKeys,
                    ?Env:seq<string>,
                    ?Privileged,
                    ?Tty,
                    ?User
                ) = 
                
                let param = new ContainerExecCreateParameters()                                
                
                AttachStderr     |> Option.iter param.set_AttachStderr
                AttachStdin      |> Option.iter param.set_AttachStdin 
                AttachStdout     |> Option.iter param.set_AttachStdout
                Cmd              |> Option.iter (fun v -> param.set_Cmd (Collections.Generic.List(v)) )   
                Detach           |> Option.iter param.set_Detach
                DetachKeys       |> Option.iter param.set_DetachKeys
                Env              |> Option.iter (fun v -> param.set_Env (Collections.Generic.List(v)) )   
                Privileged       |> Option.iter param.set_Privileged
                Tty              |> Option.iter param.set_Tty
                User             |> Option.iter param.set_User

                param

            /// Creates CreateContainerParameters
            static member InitCreateContainerParameters
                (
                    ?Hostname,
                    ?Domainname,
                    ?User,
                    ?AttachStdin,
                    ?AttachStdout,
                    ?AttachStderr,
                    ?ExposedPorts,
                    ?Tty,
                    ?OpenStdin,
                    ?StdinOnce,
                    ?Env : seq<string>,
                    ?Cmd : seq<string>,
                    ?Healthcheck,
                    ?ArgsEscaped,
                    ?Image,
                    ?Volumes,
                    ?WorkingDir,
                    ?Entrypoint : seq<string>,
                    ?NetworkDisabled,
                    ?MacAddress,
                    ?OnBuild : seq<string>,
                    ?Labels,
                    ?StopSignal,
                    ?StopTimeout,
                    ?Shell : seq<string>
                ) = 
                
                let param = new CreateContainerParameters()
                
                Hostname         |> Option.iter param.set_Hostname       
                Domainname       |> Option.iter param.set_Domainname     
                User             |> Option.iter param.set_User           
                AttachStdin      |> Option.iter param.set_AttachStdin    
                AttachStdout     |> Option.iter param.set_AttachStdout   
                AttachStderr     |> Option.iter param.set_AttachStderr   
                ExposedPorts     |> Option.iter param.set_ExposedPorts   
                Tty              |> Option.iter param.set_Tty            
                OpenStdin        |> Option.iter param.set_OpenStdin      
                StdinOnce        |> Option.iter param.set_StdinOnce      
                Env              |> Option.iter (fun v -> param.set_Env (Collections.Generic.List(v)) )            
                Cmd              |> Option.iter (fun v -> param.set_Cmd (Collections.Generic.List(v)) )                
                Healthcheck      |> Option.iter param.set_Healthcheck    
                ArgsEscaped      |> Option.iter param.set_ArgsEscaped    
                Image            |> Option.iter param.set_Image          
                Volumes          |> Option.iter param.set_Volumes        
                WorkingDir       |> Option.iter param.set_WorkingDir     
                Entrypoint       |> Option.iter (fun v -> param.set_Entrypoint (Collections.Generic.List(v)) )      
                NetworkDisabled  |> Option.iter param.set_NetworkDisabled
                MacAddress       |> Option.iter param.set_MacAddress     
                OnBuild          |> Option.iter (fun v -> param.set_OnBuild (Collections.Generic.List(v)) )         
                Labels           |> Option.iter param.set_Labels         
                StopSignal       |> Option.iter param.set_StopSignal     
                StopTimeout      |> Option.iter (fun v -> param.set_StopTimeout (Nullable<TimeSpan>(v) ) )   
                Shell            |> Option.iter (fun v -> param.set_Shell (Collections.Generic.List(v)) )  

                param

                
            /// Creates ContainerStartParameters
            static member InitContainerStartParameters
                (                    
                    ?DetachKeys 
                ) = 
                
                let param = new ContainerStartParameters()                
                DetachKeys         |> Option.iter param.set_DetachKeys       

                param


            /// Creates ContainerLogsParameters
            static member InitContainerLogsParameters
                (                    
                    ?Follow,
                    ?ShowStderr,
                    ?ShowStdout,
                    ?Since,
                    ?Tail,
                    ?Timestamps
                ) = 
                
                let param = new ContainerLogsParameters()
                Follow         |> Option.iter (fun v -> param.set_Follow     (Nullable<bool>(v) ) )
                ShowStderr     |> Option.iter (fun v -> param.set_ShowStderr (Nullable<bool>(v) ) )
                ShowStdout     |> Option.iter (fun v -> param.set_ShowStdout (Nullable<bool>(v) ) )
                Since          |> Option.iter param.set_Since
                Tail           |> Option.iter param.set_Tail
                Timestamps     |> Option.iter (fun v -> param.set_Timestamps (Nullable<bool>(v) ) )
                                
                param


            /// Creates ContainerRemoveParameters
            static member InitContainerRemoveParameters
                (                    
                    ?Force,
                    ?RemoveLinks,
                    ?RemoveVolumes
                ) = 
                
                let param = new ContainerRemoveParameters ()                
                Force         |> Option.iter (fun v -> param.set_Force         (Nullable<bool>(v) ) )
                RemoveLinks   |> Option.iter (fun v -> param.set_RemoveLinks   (Nullable<bool>(v) ) )
                RemoveVolumes |> Option.iter (fun v -> param.set_RemoveVolumes (Nullable<bool>(v) ) )
                                
                param

        /// Creates docker container with CreateContainerParameters for config (async)
        let createContainerWithAsync (connection:DockerClient) (param:CreateContainerParameters) =        
            async {              
                let! tmp = 
                    connection.Containers.CreateContainerAsync(param)
                    |> Async.AwaitTask            
        
                return tmp
                }   
                

        /// Creates docker container of an image given by image name (async)
        let createContainerByImageAsync (connection:DockerClient) imageName =            
            async {         
                let param = ContainerParams.InitCreateContainerParameters(Image=imageName)                
                //param.Cmd <-  Collections.Generic.List<string>(["/bin/bash";"c";"echo 'Hello'"])        
                let! tmp = 
                    connection.Containers.CreateContainerAsync(param)
                    |> Async.AwaitTask            
        
                return tmp
                }   
                

        /// Creates docker container of an image given by image name (async)    
        let createContainerByImage (connection:DockerClient) imageName =
            createContainerByImageAsync connection imageName
            |>  Async.RunSynchronously 


        /// Creates docker container with ContainerExecStartParameters for config (async)
        let createContainerWithExecConfigAsync (connection:DockerClient) (param:ContainerExecCreateParameters) id =        
            async {              
                let! tmp = 
                    connection.Containers.ExecCreateContainerAsync(id,param,CancellationToken.None)
                    |> Async.AwaitTask            
        
                return tmp
                }   


        /// Start container with CreateContainerParameters for config (async)
        let startContainerWithAsync (connection:DockerClient) (param:ContainerStartParameters) id =        
            async {              
                let! isRunnig = 
                    connection.Containers.StartContainerAsync(id,param,CancellationToken.None)
                    |> Async.AwaitTask            
        
                return isRunnig
                }   


        /// Start container by a given ID (async)
        let startContainerAsync (connection:DockerClient) id =        
            startContainerWithAsync connection (ContainerParams.InitContainerStartParameters()) id


        /// Start container by a given ID
        let startContainer (connection:DockerClient) id =        
            startContainerWithAsync connection (ContainerParams.InitContainerStartParameters()) id
            |> Async.RunSynchronously


        /// Start container with ContainerExecStartParameters for config (async)
        let startContainerWithExecConfigAsync (connection:DockerClient) (param:ContainerExecStartParameters) id =        
            async {              
                let! isRunnig = 
                    connection.Containers.StartWithConfigContainerExecAsync(id,param,CancellationToken.None)
                    |> Async.AwaitTask            
        
                return isRunnig
                }   



        /// Wait for container (async)
        let waitContainerAsync (connection:DockerClient) id =        
            async {              
                let! tmp = 
                    connection.Containers.WaitContainerAsync(id,CancellationToken.None)
                    |> Async.AwaitTask            
        
                return tmp
                }   

        /// Wait for container 
        let waitContainer (connection:DockerClient) id =        
            waitContainerAsync connection id
            |> Async.RunSynchronously

       
        /// Get logs from container (async)
        let getContainerLogsAsync (connection:DockerClient) param id =        
            async {                              
                let! tmp = 
                    connection.Containers.GetContainerLogsAsync(id,param,CancellationToken.None)
                    |> Async.AwaitTask            
        
                return tmp
                }   


        /// Get logs from container and show StdOut
        let getContainerLogs (connection:DockerClient) id =        
            getContainerLogsAsync connection (ContainerParams.InitContainerLogsParameters(ShowStdout=true)) id
            |> Async.RunSynchronously


        /// Lists available containers with ContainersListParameters for filtering (async)
        let listContainersWithAsync (connection:DockerClient) (param:ContainersListParameters) =
            //  ImagesListParameters are only for filtering
            async {                
                let! tmp = 
                    connection.Containers.ListContainersAsync(param,CancellationToken.None)              
                    |> Async.AwaitTask                    
                return (tmp |> Seq.map id)
                }


        /// Lists all available containers (async)
        let listAllContainersAsync (connection:DockerClient) =
            listContainersWithAsync connection (ContainerParams.InitContainerListParameters(All=true))

                       
        /// Returns true if acontainer matches the dockerid (async) 
        let existsByAsync (connection:DockerClient) (dockerid:DockerId) =
            let filter = Filters.InitContainerFilters(Ancestor=dockerid)
            let param = ContainerParams.InitContainerListParameters(All=true,Filters=filter)
            async {
                let! tmp = listContainersWithAsync connection param                
                 
                return (Seq.length tmp > 0)
                }


        /// Returns true if acontainer matches the dockerid 
        let existsByImage (connection:DockerClient) (dockerid:DockerId) =
            existsByAsync connection dockerid
            |> Async.RunSynchronously
                       

        /// Removes container  
        let removeContainerWithAsync (connection:DockerClient) (param) (dockerid:DockerId) =
            async {                
                do!
                    connection.Containers.RemoveContainerAsync(dockerid.ToString(),param,CancellationToken.None)              
                    |> Async.AwaitTask                                    
                }


        /// Removes container  
        let removeContainerAsync (connection:DockerClient) (dockerid:DockerId) =
            let param = ContainerParams.InitContainerRemoveParameters()
            removeContainerWithAsync connection param dockerid


        /// Creates an container that will perform the execution (async). IMPORTANT: Start exec-container using StartContainerExecAsync       
        let execCreateContainerAsync (connection:DockerClient) param id =        
            async {                              
                let! tmp = 
                    connection.Containers.ExecCreateContainerAsync (id,param,CancellationToken.None)
                    |> Async.AwaitTask            
        
                return tmp
                }  

        /// Starts
        let startContainerExecAsync (connection:DockerClient) id =        
            async {                              
                do!
                    connection.Containers.StartContainerExecAsync (id,CancellationToken.None)
                    |> Async.AwaitTask                                    
                }  

        /// Stops 
        let stopContainerAsync (connection:DockerClient) (param) id =        
            async {                              
                let! isRunning =
                    connection.Containers.StopContainerAsync(id,param,CancellationToken.None)
                    |> Async.AwaitTask                                    
                return isRunning
                }  



    //#####################################################
    //#####################################################
    //#####################################################

    module Volumes =
        /// Provides a set of static methods for creating Image parameter.
        type VolumesParams =
    
            /// Creates ContainerStartParameters
            static member InitVolumesCreateParameters
                (                    
                    ?Driver,
                    ?DriverOpts,
                    ?Labels,
                    ?Name

                ) = 
                
                let param = new VolumesCreateParameters()
                Driver     |> Option.iter param.set_Driver
                DriverOpts |> Option.iter param.set_DriverOpts
                Labels     |> Option.iter param.set_Labels
                Name       |> Option.iter param.set_Name    

                param

        /// Create volume with VolumesCreateParameters (async)
        let createAsync (connection:DockerClient) param =        
            async {                              
                let! tmp = 
                    connection.Volumes.CreateAsync(param,CancellationToken.None)
                    |> Async.AwaitTask            
        
                return tmp
                } 


        /// Create volume with VolumesCreateParameters
        let create (connection:DockerClient) param =        
            createAsync connection param
            |> Async.RunSynchronously


        /// Inspect volume (async)
        let inspectAsync (connection:DockerClient) name =        
            async {                              
                let! tmp = 
                    connection.Volumes.InspectAsync(name,CancellationToken.None)
                    |> Async.AwaitTask            
        
                return tmp
                } 


        /// Inspect volume 
        let inspect (connection:DockerClient) name =        
            inspectAsync connection name
            |> Async.RunSynchronously


        /// List volume (async)
        let listVolumesAsync (connection:DockerClient) =        
            async {                              
                let! tmp = 
                    connection.Volumes.ListAsync(CancellationToken.None)
                    |> Async.AwaitTask            
        
                return tmp
                } 


        /// List volume 
        let listVolumes (connection:DockerClient) =        
            listVolumesAsync connection
            |> Async.RunSynchronously





