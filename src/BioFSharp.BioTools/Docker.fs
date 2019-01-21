namespace BioFSharp.BioTools

open System
open System.Threading
open Docker.DotNet
open Docker.DotNet.Models

/// Docker helper
module Docker =
    
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




    module Container =        
        
 
        /// Provides a set of static methods for creating Image parameter.
        type ContainerParams =

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





