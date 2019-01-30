namespace BioFSharp.BioTools

open System
open System.Threading
open Docker.DotNet
open Docker.DotNet.Models

/// BioContainer helper
module BioContainer =
    
    open Docker


    //let parseSgdId   = Regex.tryEitherParse SgdId @"SGDID:\w\d+"  

    
    /// Connect to docker engine (docker deamon)
    let connect str =
        (new DockerClientConfiguration(new Uri(str)) ).CreateClient()




    let runCmdAsync (connection:DockerClient) (dockerid: DockerId) cmd =
        // Function creates and deletes new container all the time 
        // !maybe use  Containers.StartWithConfigContainerExecAsync (Docker.DotNet.Models.ContainerExecStartParameters()) in the future

        if not (Docker.Image.exists connection dockerid) then failwithf "Image %s does not exists!" (dockerid.ToString())
        async {
            let! container =
                let param = Docker.Container.ContainerParams.InitCreateContainerParameters(Image=dockerid.ToString(),Cmd=cmd)
                Docker.Container.createContainerWithAsync connection param              

            let! isRunning =
                let param = Docker.Container.ContainerParams.InitContainerStartParameters()
                Docker.Container.startContainerWithAsync connection param container.ID
                
            let! wait = 
                Docker.Container.waitContainerAsync connection container.ID
        
            let! logs =
                let param = Docker.Container.ContainerParams.InitContainerLogsParameters(ShowStdout=true)
                Docker.Container.getContainerLogsAsync connection param container.ID
                    
            do! Docker.Container.removeContainerAsync connection (DockerId.ContainerId container.ID)
                
            return logs
        } 




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
        
        





