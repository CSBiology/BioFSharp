namespace BioFSharp.BioContainers

open System
open System.IO
open System.Threading


// targetp -N /opt/targetP/test/one.fsa


module TargetP =
 
    open FSharpAux
    open FSharpAux.IO
    open FSharpAux.IO.SchemaReader.Attribute
 
    let ImageTargetP = Docker.DockerId.ImageId "targetp"

    type TargetpCustomParams =
        | CleavagePredictions
        | CutOffChloroplast of float
        | CutOffSecretory of float
        | CutOffMitochondrial of float
        | CutOffLocation of float

        static member make = function
            | CleavagePredictions   -> "-c"
            | CutOffChloroplast v   -> sprintf "-p %.2f" v
            | CutOffSecretory v     -> sprintf "-s %.2f" v
            | CutOffMitochondrial v -> sprintf "-t %.2f" v
            | CutOffLocation v      -> sprintf "-o %.2f" v


    type TargetpParams =
        | NonPlant
        | Plant
        | NonPlantCustom of seq<TargetpCustomParams>
        | PlantCustom of seq<TargetpCustomParams>
    
        static member makeCmd = function
            | NonPlant -> ["-N"]
            | Plant    -> ["-P"]
            | NonPlantCustom v -> 
                let tmp =
                    v |> Seq.map (fun p -> TargetpCustomParams.make p) |> Seq.toList
                "-N"::tmp
            | PlantCustom    v ->
                let tmp =
                    v |> Seq.map (fun p -> TargetpCustomParams.make p) |> Seq.toList
                "-P"::tmp

        static member make = function
            | NonPlant -> "-N"
            | Plant    -> "-P"
            | NonPlantCustom v -> 
                let tmp =
                    v |> Seq.map (fun p -> TargetpCustomParams.make p) |> String.concat " "
                sprintf "-N %s" tmp
            | PlantCustom    v ->
                let tmp =
                    v |> Seq.map (fun p -> TargetpCustomParams.make p) |> String.concat " "
                sprintf "-P %s" tmp



    type TargetpItem = 
        { 
            [<FieldAttribute("Name")>]  Name  : string
            [<FieldAttribute("Len")>]   Len   : int
            [<FieldAttribute("mTP")>]   Mtp   : float
            [<FieldAttribute("cTP")>]   Ctp   : float
            [<FieldAttribute("SP")>]    SP    : float
            [<FieldAttribute("other")>] Other : float
            [<FieldAttribute("Loc")>]   Loc   : string
            [<FieldAttribute("RC")>]    RC    : int
            [<FieldAttribute("TPlen")>] TPlen : string
        }

    let runWithStreamAsync bcContext (opt:TargetpParams) (fsaStream:Stream) = 
        fsaStream.Seek(0L, SeekOrigin.Begin) |> ignore
        let tp = "targetp"::TargetpParams.makeCmd opt
        let tmpFile = sprintf "/data/%A.fsa" (System.Guid.NewGuid())
        async {
            do!
                BioContainer.putStreamAsync bcContext fsaStream tmpFile
            let! targepResult =
                BioContainer.execReturnAsync bcContext (tp@[tmpFile])

            // CsV Reader
            let skipLines             = 1
            let skipLinesBeforeHeader = 6 //6
            let schemaMode = SchemaReader.Csv.Fill
            let csvReader = SchemaReader.Csv.CsvReader<TargetpItem>(SchemaMode=schemaMode)
            
            return csvReader.ReadFromString(targepResult,'\t',true,skipLines, skipLinesBeforeHeader)
 
        }
        
        
    let runWithStream bcContext (opt:TargetpParams) (fsaStream:Stream) = 
        runWithStreamAsync bcContext opt fsaStream
        |> Async.RunSynchronously

    let runWithMountedFileAsync (bcContext:BioContainer.BcContext) (opt:TargetpParams) (inputFile:string) =
        let cPath = BioContainer.MountInfo.containerPathOf bcContext.Mount inputFile
    
        //construct targetP command line string
        let tp = "targetp"::TargetpParams.makeCmd opt
        async {
            let! targepResult =
                BioContainer.execReturnAsync bcContext (tp@[cPath])
        
            //read targetP output as csv and convert it into a TargetPItem
            let skipLines             = 1
            let skipLinesBeforeHeader = 6 //6
            let schemaMode = SchemaReader.Csv.Fill
            let csvReader = SchemaReader.Csv.CsvReader<TargetpItem>(SchemaMode=schemaMode)

            return csvReader.ReadFromString(targepResult,'\t',true,skipLines, skipLinesBeforeHeader)
        }

    let runWithMountedFile (bcContext:BioContainer.BcContext) (opt:TargetpParams) (inputFile:string) =
        runWithMountedFileAsync bcContext opt inputFile
        |> Async.RunSynchronously





    
    [<Obsolete("Use runWithStreamAsync instead")>]
    let runAsync bcContext (opt:TargetpParams) (fsaStream:Stream) = 
        let tp = "targetp"::TargetpParams.makeCmd opt
        let tmpFile = sprintf "/data/%A.fsa" (System.Guid.NewGuid())
        async {
            do!
                BioContainer.putStreamAsync bcContext fsaStream tmpFile
            let! targepResult =
                BioContainer.execReturnAsync bcContext (tp@[tmpFile])

            // CsV Reader
            let skipLines             = 1
            let skipLinesBeforeHeader = 6 //6
            let schemaMode = SchemaReader.Csv.Fill
            let csvReader = SchemaReader.Csv.CsvReader<TargetpItem>(SchemaMode=schemaMode)
                
            return csvReader.ReadFromString(targepResult,'\t',true,skipLines, skipLinesBeforeHeader)
 
        }
            
    [<Obsolete("Use runWithStream instead")>]
    let run bcContext (opt:TargetpParams) (fsaStream:Stream) = 
        runAsync bcContext opt fsaStream
        |> Async.RunSynchronously