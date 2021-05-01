namespace BioFSharp.BioContainers

module TargetP2 =

    open BioFSharp.BioContainers
    open BioFSharp.BioContainers.BioContainer
    open FSharpAux
    open FSharpAux.IO
    open FSharpAux.IO.SchemaReader.Attribute

    type TargetPResult = {
        [<FieldAttribute(0)>]
        ID          : string
        [<FieldAttribute(1)>]
        Prediction  : string
        [<FieldAttribute(2)>]
        NoTP        : float
        [<FieldAttribute(3)>]
        SP          : float
        [<FieldAttribute(4)>]
        Mtp         : float
        [<FieldAttribute(5)>]
        CSPosition  : string
    }

    type TargetP2OutputFormat =
        | Short
        | Long

        static member make = function
            | Short -> "short"    
            | Long  -> "long"

    type TargetP2Organism =
        | Plant   
        | NonPlant

        static member make = function
            | Plant     -> "pl"
            | NonPlant  -> "non-pl"

    type TargetP2PlotFormat =
        | Png
        | Eps
        | NoFormat

        static member make = function
            | Png       -> "png"
            | Eps       -> "eps"
            | NoFormat  -> "none"

    type TargetP2Params =
        | Batch             of int
        | FastaInputFile    of string
        | Format            of TargetP2OutputFormat
        | GFF3
        | Mature
        | Organism          of TargetP2Organism
        | Plot              of TargetP2PlotFormat
        | Prefix            of string
        | Stdout
        | Tmp               of string
        | Verbose           of bool
        | Version

        static member makeCmd = function
            | Batch           b     -> ["-batch"; string b]
            | FastaInputFile  path  -> ["-fasta"; path]
            | Format          f     -> ["-format"; TargetP2OutputFormat.make f]
            | GFF3                  -> ["-gff3"]
            | Mature                -> ["-mature"]
            | Organism        o     -> ["-org"; TargetP2Organism.make o]
            | Plot            p     -> ["-plot"; TargetP2PlotFormat.make p]
            | Prefix          p     -> ["-prefix"; p]
            | Stdout                -> ["-stdout"]
            | Tmp             path  -> ["-tmp"; path]
            | Verbose         v     -> [sprintf "-verbose=%b" v]
            | Version               -> ["-version"]

        static member makeCmdWith (m: MountInfo) = function
            | Batch           b     -> ["-batch"; string b]
            | FastaInputFile  path  -> ["-fasta"; MountInfo.containerPathOf m path]
            | Format          f     -> ["-format"; TargetP2OutputFormat.make f]
            | GFF3                  -> ["-gff3"]
            | Mature                -> ["-mature"]
            | Organism        o     -> ["-org"; TargetP2Organism.make o]
            | Plot            p     -> ["-plot"; TargetP2PlotFormat.make p]
            | Prefix          p     -> ["-prefix"; p]
            | Stdout                -> ["-stdout"]
            | Tmp             path  -> ["-tmp"; MountInfo.containerPathOf m path]
            | Verbose         v     -> [sprintf "-verbose=%b" v]
            | Version               -> ["-version"]

    let runWithMountAsync (bcContext:BioContainer.BcContext) (opt:TargetP2Params list) =
        
        let cmds = (opt |> List.map (TargetP2Params.makeCmdWith bcContext.Mount))
        let tp = "targetp"::(cmds |> List.concat)

        printfn "Starting process targetp\r\nparameters:"
        cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

        async {
            let! targepResult =
                BioContainer.execReturnAsync bcContext tp
        
            //read targetP output as csv and convert it into a TargetPItem
            let skipLines             = 2
            let schemaMode = SchemaReader.Csv.Fill
            let csvReader = SchemaReader.Csv.CsvReader<TargetPResult>(SchemaMode=schemaMode)

            return csvReader.ReadFromString(targepResult,'\t',false,skipLines)
            //return targepResult
        }

    let runWithMount (bcContext:BioContainer.BcContext) (opt:TargetP2Params list) =
        runWithMountAsync bcContext opt
        |> Async.RunSynchronously


    let getTargetP2Results (model: TargetP2Organism) (containerContext) (processedFastAPaths:string [])=
        processedFastAPaths
        |> Array.map (fun tmpPath -> 
            let containerparams = [
                FastaInputFile tmpPath
                Organism TargetP2Organism.NonPlant
                Format TargetP2OutputFormat.Short
                Stdout
            ]
            runWithMount containerContext containerparams )


    let getRawscores (containerContext) (processedFastAPaths:string [])=
        processedFastAPaths
        |> Array.map (fun tmpPath -> 
            let containerparams = [
                FastaInputFile tmpPath
                Organism TargetP2Organism.NonPlant
                Format TargetP2OutputFormat.Short
                Stdout
            ]
            runWithMount containerContext containerparams )
        |> Array.map (fun (tpres) -> tpres |> Seq.map (fun x -> x.Mtp))
        |> Seq.concat
        |> Array.ofSeq