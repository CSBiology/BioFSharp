namespace BioFSharp.BioContainers

open System
open System.IO
open System.Threading


/// TMHMM 2.0c predicts transmembrane helices in proteins
module Tmhmm =
 
    open FSharpAux
    open FSharpAux.IO
    open FSharpAux.IO.SchemaReader.Attribute
 
    let ImageTmhmm = Docker.DockerId.ImageId "tmhmm"


    //type TmhmmParams =
    //    | Short
    //    | Verbose
    
    //    static member makeCmd = function
    //        | Short   -> ["-short"]
    //        | Verbose -> []

    //    static member make = function
    //        | Short   -> "-short"
    //        | Verbose -> ""


    
    type ConverterSplitString() = 
        inherit ConverterAttribute()
        override this.convertToObj = 
            (fun (str : string) -> 
                String.split '=' str
                |> Array.tryItem 1
                |> Option.defaultValue "" 
                |> box) |> SchemaReader.Converter.Single
                
    type ConverterSplitInt() = 
        inherit ConverterAttribute()
        override this.convertToObj = 
            (fun (str : string) -> 
                String.split '=' str
                |> Array.tryItem 1
                |> Option.defaultValue "0"
                |> FSharpAux.String.tryParseIntDefault 0 
                |> box) |> SchemaReader.Converter.Single

    type ConverterSplitFloat() = 
        inherit ConverterAttribute()
        override this.convertToObj = 
            (fun (str : string) -> 
                String.split '=' str
                |> Array.tryItem 1
                |> Option.defaultValue "nan"
                |> FSharpAux.String.tryParseFloatDefault nan 
                |> box) |> SchemaReader.Converter.Single

    // 5H2A_CRIGR      len=471 ExpAA=159.47    First60=0.02    PredHel=7       Topology=o77-99i112-134o149-171i192-214o234-256i325-347o357-379i
    type TmhmmItem = 
        { 
            [<FieldAttribute(0)>]                           Name     : string
            [<FieldAttribute(1)>][<ConverterSplitInt()>]    Len      : int
            [<FieldAttribute(2)>][<ConverterSplitFloat()>]  ExpAA    : float
            [<FieldAttribute(3)>][<ConverterSplitFloat()>]  First60  : float
            [<FieldAttribute(4)>][<ConverterSplitInt()>]    PredHel  : int
            [<FieldAttribute(5)>][<ConverterSplitString()>] Topology : string
            
        }


    let runAsync bcContext (fsaStream:Stream) = 
        let tp = "tmhmm"::["-short"]
        let tmpFile = sprintf "/data/%A.fsa" (System.Guid.NewGuid())
        async {
            do!
                BioContainer.putStreamAsync bcContext fsaStream tmpFile
            let! result =
                BioContainer.execReturnAsync bcContext (tp@[tmpFile])
            //do!
            //    BioContainer.disposeAsync bcContext
 
            // CsV Reader
            let skipLines             = 0
            let skipLinesBeforeHeader = 0
            let schemaMode = SchemaReader.Csv.Fill
            let csvReader = SchemaReader.Csv.CsvReader<TmhmmItem>(SchemaMode=schemaMode)
            
            return csvReader.ReadFromString(result,'\t',false,skipLines, skipLinesBeforeHeader)
 
        }
        
        
    let run bcContext (fsaStream:Stream) = 
        runAsync bcContext fsaStream
        |> Async.RunSynchronously
