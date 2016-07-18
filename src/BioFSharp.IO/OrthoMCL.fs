namespace BioFSharp.IO

module OrthoMCL =
    
    open FSharp.Care.IO
    open FSharp.Care.IO.SchemaReader
    open FSharp.Care.IO.SchemaReader.Attribute

    type EvalueConverter() = 
        inherit ConverterAttribute()
        override this.convertToObj = 
            Converter.Collection(fun (strs : seq<string>) -> 
                (
                    FSharp.Care.String.tryParseFloatDefault nan (String.concat "e" strs)
                 )
                |> box)

    type OrthoMCL = {
        [<FieldAttribute(0)>]
        Query_SeqId   : string
        [<FieldAttribute(2)>]
        Subject_SeqId : string
        [<FieldAttribute(1)>]
        Orthomcl_group : string
        [<FieldAttribute([|3;4|])>]
        [<EvalueConverter()>]
        Evalue        : float
        [<FieldAttribute(5)>]
        Identity      : float
        [<FieldAttribute(6)>]
        Similarity    : float    
        }

    let no_Orthomcl_group = "NO_GROUP"

    let readOrthoMCL filePath =
        let csvReader = SchemaReader.Csv.CsvReader<OrthoMCL>(schemaMode=SchemaReader.Csv.Exact)
        csvReader.ReadFile(filePath,'\t',false)


