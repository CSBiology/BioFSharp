namespace BioFSharp.IO

open System
open FSharpAux
open FSharpAux.IO

module GAF =
    
    type GAFEntry = {
        Database            : string
        DbObjectID          : string
        DbObjectSymbol      : string
        Qualifier           : string []
        GoTerm              : string
        DbReference         : string []
        Evidence            : string
        WithFrom            : string []
        Aspect              : string
        DbObjectName        : string
        DbObjectSynonym     : string []
        DbObjectType        : string
        Taxon               : string []
        Date                : System.DateTime
        AssignedBy          : string
        AnnotationExtension : string [] option
        GeneProductFormId   : string option
                    }

    let createGAFEntry (str:string) version2 =  
        let split = str.Split([|'\t'|])
        { 
        Database            = split.[0]
        DbObjectID          = split.[1]
        DbObjectSymbol      = split.[2]
        Qualifier           = split.[3].Split([|'|'|])
        GoTerm              = split.[4]
        DbReference         = split.[5].Split([|'|'|])
        Evidence            = split.[6]
        WithFrom            = split.[7].Split([|'|'|])
        Aspect              = split.[8]
        DbObjectName        = split.[9]
        DbObjectSynonym     = split.[10].Split([|'|'|])
        DbObjectType        = split.[11]
        Taxon               = split.[12].Split([|'|'|])
        Date                = System.DateTime.ParseExact(split.[13],"yyyyMMdd",null).Date
        AssignedBy          = split.[14]
        AnnotationExtension = if version2 then Some (split.[15].Split([|','|])) else None
        GeneProductFormId   = if version2 then Some  split.[16]                 else None
        }
    
    type GAF = {
        Header  : seq<string>
        Entries : seq<GAFEntry>
        }

    let fromFile filepath :GAF=
        let strEnumerator = (FileIO.readFile(filepath)).GetEnumerator()

        let version = 
            strEnumerator.MoveNext() |> ignore
            strEnumerator.Current

        let isVersion2 = 
            version.StartsWith("!gaf-version: 2")

        let rec parseSingle (accE:GAFEntry list) (accH:string list)=
            if strEnumerator.MoveNext() then 
                let currentString = strEnumerator.Current
                if currentString.StartsWith("!") then 
                    parseSingle accE (currentString::accH) 
                else 
                    parseSingle ((createGAFEntry currentString isVersion2)::accE) accH

            else 
                {Header =  accH |> List.rev |> Seq.cast
                 Entries = accE |> List.rev |> Seq.cast}   

        parseSingle [] [version]

    let toFile filepath (gaf: GAF) : unit =
        let isVersion2 = 
            (gaf.Header |> Seq.item 0).StartsWith("!gaf-version: 2")

        let toString (entry: GAFEntry) : string =
            [
            entry.Database          
            entry.DbObjectID
            entry.DbObjectSymbol
            entry.Qualifier         |> String.concat "|"
            entry.GoTerm
            entry.DbReference       |> String.concat "|"
            entry.Evidence
            entry.WithFrom          |> String.concat "|"
            entry.Aspect
            entry.DbObjectName
            entry.DbObjectSynonym   |> String.concat "|"
            entry.DbObjectType
            entry.Taxon             |> String.concat "|"
            entry.Date.ToString("yyyyMMdd")
            entry.AssignedBy    
            entry.AnnotationExtension |> fun x -> if isVersion2 then x.Value |> String.concat "," else ""   //adds additional tabs to file if version is <2
            entry.GeneProductFormId   |> fun x -> if isVersion2 then x.Value else ""                        //adds additional tabs to file if version is <2
            ]
            |> String.concat "\t"

        let toArr =
            Seq.append gaf.Header (gaf.Entries |> Seq.map toString)
        System.IO.File.WriteAllLines(filepath,toArr)




//\bin\BioFSharp.IO\net47\FSharpAux.dll"
//\bin\BioFSharp.IO\net47\BioFSharp.dll"
//\bin\BioFSharp.IO\net47\BioFSharp.IO.dll"