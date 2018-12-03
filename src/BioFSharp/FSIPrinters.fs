namespace BioFSharp

///Pretty printers for various custom types in the library
module FSIPrinters =

    open BioFSharp
    
    ///print BioItems by using symbols for AminoAcids and Nucleotides, and the name of Modifications in [brackets]
    let printBioItem (a: 'a when 'a :> IBioItem) =
        match (a :> IBioItem) with
        | :? AminoAcids.AminoAcid | :? Nucleotides.Nucleotide-> sprintf "%c" (BioItem.symbol a)
        | :? ModificationInfo.Modification -> sprintf "[%s]" (BioItem.name a)
        | _ -> "?"

    ///print Biocollections in 6x10char blocks per line, preceeded by an index indicator
    let prettyPrintBioCollection (sequence:seq<'a> when 'a :> IBioItem ) =
        let stringSplits =
            sequence 
            |> Seq.chunkBySize 60
        let innerStringSplits =
            stringSplits
            |> Seq.map (Seq.chunkBySize 10)
        innerStringSplits 
        |> Seq.mapi (fun i strs ->  let lineIndex = (i * 60 + 1)
                                    let line = 
                                        strs 
                                        |> Seq.fold (fun acc elem -> sprintf "%s %s" acc (elem |> Seq.map printBioItem |> String.concat "")) "" 
                                    sprintf "%s%i %s" ([for x in 1 .. (10 - (string lineIndex).Length) do yield " "] |> String.concat "") lineIndex line )
        |> String.concat "\r\n"
        |> (fun prnt -> sprintf "\r\n%s\r\n" prnt)

    
