// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

open BioFSharp

[<EntryPoint>]
let main argv =
    
    let seqs = 
        [
            TaggedSequence.create "1" ("ATGTA" |> BioArray.ofNucleotideString)
            TaggedSequence.create "2" ("ATGTT" |> BioArray.ofNucleotideString)
        ]

    let x = PhylogeneticTree.ofTaggedBioSequences (fun a b -> 1.) seqs

    printfn "%A" x
    0