namespace BioFSharp


type AnnotatedSequence<'T when 'T :> IBioItem> = 
    {
        Tag: string
        Sequence : seq<'T>
        Features: Map<string,SequenceFeature list>
    } 

module AnnotatedSequence =
    
    let create tag sequence (featureMap: Map<string,SequenceFeature list>) =

        let mutable hasInvalidFeatures = false
        let mutable invalidFeatures: (string*(SequenceFeature*SequenceFeature)) list = []

        let isOverlap (stretch1:int * int) (stretch2: int * int) =
            let s1, e1 = stretch1
            let s2, e2 = stretch2

            (s1 <= s2 && e1 >= s2)
            || (s1 <= s2 && e1 >= e2)
            || (s1 <= e2 && e1 >= e2)
            || (s1 >= s2 && e1 <= e2)

        featureMap
        |> Map.iter (fun key features ->
            let rec loop (featureList:SequenceFeature list) =
                match featureList with
                | f::rest -> 
                    for frest in rest do 
                        if isOverlap (f.Start, f.End) (frest.Start, frest.End) then 
                            hasInvalidFeatures <- true
                            invalidFeatures <- (key,(f,frest))::invalidFeatures
                    loop rest
                | [] -> ()
            loop features
        )
        if hasInvalidFeatures then
            failwith $"""At least one  sequence feature annotation collection contains overlapping annotations. This is not supported. Please annotate them as separate feature lists.
Offending annotations: 
{invalidFeatures}
"""         
        else
            {
                Tag = tag
                Sequence = sequence
                Features= featureMap
            }

    let addFeatures (featureKey: string) (features: SequenceFeature list) (anns: AnnotatedSequence<_>) =
        {
            anns with
                Features = 
                    if Map.containsKey featureKey anns.Features then
                        anns.Features |> Map.add featureKey (features @ anns.Features.[featureKey])
                    else
                        anns.Features |> Map.add featureKey features
                
        }

    let toStrings (anns: AnnotatedSequence<_>) =
        let sequenceString = anns.Sequence |> Seq.map (BioItem.symbol >> string) |> String.concat ""
        let emptyFormatString = [for i in 1 .. (Seq.length anns.Sequence) do yield " "] |> String.concat ""
        let featureFormats =
            anns.Features
            |> Map.map (fun key features -> 
                features
                |> Seq.fold (fun (acc:string) (feature) ->
                    let featureStretch = [for _ in 1 .. feature.Length do yield (feature.Abbreviation |> string)] |> String.concat ""
                    acc
                        .Remove(feature.Start, feature.Length)
                        .Insert(feature.Start, featureStretch)
                ) emptyFormatString
            )
        sequenceString,featureFormats

    let format (anns: AnnotatedSequence<_>) =
        let sequenceString, featureStrings = anns |> toStrings
        let longestId = 
            ["Sequence"; yield! (featureStrings |> Map.toList |> List.map fst)] 
            |> Seq.maxBy (fun x -> x.Length)
            |> fun s -> s.Length

        let ids = 
            ["Sequence"; yield! (featureStrings |> Map.toList |> List.map fst)]
            |> List.map (fun s -> s.PadRight(longestId+4))
        
        let blocks = 
            [sequenceString; yield! (featureStrings |> Map.toList |> List.map snd)]
            |> List.mapi (fun index seqString ->
                let id = ids.[index]
                let splits = 
                    seqString.ToCharArray() 
                    |> Seq.map string
                    |> Seq.chunkBySize 60 
                    |> Seq.map (String.concat "")

                let innerSplits = 
                    splits |> Seq.map (fun s -> 
                        s.ToCharArray() 
                        |> Seq.map string
                        |> Seq.chunkBySize 10 
                        |> Seq.map (String.concat "")
                )

                innerSplits 
                |> Seq.mapi (fun i strs ->  
                    let line = 
                        strs 
                        |> Seq.fold (fun acc elem -> sprintf "%s %s" acc elem) "" 
                    $"{id} {(string (((i+1)*60) - 60 + 1)).PadLeft(10)}{line}" 
                )
                |> Array.ofSeq
            )

        [for i in 0 .. blocks.[0].Length-1 do
            for b in blocks do yield b.[i]
        ]
        |> String.concat System.Environment.NewLine
        |> fun s -> $"{System.Environment.NewLine}{s}{System.Environment.NewLine}"