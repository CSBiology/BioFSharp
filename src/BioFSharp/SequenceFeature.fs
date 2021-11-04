namespace BioFSharp

type SequenceFeature = {
    Name: string
    //zero-based
    Start: int
    //zero-based
    End: int
    Length: int
    Abbreviation: char
    Metadata: Map<string,string>
    FeatureType: string
} with
    static member create 
        (
            name: string,
            featureStart: int,
            featureEnd: int,
            ?Abbreviation: char,
            ?Metadata: Map<string,string>,
            ?FeatureType: string
        ) =
            if featureStart < 0 || featureEnd < 0 || featureStart > featureEnd then
                failwith $"invalid feature stretch ({featureStart},{featureEnd})"
            else
                {
                    Name            = name        
                    Start           = featureStart
                    End             = featureEnd  
                    Length          = featureEnd - featureStart + 1
                    Abbreviation    = Abbreviation |> Option.defaultValue ' '
                    Metadata        = Metadata |> Option.defaultValue (Map.ofList [])
                    FeatureType     = FeatureType |> Option.defaultValue ""
                }

    static member tryGetIntersection (feature1:SequenceFeature) (feature2:SequenceFeature) =
        let s1,e1 = feature1.Start, feature1.End
        let s2,e2 = feature2.Start, feature2.End
        
        if (s2 > e1) || (s1 > e2) then
            None
        else
            Some ((max s1 s2), (min e1 e2))