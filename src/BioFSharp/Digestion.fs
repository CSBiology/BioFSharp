namespace BioFSharp

module Digestion =
    
    open AminoAcids

    // p4 p3 p2 p1 || p1' p2'
    type Protease = {
        Name : string
        Expression : AminoAcid option -> AminoAcid option
                  -> AminoAcid option -> AminoAcid option
                  -> AminoAcid option-> AminoAcid option -> bool    
        }
    let createProtease name f =
         {Name = name; Expression = f}

    type DigestedPeptide = {
        ProteinID: int
        MissCleavages: int
        MissCleavageStart:int
        MissCleavageEnd: int
        PepSequence: AminoAcid list
        }

    let createDigestedPeptide proteinID missCleavages missCleavageStart missCleavageEnd pepSequence = {
         ProteinID=proteinID
         MissCleavages=missCleavages
         MissCleavageStart=missCleavageStart
         MissCleavageEnd=missCleavageEnd
         PepSequence=pepSequence
         }
   
    let trypsin = createProtease "Trypsin" 
                        (let _p1 = [AminoAcid.Lys;AminoAcid.Arg] |> Set.ofList 
                         fun p4 p3 p2 p1 p1' p2' -> 
                            match p1,p1' with
                            | Some a1,Some a1' -> _p1.Contains(a1) && not (a1' = AminoAcid.Pro)
                            | _   -> false                     
                         )

    let isCutingSite (protease:Protease) (arr:AminoAcid option[]) =
        match arr with
        | [|p4; p3; p2; p1; p1'; p2';|] -> protease.Expression p4 p3 p2 p1 p1' p2'
        | _ -> false

    /// Returns current value,array tuple (current, [|prefix; current; suffix|])
    let motivy prefixLength suffixLength (source: 'T []) =    
        if prefixLength < 0 then invalidArg "prefixLength" "Input must be non negative"
        if suffixLength < 0 then invalidArg "suffixLength" "Input must be non negative"
        let windowSize = prefixLength + suffixLength + 1

        Array.init (source.Length) 
            (fun i ->
                let motive =
                    Array.init windowSize 
                        (fun ii -> 
                            if i+ii < prefixLength || (i+ii-prefixLength) > (source.Length-1) then
                                None 
                            else
                                Some source.[i+ii-prefixLength])
                source.[i],motive
            )

    /// Takes Proteinsequence as input and returns Array of resulting DigestedPeptides
    let digest (protease: Protease) (proteinID: int) (aas: AminoAcid []) =
        let aasLength = aas.Length
        let rec groupAfter f acc lowercounter counter (aasWithOption: (AminoAcid*'a []) []) =
            if counter = aasLength-1 then (createDigestedPeptide proteinID 0 (lowercounter+1) (counter+1) (aas.[lowercounter.. counter]|> Array.toList))::acc |> List.rev 
            else 
                match (f aasWithOption.[counter]) with
                | true  -> groupAfter f ((createDigestedPeptide proteinID 0 (lowercounter+1) (counter+1) (aas.[lowercounter.. counter]|> Array.toList))::acc) (counter+1) (counter+1) aasWithOption 
                | false -> groupAfter f acc lowercounter (counter+1) aasWithOption
        aas 
        |> motivy 3 2 
        |> (groupAfter (fun (c,arr) -> isCutingSite protease arr) [] 0 0) 
        |> List.toArray

    /// Takes Array of DigestedPeptides and and returns Array of DigestedPeptides including those resulting of one or more Misscleavage events
    let concernMissCleavages (minMissCleavages:int) (maxMisscleavages:int) (digestedPeptidesA:DigestedPeptide []) =
        let lengthOfPeptideL = digestedPeptidesA.Length
        let minToMaxMissCleavagesL = [minMissCleavages.. maxMisscleavages]
        let rec connectDigestedPeptides acc (digestedPeptidesA: DigestedPeptide []) (fstPepIdx:int) currentMissCleavages (lastPepIdx:int) =
            if lengthOfPeptideL < lastPepIdx then acc
            else
            match lastPepIdx with
            |x when lastPepIdx = lengthOfPeptideL -> acc
            |_ ->   
                let currentPeptideSeq = 
                    (digestedPeptidesA.[fstPepIdx.. lastPepIdx]) 
                    |> Array.map (fun digpep -> digpep.PepSequence) 
                    |> List.concat
                let currentPeptide = 
                    createDigestedPeptide digestedPeptidesA.[1].ProteinID (currentMissCleavages+1) digestedPeptidesA.[fstPepIdx].MissCleavageStart 
                        digestedPeptidesA.[lastPepIdx].MissCleavageEnd currentPeptideSeq
                    
                connectDigestedPeptides (currentPeptide::acc) digestedPeptidesA (fstPepIdx+1) currentMissCleavages (lastPepIdx+1) 
        
        minToMaxMissCleavagesL
        |> List.mapi (connectDigestedPeptides [] (digestedPeptidesA) 0) 
        |> List.concat
        |> Array.ofList
        |> Array.append digestedPeptidesA