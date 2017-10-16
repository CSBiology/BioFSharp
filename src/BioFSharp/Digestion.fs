namespace BioFSharp

///Contains types and functions needed to digest amino acid sequences with proteases
module Digestion =
    
    open AminoAcids

    /// p4 p3 p2 p1 || p1' p2'
    type Protease = {
        Name : string
        Expression : AminoAcid option -> AminoAcid option
                  -> AminoAcid option -> AminoAcid option
                  -> AminoAcid option-> AminoAcid option -> bool    
        }

    ///Creates a Protease from given name and motifFunction f
    let createProtease name f =
         {Name = name; Expression = f}

    /// Digested peptide
    // TODO: type of ProteinID to 'a; rename "missCleavageStart" to "cleavageStart"; Same for ..End..
    type DigestedPeptide = {
        ///Identifier of protein
        ProteinID: int
        ///
        MissCleavages: int
        MissCleavageStart:int
        MissCleavageEnd: int
        ///Sequence of peptide
        PepSequence: AminoAcid list
        }

    ///Creates digested peptide from given information
    let createDigestedPeptide proteinID missCleavages missCleavageStart missCleavageEnd pepSequence = {
         ProteinID=proteinID
         MissCleavages=missCleavages
         MissCleavageStart=missCleavageStart
         MissCleavageEnd=missCleavageEnd
         PepSequence=pepSequence
         }
   
    ///Returns true, if AminoAcid array resembles cutting site of given protease, else returns false
    // TODO: rename
    let isCutingSite (protease:Protease) (arr:AminoAcid option[]) =
        match arr with
        | [|p4; p3; p2; p1; p1'; p2';|] -> protease.Expression p4 p3 p2 p1 p1' p2'
        | _ -> false

    [<AutoOpen>]
    ///Contains functions for digesting AminoAcid sequences
    module BioSeq =
        
        /// Returns current value,array tuple (current, [|prefix; current; suffix)
        let motivy prefixLength suffixLength (source: seq<'T>) =    
            if prefixLength < 0 then invalidArg "prefixLength" "Input must be non negative"
            if suffixLength < 0 then invalidArg "suffixLength" "Input must be non negative"
            let windowSize = prefixLength + suffixLength + 1
            //if windowSize <= 0 then invalidArg "windowSize" "Input must be non zero"
    
            seq {   let arr = Array.create windowSize None
                    let r = ref (suffixLength ) 
                    let i = ref (prefixLength) 
                    use e = source.GetEnumerator()
                    while e.MoveNext() do
                        arr.[!i] <- Some e.Current   // ! get while := set
                        i := (!i + 1) % windowSize
                        if !r = 0 then
                            let tmp = Array.init windowSize (fun j -> arr.[(!i+j) % windowSize])
                            yield (tmp.[prefixLength].Value,tmp)
                        else
                        r := (!r - 1) 
                    // continue shifting for suffixLength  
                    let arr = Array.init windowSize (fun j -> arr.[(!i+j) % windowSize])
                    for i = 1 to suffixLength do
                        let tmp = Array.create windowSize None
                        Array.blit arr i tmp 0 (arr.Length-i)
                        yield (tmp.[prefixLength].Value,tmp)
                        }

        ///Cuts AminoAcid sequence at each place, where the sequence fits the cutting pattern of the protease. Returns sequence of resulting AminoAcid sequences
        let digest (protease:Protease) (aas:seq<AminoAcid>) =

            let groupAfter f (input:seq<_>) =     
                let rec group (en:System.Collections.Generic.IEnumerator<_>) cont acc c  =            
                        if not(f en.Current) && en.MoveNext() then
                            group en (fun l -> cont <| c::l) acc (fst en.Current) // modified!
                        else
                            (fun l -> cont <| c::l) []
                seq{
                    use en = input.GetEnumerator()
                    while en.MoveNext() do
                        yield group en id [] (fst en.Current) }// modified! 

            aas
            |> motivy 3 2
            |> groupAfter (fun (c,arr) -> isCutingSite protease arr)       


    [<AutoOpen>]
    ///Contains functions for digesting AminoAcid arrays
    module BioArray =

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
            if aasLength = 0 then [||]
            else
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
        let concernMissCleavages (minMissCleavages:int) (maxMisscleavages:int) (digestedPeptidesA:(DigestedPeptide) []) =
            if digestedPeptidesA = [||] then [||]
            else
            let lengthOfPeptideL = digestedPeptidesA.Length
            let minToMaxMissCleavagesL = [minMissCleavages.. maxMisscleavages]
            let rec connectDigestedPeptides acc (digestedPeptidesA: DigestedPeptide []) (fstPepIdx:int)  (lastPepIdx:int) currentMissCleavages =
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
                        createDigestedPeptide digestedPeptidesA.[0].ProteinID (currentMissCleavages) digestedPeptidesA.[fstPepIdx].MissCleavageStart 
                            digestedPeptidesA.[lastPepIdx].MissCleavageEnd currentPeptideSeq
                    
                    connectDigestedPeptides (currentPeptide::acc) digestedPeptidesA (fstPepIdx+1) (lastPepIdx+1) currentMissCleavages
        
            minToMaxMissCleavagesL
            |> List.map (fun x ->  (connectDigestedPeptides [] (digestedPeptidesA) 0 x x)) 
            |> List.concat
            |> Array.ofList

    ///Contains frequently needed proteases
    module Table = 
        ///Possible inputs: "Trypsin", "Lys-C"
        let getProteaseBy name = 
            match name with
            | "Trypsin" ->
                createProtease "Trypsin" (let _p1 = [AminoAcid.Lys;AminoAcid.Arg] |> Set.ofList 
                                          fun p4 p3 p2 p1 p1' p2' -> 
                                          match p1,p1' with
                                          | Some a1,Some a1' -> _p1.Contains(a1) && not (a1' = AminoAcid.Pro)
                                          | _   -> false                     
                                         )       
                 
                   
            | "Lys-C"  ->
                createProtease "Lys-C" (let _p1 = [AminoAcid.Lys] |> Set.ofList
                                        fun p4 p3 p2 p1 p1' p2' -> 
                                        match p1 with
                                        | Some a1 -> _p1.Contains(a1)
                                        | _ -> false
                                       )    