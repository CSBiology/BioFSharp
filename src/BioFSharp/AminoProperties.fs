namespace BioFSharp

open FSharpAux

///Contains functionalities for obtaining included literary data on key amino acid properties
module AminoProperties =
    
    open System.Reflection
    open AminoAcidSymbols

    ///Union case of amino acid properties, referencing the according included information in this library. Use "initGetAminoProperty" function to obtain a simple mapping function
    type AminoProperty =
        // Hydrophobicity index (Argos et al., 1982)
        | HydrophobicityIndex
        // Hydrophobicity index (Fasman, 1989)
        | HydrophobicityFasman  
        // Melting point (Fasman, 1976)
        | MeltingPointFasman    
        // Optical rotation (Fasman, 1976)
        | OpticalRotationFasman 
        // pK-N (Fasman, 1976)
        | PK_NFasman            
        // pK-C (Fasman, 1976)
        | PK_CFasman            
        // Normalized frequency of beta-sheet (Crawford et al., 1973)
        | NormalizedBetaSheet 
        // Normalized frequency of middle helix (Crawford et al., 1973)
        | NormalizedHelix     
        // Normalized frequency of turn (Crawford et al., 1973)
        | NormalizedTurn      
        // Helix-coil equilibrium constant (Ptitsyn-Finkelstein, 1983)
        | HelixCoil 
        // Beta-coil equilibrium constant (Ptitsyn-Finkelstein, 1983)
        | BetaCoil
        //Alpha-helix propensity derived from designed sequences (Koehl-Levitt, 1999)
        | Helicity
        // Beta-sheet propensity derived from designed sequences (Koehl-Levitt, 1999)
        | BetaSheetPropensity
        // PRIFT index (Cornette et al., 1987)
        | Amphiphilicity      
        // NNEIG index (Cornette et al., 1987)
        | NNEIG
        // SWEIG index (Cornette et al., 1987)
        | SWEIG
        // PRILS index (Cornette et al., 1987)
        | PRILS
        // ALTFT index (Cornette et al., 1987)
        | ALTFT
        // ALTLS index (Cornette et al., 1987)
        | ALTLS
        // TOTFT index (Cornette et al., 1987)
        | TOTFT
        // TOTLS index (Cornette et al., 1987)
        | TOTLS
        // pKr (Christen, Jaussi, Benoit 2016)
        | PKr
        // Activation Gibbs energy of unfolding, pH9.0 (Yutani et al., 1987)
        | ActivationGibbsEnergy9
        // AA composition of MEM of single-spanning proteins (Nakashima-Nishikawa, 1992)
        | MEMofSingleSpanning  
        // Principal component I (Sneath, 1966)
        | PrincipalComponentI
        // Principal component II (Sneath, 1966)
        | PrincipalComponentII
        // Principal component III (Sneath, 1966)
        | PrincipalComponentIII
        // Principal component IV (Sneath, 1966)
        | PrincipalComponentIV
        // Hydration potential (Wolfenden et al., 1981)
        | HydrationPotential
        // Hydrophobicity index (Wolfenden et al., 1979)
        | HydrophobicityIndex2 
        // The Chou-Fasman parameter of the coil conformation (Charton-Charton, 1983)
        | ChouFasmanCoil
        // Average number of surrounding residues (Ponnuswamy et al., 1980)
        | AverageNumberSurroundingResidues
        // Interior composition of amino acids in intracellular proteins of mesophiles (percent) (Fukuchi-Nishikawa, 2001)
        | CompositionIntracellular
        // Composition of amino acids in extracellular proteins (percent) (Cedano et al., 1997)
        | CompositionExtracellular
        // Weights for alpha-helix at the window position of -3 (Qian-Sejnowski, 1988)
        | WeightsHelixMinus3
        // Helix formation parameters (delta delta G) (O'Neil-DeGrado, 1990)
        | HelixFormationParameters
        // Free energy in alpha-helical region (Munoz-Serrano, 1994)
        | FreeEnergyHelicalRegion
        // Average relative fractional occurrence in EL(i) (Rackovsky-Scheraga, 1982)
        | ELi


        static member toString = function
            | HydrophobicityIndex       -> "Hydrophobicity index (Argos et al., 1982)"
            | HydrophobicityFasman      -> "Hydrophobicity index (Fasman, 1989)"
            | HydrophobicityIndex2      -> "Hydrophobicity index (Wolfenden et al., 1979)"
            | MeltingPointFasman        -> "Melting point (Fasman, 1976)"
            | OpticalRotationFasman     -> "Optical rotation (Fasman, 1976)"
            | PK_NFasman                -> "pK-N (Fasman, 1976)"
            | PK_CFasman                -> "pK-C (Fasman, 1976)"
            | NormalizedBetaSheet       -> "Normalized frequency of beta-sheet (Crawford et al., 1973)"
            | NormalizedHelix           -> "Normalized frequency of middle helix (Crawford et al., 1973)"
            | NormalizedTurn            -> "Normalized frequency of turn (Crawford et al., 1973)"
            | HelixCoil                 -> "Helix-coil equilibrium constant (Ptitsyn-Finkelstein, 1983)"
            | BetaCoil                  -> "Beta-coil equilibrium constant (Ptitsyn-Finkelstein, 1983)"
            | Helicity                  -> "Alpha-helix propensity derived from designed sequences (Koehl-Levitt, 1999)"
            | BetaSheetPropensity       -> "Beta-sheet propensity derived from designed sequences (Koehl-Levitt, 1999)"
            | Amphiphilicity            -> "PRIFT index (Cornette et al., 1987)"
            | NNEIG                     -> "NNEIG index (Cornette et al., 1987)"
            | SWEIG                     -> "SWEIG index (Cornette et al., 1987)"
            | PRILS                     -> "PRILS index (Cornette et al., 1987)"
            | ALTFT                     -> "ALTFT index (Cornette et al., 1987)"
            | ALTLS                     -> "ALTLS index (Cornette et al., 1987)"
            | TOTFT                     -> "TOTFT index (Cornette et al., 1987)"
            | TOTLS                     -> "TOTLS index (Cornette et al., 1987)"                        
            | PKr                       -> "pKr (Christen, Jaussi, Benoit 2016)"
            | ActivationGibbsEnergy9    -> "Activation Gibbs energy of unfolding, pH9.0 (Yutani et al., 1987)"
            | MEMofSingleSpanning       -> "AA composition of MEM of single-spanning proteins (Nakashima-Nishikawa, 1992)"
            | PrincipalComponentI       -> "Principal component I (Sneath, 1966)"
            | PrincipalComponentII      -> "Principal component II (Sneath, 1966)"
            | PrincipalComponentIII     -> "Principal component III (Sneath, 1966)"
            | PrincipalComponentIV      -> "Principal component IV (Sneath, 1966)"
            | HydrationPotential        -> "Hydration potential (Wolfenden et al., 1981)"                                        
            | ChouFasmanCoil            -> "The Chou-Fasman parameter of the coil conformation (Charton-Charton, 1983)"
            | AverageNumberSurroundingResidues   -> "Average number of surrounding residues (Ponnuswamy et al., 1980)"
            | CompositionIntracellular  -> "Interior composition of amino acids in intracellular proteins of mesophiles (percent) (Fukuchi-Nishikawa, 2001)"
            | CompositionExtracellular  -> "Composition of amino acids in extracellular proteins (percent) (Cedano et al., 1997)"
            | WeightsHelixMinus3        -> "Weights for alpha-helix at the window position of -3 (Qian-Sejnowski, 1988)"
            | HelixFormationParameters  -> "Helix formation parameters (delta delta G) (O'Neil-DeGrado, 1990)"
            | FreeEnergyHelicalRegion   -> "Free energy in alpha-helical region (Munoz-Serrano, 1994)"
            | ELi                       -> "Average relative fractional occurrence in EL(i) (Rackovsky-Scheraga, 1982)"
            








    let private ofPropteryString propertyName (str:string) = 
        // TODO: validation
        str.Split(' ')
        |> Array.map (fun v -> 
            match AminoAcidSymbols.parseChar v.[0] with
            | _,Some ac -> ac, float v.[2..] 
            | _ -> failwithf "Error in AminoAcid code at given property: %s" propertyName )

    let private initAminoPropertyLookUp () =
        let assembly = Assembly.GetExecutingAssembly()
        let resourceName = "AminoAcidProperties.txt"
        use stream = assembly.GetManifestResourceStream(resourceName)
        use reader = new System.IO.StreamReader(stream)
        
        [
        while not reader.EndOfStream do
            let pname = reader.ReadLine()
            if not reader.EndOfStream && pname <> "" &&  not (pname.StartsWith("#")) then
                yield pname, ofPropteryString pname (reader.ReadLine())
        ] |> Map.ofList
        
    ///Returns a simple mapping function for the given amino acid property
    let initGetAminoProperty (property:AminoProperty) =
        // TODO: Memorize
        let lookUp = initAminoPropertyLookUp()
        let arr = lookUp.[AminoProperty.toString property]
        let av = Array.create 26 nan
        for a in arr do 
            av.[int (fst a) - 65] <- snd a
    
        (fun  (amino:AminoAcidSymbol) -> 
            let index = int amino - 65
            // Ignores Gap and Term
            if index < 0 || index > 25 then
                nan
            else
                av.[index])

    ///Returns a simple mapping function for the given amino acid property. Normalizes the values to the Z-Norm scale
    let initGetAminoPropertyZnorm (property:AminoProperty) =
        // TODO: Memorize
        let lookUp = initAminoPropertyLookUp()
        let arr = 
            lookUp.[AminoProperty.toString property]

        let mean = 
            arr |> Array.averageBy snd
        let sd =
            arr 
            |> Array.sumBy (fun (_,x) -> (x-mean) * (x-mean) )
            |> fun x -> sqrt(x / (float arr.Length))

        let arrZ = arr |> Array.map (fun (n,x) -> n,(x - mean) / sd)
        let av = Array.create 26 nan
        for a in arrZ do 
            av.[int (fst a) - 65] <- snd a
    
        (fun  (amino:AminoAcidSymbol) -> av.[int amino - 65])


    /// Returns an array of sliding windows based property averages.
    /// Each window contains the n elements surrounding the current element
    let ofWindowedBioArray n (pf:'a->float) (source:BioArray.BioArray<'a>) =
        if n < 0 then invalidArg "n" "n must be a positive integer"
        
        let lastIndex = source.Length - 1
        let arrSize = n + n + 1 |> float
        
        let pfArr = source |> Array.map pf
        
        Array.init source.Length
            (fun i -> 
                match i with
                | pos when pos < n -> 
                    Array.foldSub (+) 0.0 pfArr 0 (pos+n) / float (pos+n+1)
                | pos when pos+n > lastIndex  ->  
                    Array.foldSub (+) 0.0 pfArr (pos-n) lastIndex / float (source.Length-pos+n)
                | _ -> 
                    Array.foldSub (+) 0.0 pfArr (i-n) (i+n) / arrSize
            )

    /// Returns an array of sliding windows based property averages.
    /// Each window contains the n elements surrounding the current element
    //                
    let ofBioArrayRndNorm (sampler:unit->'a) n sampleSize (pf:'a->float) (source:BioArray.BioArray<'a>) =
        let proSeqSampler size =
            if source.Length <= (n+n) then failwithf "Error: Source sequence must be at least of the length 2n"
            Array.init size (fun _ -> sampler () )
            |> ofWindowedBioArray n pf
            // TODO: Check length!!
            |> fun arr -> arr.[n-1 .. arr.Length-n-1]   
            //Array.init n (fun _ -> samples length)    
    
        let rndData =
            //if source.Length < windowSize then failwith "Warning! Length of source must equal windowSize or greater." else
            Array.init sampleSize (fun _ -> proSeqSampler source.Length)
            |> JaggedArray.transpose

        let rndMean =
            rndData
            |> Array.map (fun arr -> Array.average arr)
            |> Array.average

        let rndStd =
            rndData
            |> Array.mapi 
                (fun i arr -> 
                    arr 
                    |> Array.fold (fun acc x -> acc + (x-rndMean) * (x-rndMean) ) 0.0
                    |> fun x -> x / float arr.Length
                    |> sqrt 
                ) 
            |> Array.average

        source
        |> ofWindowedBioArray n pf
        |> Array.map (fun item -> (item-rndMean) / rndStd)
        

