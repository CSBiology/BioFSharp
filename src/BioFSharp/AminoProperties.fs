namespace BioFSharp

open FSharp.Care.Collections

///Contains functionalities for obtaining included literary data on key amino acid properties
module AminoProperties =
    
    open System.Reflection
    open AminoAcidSymbols

    ///Union case of amino acid properties, referencing the according included information in this library. Use "initGetAminoProperty" function to obtain a simple mapping function
    type AminoProperty =
        | HydrophobicityIndex
        | HydrophobicityFasman
        | BetaSheet           
        | Coil                
        | Helicity            
        | Amphiphilicity      
        | PKr

        static member toString = function
            | HydrophobicityIndex  -> "Hydrophobicity index (Argos et al., 1982)"
            | HydrophobicityFasman -> "Hydrophobicity index (Fasman, 1989)"
            | BetaSheet            -> "Normalized frequency of beta-sheet (Crawford et al., 1973)"
            | Coil                 -> "Helix-coil equilibrium constant (Ptitsyn-Finkelstein, 1983)"
            | Helicity             -> "Alpha-helix propensity derived from designed sequences (Koehl-Levitt, 1999)"
            | Amphiphilicity       -> "PRIFT index (Cornette et al., 1987)"
            | PKr                  -> "pKr (Christen, Jaussi, Benoit 2016)"

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
            if not reader.EndOfStream && pname <> "" then
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
        

