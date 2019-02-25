namespace BioFSharp.ML

///DPPOP - DeeP Peptide Observability Predictor.
///
///d::pPop uses a deep neural network to predict proteotypic peptides for proteins of interest.
///
///See the publication: https://www.frontiersin.org/articles/10.3389/fpls.2018.01559/full
module DPPOP =

    // use old functionality due to recent changes in original library, needs fixing later
    module internal IsoelectricPoint2 = 
        open BioFSharp
        open AminoAcidSymbols
        open AminoProperties

        ///Finds the value in an interval for which a given function returns a value close to 0 
        let private tryFindRoot func accuracy lowerBound upperBound maxIter = 
            let acc = abs accuracy
            let rec loop a b i = 
                let c = (a + b)/2.
                let fc = func c
                if (abs fc) < acc then 
                    Some c
                else
                    if i = maxIter then None
                    else 
                        if sign fc = sign (func a) then loop c b (i+1)
                        else loop a c (i+1)

            let checkConditions a b = 
                let fa = func a
                let fb = func b
                if (abs fa) < acc then Some a
                elif (abs fb) < acc then Some b
                else 
                    if fa < 0. then 
                        if fb > 0. then
                            loop a b 0
                        else None
                    else 
                        if fb < 0. then 
                            loop a b 0
                        else None
            if lowerBound < upperBound then checkConditions lowerBound upperBound
            else checkConditions upperBound lowerBound

        ///Maps AminoAcidSymbol to default pK value of it's sidechain. Returns 0.0 if sidechain is neither acidic nor basic
        let getpKr  = initGetAminoProperty AminoProperty.PKr

        ///Finds the pH for which the global charge of the aaSeq is closer to 0 than the given accuracy.
        let tryFind (pKrFunc: AminoAcidSymbol -> float) accuracy (aaSeq : AminoAcidSymbol seq) = 
            let en = aaSeq.GetEnumerator()
            let compVec = Array.zeroCreate 26
            let rec loop current =
                match en.MoveNext() with
                | false ->
                    current
                | true ->
                    let index = (int (BioItem.symbol en.Current)) - 65
                    compVec.[index] <- compVec.[index] + 1
                    loop en.Current
            match en.MoveNext() with 
            |false -> None
            |true -> 
                compVec.[(int (BioItem.symbol en.Current)) - 65] <- 1            
                let nTerm,cTerm = en.Current, loop en.Current
            
                let f pH = 
                    let posChargeState = 
                        let CR = 10. ** (7.4 - pH)
                        CR/(CR+1.)
                        +
                        Seq.fold 
                            (fun chargeState aa -> 
                                let CR = 10. ** ((pKrFunc aa) - pH)
                                let partialCharge = CR/(CR+1.)
                                let count = float compVec.[(int (BioItem.symbol aa)) - 65]                  
                                chargeState + partialCharge * count
                                )
                            0.
                            AminoSymbolSetPosCharged
                    let negChargeState =
                        let CR = 10. ** (pH - 3.9)
                        CR/(CR+1.)
                        +
                        Seq.fold 
                            (fun chargeState aa -> 
                                let CR = 10. ** (pH - (pKrFunc aa))
                                let partialCharge = CR/(CR+1.)
                                let count = float compVec.[(int (BioItem.symbol aa)) - 65]                  
                                chargeState + partialCharge * count
                                )
                            0.
                            AminoSymbolSetNegCharged
                    posChargeState - negChargeState
                tryFindRoot f accuracy 0. 14. 50
                |> Option.map (fun pH -> pH, f pH)
    // use old functionality due to recent changes in original library, needs fixing later
    module internal Digestion2 =
        open BioFSharp
        open AminoAcids
        open FSharpAux

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
        //TODO: switch to better list system
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


    
    
        // Implementation of CleavageScore [ref: Prediction of Missed Cleavage Sites in Tryptic Peptides Aids Protein Identification in Proteomics
        //                                       Jennifer A. Siepen,1 Emma-Jayne Keevil,1 David Knight,1 and Simon J. Hubbard 
        //                                       PMC 2009 April 3 ]
        // As smaller the CleavageScore as more like is the cleavage
        // A CleavageScore over a threshold of 0.5 makes the cleavage unlikely
        //
        // Digestion site nomenclature  :     p4  p3  p2  p1  CS p'1 p'2 p'3 p'4 
        // Digestion site Order in Array:     p'1 p'2 p'3 p'4 CS p4  p3  p2  p1 
    
        /// Get cleaved probability
        module CleavageScore =
        
            open AminoAcidSymbols

            /// Get cleaved probability
            let getCleavedP (a:AminoAcidSymbol) index =
                match a with
                | Field AminoAcidSymbol.Ala -> Array.get (   [| 0.0186237213392114  ;  0.00420022515410901 ; 3.53002332739628e-05 ; 0.00133092924208546 ; -0.0049122447091631 ; -0.00626781185306248 ; -0.00113516316915836 ; 0.00392961759045261 ; 0.0 ; |] ) index          
                | Field AminoAcidSymbol.Cys -> Array.get (   [| 0.0173071318266984  ;  0.0182362544270455  ; 0.0058884097224481 ; 0.00192544607949062 ; 0.01095597703868 ; 0.0148254346379286 ; 0.0219373168460715 ; 0.0112312454730355 ; 0.0 ; |] ) index      
                | Field AminoAcidSymbol.Asp -> Array.get (   [| -0.007133815548037  ; -0.0118184470690743  ; 0.00654705296026112 ; 0.0067050281150376 ; 0.0141624132472289 ; 0.0099568688324829 ; 0.000929377266564833 ; -0.0162973823115694 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Glu -> Array.get (   [| -0.0124248587558356 ; -0.0232307606334326  ; 0.00181031130741747 ; 0.00716014784841427 ; 0.00818034396259026 ; -0.00222127914039957 ; 0.0028442937155881 ; 0.00491926832574982 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Phe -> Array.get (   [| 0.0119920426834694  ;  0.0139395144782525  ; 0.00468781345674047 ; 0.00297659657226406 ; 0.0101203450370329 ; 0.0144613722136596 ; 0.0280184052564126 ; 0.0181982754958097 ; 0.0 ; |] ) index 
                | Field AminoAcidSymbol.Gly -> Array.get (   [| -8.20312810020782e-05 ; -0.00290995754241101 ; 0.000209352903511755 ; 0.00371362222983421 ; -0.000318027050528464 ; 0.000868058270173546 ; 0.000330198733098511 ; -0.00346452584212206 ; 0.0 ; |] ) index     
                | Field AminoAcidSymbol.His -> Array.get (   [| 0.0117663765407453 ; 0.0184516214958325 ; 0.0133219958800885 ; 0.00914181836068942 ; 0.00578955397457641 ; 0.00455527468070269 ; 0.020885755843452 ; 0.00594142776223056 ; 0.0 ; |] ) index  
                | Field AminoAcidSymbol.Ile -> Array.get (   [| 0.0199421197114211 ; 0.0138750181207925 ; 0.00102285937033262 ; 0.00455976059407499 ; 0.00384378298672882 ; 0.00131189125848209 ; -0.000352115411225422 ; 0.00565904209703786 ; 0.0 ; |] ) index    
                | Field AminoAcidSymbol.Lys -> Array.get (   [| 0.00402270759345742 ; -0.0148551046432945 ; -0.0267866580116544 ; -0.043014650796067 ; -0.0426349905702423 ; -0.044153635141278 ; -0.0498530534413316 ; -0.0497578863798243 ; -0.0106125875004242 ; |] ) index
                | Field AminoAcidSymbol.Leu -> Array.get (   [| 0.0147945517930332 ; 0.00748424375471079 ; 0.00219706314393281 ; 0.00457080341133138 ; 0.00188686459291307 ; 0.00508630706016019 ; 0.0015271894185259 ; 0.000930249394099852 ; 0.0 ; |] ) index      
                | Field AminoAcidSymbol.Met -> Array.get (   [| 0.0212654895269526 ; -0.0087793419126029 ; -0.013142656172641 ; -0.00273285530984907 ; 0.00754132801219614 ; -0.00524147923293532 ; 0.0057657018440194 ; -0.0101222284297051 ; 0.0 ; |] ) index    
                | Field AminoAcidSymbol.Asn -> Array.get (   [| 0.0153786428896758 ; -0.00122862317011291 ; 0.00644852823507257 ; 0.0143452805729061 ; 0.00863951037114175 ; 0.0111926226367841 ; 0.010514806562435 ; 0.000459581898316416 ; 0.0 ; |] ) index   
                | Field AminoAcidSymbol.Pyl -> 0.    
                | Field AminoAcidSymbol.Pro -> Array.get (   [| -1.408135429711 ; -0.0151176661385177 ; 0.000582621641907323 ; 0.00559968095391454 ; 0.0103237181563291 ; 0.0150543502357306 ; 0.00487552628651554 ; 0.0258005600513561 ; 0.0 ; |] ) index    
                | Field AminoAcidSymbol.Gln -> Array.get (   [| 0.0218723992037776 ; 0.0101618904516775 ; 0.0109962160318145 ; 0.00779255049091823 ; -0.00238950679147767 ; 0.00636975003212183 ; 0.012161600167981 ; 0.0104019447570531 ; 0.0 ; |] ) index    
                | Field AminoAcidSymbol.Arg -> Array.get (   [| -0.0206030273840874 ; -0.0245971841699276 ; -0.0427821442802085 ; -0.0566332092070675 ; -0.0559191548111558 ; -0.0455394380519306 ; -0.0541455813655727 ; -0.0538609149609292 ; 0.0112126601842253 ; |] ) index     
                | Field AminoAcidSymbol.Ser -> Array.get (   [| -0.00414728931498034 ; -0.00607359115820411 ; 0.00688957312924048 ; -0.00101967408837821 ; 0.00155119425371577 ; -0.00188774397621617 ; -0.00179609780733301 ; 0.00120217171057805 ; 0.0 ; |] ) index        
                | Field AminoAcidSymbol.Thr -> Array.get (   [| 0.0115728837855243 ; 0.00871709724548706 ; 0.00208777500908572 ; 3.77150826628033e-06 ; 0.00437580160216219 ; 0.00526322191736816 ; -0.0022521384724719 ; 0.00746782714495857 ; 0.0 ; |] ) index     
                | Field AminoAcidSymbol.Sel -> 0.
                | Field AminoAcidSymbol.Val -> Array.get (   [| 0.00681194613657833 ; 0.0173429094275379 ; 0.00479136512294075 ; 0.00825865300614361 ; 0.00493316169438667 ; 0.00417320066605687 ; 0.00917321806055152 ; 0.00952970722162894 ; 0.0 ; |] ) index        
                | Field AminoAcidSymbol.Trp -> Array.get (   [| 0.0306856368818309 ; 0.00282917821310596 ; 0.00730387808155344 ; 0.0120257729838156 ; 0.00693320815473958 ; 0.0181272910523906 ; 0.0254494100003613 ; 0.0354451553685568 ; 0.0 ; |] ) index    
                | Field AminoAcidSymbol.Tyr -> Array.get (   [| 0.0194284810017644 ; 0.0127667737830556 ; 0.00498714111480968 ; 0.00476543997301542 ; -0.00523499887692041 ; 0.0152488432689032 ; 0.0194801608035318 ; 0.0168451463172139 ; 0.0 ; |] ) index   
             
                | Field AminoAcidSymbol.Xaa -> Array.get (   [| -0.0395949915379296 ; 0.00616249902274598 ; -0.0395949915379296 ; -0.0395949915379296 ; -0.011566267937686 ; -0.090747513985311 ; 0.0183969554397552 ; 0.0853437450703708 ; 0.0 ; |] ) index        
                | Field AminoAcidSymbol.Xle -> 0. // nan // TODO average IL L
                | Field AminoAcidSymbol.Glx -> Array.get (   [| 0.0853437450703708 ; nan ; nan ; nan ; 0.0853437450703708 ; 0.0853437450703708 ; nan ; nan ; nan ; |] ) index
                | Field AminoAcidSymbol.Asx -> Array.get (   [| nan ; 0.0853437450703708 ; nan ; 0.0853437450703708 ; 0.0853437450703708 ; 0.0853437450703708 ; nan ; nan ; nan ; |] ) index
                                       
                | Field AminoAcidSymbol.Gap ->  Array.get (   [| 0.0853437450703708 ; -0.0332362471694817 ; -0.00965307329340537 ; 0.0276120364797184 ; 0.0 ; 0.0 ; 0.0 ; 0.0 ; 0.0 ; |] ) index //+
                | Field AminoAcidSymbol.Ter -> Array.get (   [| 0.0 ; 0.0 ; 0.0 ; 0.0 ; 0.0809346261653139 ; 0.0688146873864017 ; -0.00598639055684963 ; -0.190862666868579 ; 0.0 ; |] ) index
                // | '+'      -> Array.get (   [| 0.0853437450703708 ; -0.0332362471694817 ; -0.00965307329340537 ; 0.0276120364797184 ; 0.0 ; 0.0 ; 0.0 ; 0.0 ; 0.0 ; |] ) index                                     
                | _ -> 0.


            /// Get missed probability
            let getMissedP (a:AminoAcidSymbol) index =
                match a with
                | Field AminoAcidSymbol.Ala -> Array.get (   [| -0.0978752075888718 ; -0.0198847901320386 ; -0.000162599309729288 ; -0.00618230263814537 ; 0.0219309154288239 ; 0.0277509980548175 ; 0.00518962066798522 ; -0.0185699461081312 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Cys -> Array.get (   [| -0.0900299925122872 ; -0.0955491607057649 ; -0.0281974726429136 ; -0.00897879962781697 ; -0.0543599833494246 ; -0.075678654240404 ; -0.118382665495958 ; -0.055836336165553 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Asp -> Array.get (   [| 0.0314193812682074 ; 0.0506231703962375 ; -0.0314932243949499 ; -0.0322881887450678 ; -0.0719369441334252 ; -0.0490509678757512 ; -0.00430576272835574 ; 0.0680412256217175 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Glu -> Array.get (   [| 0.053033404763815 ; 0.0933726218622424 ; -0.00843551965998847 ; -0.0345882434083597 ; -0.039797629165997 ; 0.0100851603709803 ; -0.0133442628224803 ; -0.0234020070165263 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Phe -> Array.get (   [| -0.0599479594033155 ; -0.0706875113660484 ; -0.0222660916680952 ; -0.0139772319849951 ; -0.0499143795822267 ; -0.0736193995313521 ; -0.159199188226274 ; -0.0953219453246492 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Gly -> Array.get (   [| 0.000377565111733786 ; 0.0131546928606713 ; -0.000965402581785676 ; -0.0175238898661841 ; 0.00146155865238389 ; -0.00402007022809444 ; -0.00152385762818177 ; 0.0156073024880689 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.His -> Array.get (   [| -0.0587235591508078 ; -0.0968402609563878 ; -0.0672480561621474 ; -0.0447764411487796 ; -0.0277053940254705 ; -0.021617235705912 ; -0.111752086596945 ; -0.0284616589594472 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Ile -> Array.get (   [| -0.105900364403556 ; -0.0703267734771773 ; -0.00474174668456256 ; -0.0216391778358078 ; -0.0181538887728747 ; -0.00609311139032893 ; 0.00161786388127948 ; -0.0270567632506103 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Lys -> Array.get (   [| -0.0190217159063933 ; 0.0625277225670337 ; 0.105665409640322 ; 0.156626255550412 ; 0.155520296424895 ; 0.159921600947166 ; 0.175921830105783 ; 0.175661135601301 ; 0.045780112741825 ; |] ) index 
                | Field AminoAcidSymbol.Leu -> Array.get (   [| -0.0755035242375552 ; -0.0362350755152361 ; -0.0102637359244067 ; -0.0216931977051885 ; -0.00879665556808435 ; -0.0242240132320469 ; -0.00710306174657736 ; -0.00430982772116158 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Met -> Array.get (   [| -0.114132974344623 ; 0.0382858518051329 ; 0.0558648649150308 ; 0.0123678582184007 ; -0.0365259139536316 ; 0.0233533371391882 ; -0.0275867644275067 ; 0.0437912585812453 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Asn -> Array.get (   [| -0.0788299287119621 ; 0.00561354121363955 ; -0.0309983090244586 ; -0.0729651571272533 ; -0.0421668411875381 ; -0.0556288300737603 ; -0.05200619945784 ; -0.00212273527092521 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Pyl -> 0.
                | Field AminoAcidSymbol.Pro -> Array.get (   [| 0.734415886060518 ; 0.0635380307218723 ; -0.00269318400929317 ; -0.0267621290007437 ; -0.0509913667336383 ; -0.0769793817145727 ; -0.0231870591559591 ; -0.143797826856209 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Gln -> Array.get (   [| -0.117969916479998 ; -0.0501341304713985 ; -0.0545754266914563 ; -0.0378086424322195 ; 0.0108374287670045 ; -0.0306030716008546 ; -0.0608706268016039 ; -0.05140647211982 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Arg -> Array.get (   [| 0.0839923310796518 ; 0.098149568952218 ; 0.15594940772927 ; 0.193963194795178 ; 0.192111114311861 ; 0.163886132848834 ; 0.187463071487379 ; 0.186710603099994 ; -0.0557364696622383 ; |] ) index
                | Field AminoAcidSymbol.Ser -> Array.get (   [| 0.0186037055032237 ; 0.026922998856415 ; -0.0332190722669007 ; 0.00466508103872825 ; -0.00721584368297066 ; 0.00858895217038756 ; 0.00817672965188891 ; -0.00557951790496735 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Thr -> Array.get (   [| -0.0576769689205623 ; -0.0425687027718217 ; -0.00974617373848127 ; -1.73687197929421e-05 ; -0.0207404654555115 ; -0.0250966660897753 ; 0.0102232743918174 ; -0.0361514776936105 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Sel -> 0.
                | Field AminoAcidSymbol.Val -> Array.get (   [| -0.0328272141698239 ; -0.0902410077930348 ; -0.0227738520402987 ; -0.0402005991445456 ; -0.0234703056642569 ; -0.0197532660165449 ; -0.0449401919765487 ; -0.0468044755798308 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Trp -> Array.get (   [| -0.178586262592309 ; -0.0132720175157153 ; -0.0353176592986856 ; -0.0601313220751986 ; -0.0334395263233619 ; -0.0948976405530249 ; -0.141415693210588 ; -0.215820560584245 ; 0.0 ; |] ) index 
                | Field AminoAcidSymbol.Tyr -> Array.get (   [| -0.1027532603019 ; -0.0641827923770768 ; -0.0237357867488708 ; -0.0226466622914581 ; 0.0233253947565591 ; -0.0780881261688707 ; -0.103068708688225 ; -0.0873159356295266 ; 0.0 ; |] ) index
             
                | Field AminoAcidSymbol.Xaa -> Array.get (   [| 0.146525928646086 ; -0.0295653304095948 ; 0.146525928646086 ; 0.146525928646086 ; 0.0496159156380304 ; 0.271464665254386 ; -0.0965121200402077 ; 0.0 ; 0.0 ; |] ) index
                | Field AminoAcidSymbol.Xle -> 0. // nan // TODO average IL L                                                                                             
                | Field AminoAcidSymbol.Glx -> Array.get (   [| 0.0 ; nan ; 0.0 ; nan ; 0.0 ; 0.0 ; 0.0 ; 0.0 ;  nan; |] ) index
                | Field AminoAcidSymbol.Asx -> Array.get (   [| nan ; 0.0 ; nan ; 0.0 ; 0.0 ; 0.0 ; nan ; nan ; nan ; |] ) index
                                       
                | Field AminoAcidSymbol.Gap -> 0.
                | Field AminoAcidSymbol.Ter -> Array.get (   [| 0.0 ; 0.0 ; 0.0 ; 0.0 ; -1.2470492746235 ; -0.679188613161495 ; 0.0265506115693381 ; 0.421226985587719 ; 0.0 ; |] ) index       
                // Nterminal    | '+'      -> Array.get (   [| 0.0 ; 0.126871240649617 ; 0.0418777246061289 ; -0.156330115182119 ; 0.0 ; 0.0 ; 0.0 ; 0.0 ; 0.0 ; |] ) index                                     
                | _ -> 0.

        

            let calculateCleavageScore (source:BioArray.BioArray<AminoAcidSymbol>) =
                let n = 4       
                let lastIndex = source.Length - 1
        
                let get a i = 
                    (getMissedP a i) - (getCleavedP a i)
        
                Array.init source.Length
                    (fun i -> 
                        match i with
                        | pos when pos < n -> 
                            Array.foldSub (fun (i,acc) v -> (i+1,acc + get v i) ) (0,0.0) source 0 (pos+n) 
                            |> snd
                        | pos when pos+n > lastIndex  ->  
                            Array.foldSub (fun (i,acc) v -> (i+1,acc + get v i) ) (0,0.0) source (pos-n) lastIndex 
                            |> snd
                        | _ -> 
                            Array.foldSub (fun (i,acc) v -> (i+1,acc + get v i) ) (0,0.0) source (i-n) (i+n) 
                            |> snd
                    )

    open System.Collections.Generic
    open System.Reflection
    open FSharpAux
    open FSharpAux.IO
    open BioFSharp
    open BioFSharp.IO
    open BioFSharp.BioArray
    open BioFSharp.Digestion
    open BioFSharp.AminoAcidSymbols
    open CNTK

    /// Record type containing feature vector, proteinId, and sequence of a peptide
    type PredictionInput = {
        ///Feature vector
        Data      : float[]
        ///ID of the protein this peptide maps to
        ProtId    : string
        ///Sequence of the peptide
        Sequence  : string
    }

    /// returns a PredictionInput type given the values for the record fields
    let createPredictionInput data protID sequence  = {
        Data      = data
        ProtId    = protID
        Sequence  = sequence
        }

    /// Record type containing prediction score, proteinId, and sequence of a peptide
    type PredictionOutput = {
        ///ID of the protein this peptide maps to
        ProteinId: string
        ///Sequence of the peptide
        Sequence: string
        ///observability score returned by dppop prediction
        PredictionScore: float
    }
    /// returns a PredictionOutput type given the values for the record fields
    let createPredictionOutput pId sequence score = {ProteinId = pId; Sequence = sequence; PredictionScore = score}

    ///Contains functions to extract the features used in dppop to classify peptides for observability prediction
    module Classification =
        open FSharpAux

        let private peptideFeatures =
            [|
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.ActivationGibbsEnergy9;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.MEMofSingleSpanning;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.PrincipalComponentII;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.HydrophobicityIndex2;        
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.ChouFasmanCoil;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.AverageNumberSurroundingResidues;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.CompositionIntracellular;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.WeightsHelixMinus3;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.HelixFormationParameters;        
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.FreeEnergyHelicalRegion;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.ELi;        
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.CompositionExtracellular;
                AminoProperties.initGetAminoProperty AminoProperties.AminoProperty.HydrophobicityIndex;
            |]


        ///Tryptic digestion of an amino acid sequence with the ability to control the maximal amount of misscleavages and filtering of a minimal peptide length
        let digestTrypticWith (maxMissCleavages:int) (minPeptideLength:int) (aminoAcidSeq:BioArray<AminoAcids.AminoAcid>) =
            Digestion2.BioArray.digest (Digestion2.Table.getProteaseBy "Trypsin") 0 aminoAcidSeq
            |> Digestion2.BioArray.concernMissCleavages 0 maxMissCleavages
            |> Seq.map (fun p -> p.PepSequence |> List.toArray)
            |> Seq.filter (fun p -> p.Length > minPeptideLength)

        ///Tryptic Digestion2 of an amino acid sequence with the settings used for the dppop web API
        let digestTryptic (aminoAcidSeq:BioArray<AminoAcids.AminoAcid>) =
            aminoAcidSeq
            |> digestTrypticWith 0 6 

        ///Returns a distinct set of peptides that map uniquely to a single protein from the given fasta input
        let getDistinctTrypticPeptidesFromFasta (fa:seq<FastA.FastaItem<BioArray<AminoAcids.AminoAcid>>>)= 
            //fileDir + "Chlamy_Cp.fastA"
            fa
            |> Seq.map (fun fi -> fi.Sequence |> Array.filter (not << AminoAcids.isTerminator))
            |> Seq.collect digestTryptic
            |> Seq.map BioArray.toString
            |> Set.ofSeq

        ///Returns a distinct set of peptides that map uniquely to a single protein from the given fasta file input
        let getDistinctTrypticPeptidesFromFastaFile (filePath: string) = 
            //fileDir + "Chlamy_Cp.fastA"
            filePath
            |> FastA.fromFile BioArray.ofAminoAcidString
            |> getDistinctTrypticPeptidesFromFasta

        ///returns a map mapping from a (proteinID*sequence) touple to the three digestion efficiency scores in the form of a (float*float*float) tuple
        let private getDigestionEfficiency (protId) (sequence:BioArray.BioArray<AminoAcids.AminoAcid>) =
                let cs = sequence |> Array.map AminoAcidSymbols.aminoAcidSymbol |> Digestion2.CleavageScore.calculateCleavageScore
        
                let getStart index = if index < 2 then 0. else cs.[index-1]
                let getEnd index = if index >= cs.Length  then 0. else cs.[index]

                let calc (p:Digestion2.DigestedPeptide) =
                    if p.MissCleavages < 1 then
                        (protId,p.PepSequence |> Seq.map AminoAcidSymbols.aminoAcidSymbol |> Seq.toArray ),(getStart p.MissCleavageStart,0.,getEnd p.MissCleavageEnd)
                    else
                        let inter' = p.MissCleavages - 1 |> float
                        let s = getStart p.MissCleavageStart                
                        let e = getEnd p.MissCleavageEnd
                        // let inter' = inter - s - e
                        (protId,p.PepSequence |> Seq.map AminoAcidSymbols.aminoAcidSymbol |> Seq.toArray),(s,inter',e)

                Digestion2.BioArray.digest (Digestion2.Table.getProteaseBy "Trypsin") 0 sequence
                |> Digestion2.BioArray.concernMissCleavages 0 3
                |> Seq.map calc

        //adaption to new digestion module, not working correctly. 
        //let getDigestionEfficiency (protId) (sequence) =
        //    let cleavageScore = sequence |> Array.map AminoAcidSymbols.aminoAcidSymbol |> Digestion.CleavageScore.calculateCleavageScore
        
        //    //TODO: digestion hast changed from 1 based index to 0 based index, identify the numbers to change
        //    //ERROR HERE
        //    let getStart index = if index < 2 then 0. else cleavageScore.[index]//pretty sure this one
        //    let getEnd index = if index >= cleavageScore.Length-1  then 0. else cleavageScore.[index]


        //    let calc (p:DigestedPeptide<int>) =
        //        if p.MissCleavages < 1 then
        //            (protId,p.PepSequence |> Seq.map AminoAcidSymbols.aminoAcidSymbol |> Seq.toArray ),(getStart p.CleavageStart,0.,getEnd p.CleavageEnd)
        //        else
        //            let inter' = p.MissCleavages - 1 |> float // maybe this one
        //            let s = getStart p.CleavageStart                
        //            let e = getEnd p.CleavageEnd
        //            // let inter' = inter - s - e
        //            (protId,p.PepSequence |> Seq.map AminoAcidSymbols.aminoAcidSymbol |> Seq.toArray),(s,inter',e)

        //    Digestion.BioArray.digest (Digestion.Table.getProteaseBy "Trypsin") 0 sequence
        //    |> Digestion.BioArray.concernMissCleavages 0 3
        //    |> Seq.map calc

        ///returns a map mapping from a (proteinID*sequence) touple to the three digestion efficiency scores in the form of a (float*float*float) tuple from the input fasta item collection
        let createDigestionEfficiencyMapFromFasta (fa:seq<FastA.FastaItem<BioArray<AminoAcids.AminoAcid>>>) = 
            fa
            |> Seq.map (fun fi -> {fi with Sequence=fi.Sequence |> Array.filter (not << AminoAcids.isTerminator)})
            |> Seq.collect (fun fi -> getDigestionEfficiency fi.Header fi.Sequence)
            |> Map.ofSeq

        ///get the physicochemical properties of a peptide: length, MolecularWeight, NetCharge, PositiveCharge, NegativeCharge, piI, Relative frewuencies of polar, hydrophobic, and negatively charge amino acids
        let getPhysicochemicalProperties (peptide:BioArray<AminoAcidSymbol>) =
            let pI peptide = 
                //default function for pKr of charged aminoacids
                let pKrFunction = IsoelectricPoint2.getpKr
                match IsoelectricPoint2.tryFind pKrFunction 0.5 peptide with
                | Some (ph,pk) -> pk
                | None -> 0.
            let len = float peptide.Length
            let positiveCharge = peptide |> Seq.countIf AminoAcidSymbols.isPosCharged |> float
            let negativeCharge = peptide |> Seq.countIf AminoAcidSymbols.isNegCharged |> float
            [|
                //length
                len;
                //MolecularWeight
                BioArray.toAverageMass peptide
                //  NetCharge
                negativeCharge + positiveCharge
                // PositiveCharge, 
                positiveCharge
                // NegativeCharge        
                negativeCharge
                // piI
                //ERROR HERE
                pI peptide      
                //RelFreqPolar
                peptide |> Seq.countIf AminoAcidSymbols.isPolar |> fun x -> float x / len  
                //RelFreqHydrophobic
                peptide |> Seq.countIf AminoAcidSymbols.isHydrophobic |> fun x -> float x / len
                //RelFreqNegative 
                negativeCharge / len
            |]


        ///Returns a PredictionInput record type for the input peptide containing the calculated feature vector given a digestion efficiency map and the proteinId the peptide maps to
        let getPeptideFeatures (digestionEfficiencyMap:Map<(string*BioArray<AminoAcidSymbol>),(float*float*float)>) (protId:string) (peptide) =
            let peptide' = peptide |> Array.map (fun x -> (x :> IBioItem).Symbol) |> String.fromCharArray
            let getIndex (a:AminoAcidSymbol) = (int a) - 65
            // Relative amino acid frequency peptide features
            let relFreq = 
                let tmp = BioArray.toRelCompositionVector peptide
                [|
                    tmp.[getIndex AminoAcidSymbol.Ala];tmp.[getIndex AminoAcidSymbol.Cys];tmp.[getIndex AminoAcidSymbol.Asp];tmp.[getIndex AminoAcidSymbol.Glu];
                    tmp.[getIndex AminoAcidSymbol.Phe];tmp.[getIndex AminoAcidSymbol.Gly];tmp.[getIndex AminoAcidSymbol.His];tmp.[getIndex AminoAcidSymbol.Ile];
                    tmp.[getIndex AminoAcidSymbol.Lys];tmp.[getIndex AminoAcidSymbol.Leu];tmp.[getIndex AminoAcidSymbol.Met];tmp.[getIndex AminoAcidSymbol.Asn];
                    tmp.[getIndex AminoAcidSymbol.Pro];tmp.[getIndex AminoAcidSymbol.Gln];tmp.[getIndex AminoAcidSymbol.Arg];tmp.[getIndex AminoAcidSymbol.Ser];
                    tmp.[getIndex AminoAcidSymbol.Thr];tmp.[getIndex AminoAcidSymbol.Val];tmp.[getIndex AminoAcidSymbol.Trp];tmp.[getIndex AminoAcidSymbol.Tyr];        
                |]

            let physicochemical = getPhysicochemicalProperties peptide
            let pf = 
                peptideFeatures
                |> Array.map (fun f -> peptide |> Array.averageBy f)


            let digest = 
                if digestionEfficiencyMap.ContainsKey (protId,peptide) then
                    let a,b,c = digestionEfficiencyMap.[(protId,peptide)]
                    Some [|a;b;c|]
                else
                    //printfn "%s - %A" protId peptide
                    // [|0.;0.;0.|]
                    None
            match digest with
            | Some v -> let inp = createPredictionInput (Array.concat [|relFreq;physicochemical;pf;v|]) protId (peptide')
                        Some inp 
            | None -> None
        
        ///normalizes feature vector given a normalization vector containing tuples of (stDev,mean) of all features across a dataset
        let zNormalizePeptideFeaturesBy (norm: (float*float) []) (features:PredictionInput)  =
            if norm.Length = features.Data.Length then
                {features with 
                    Data = 
                        (Array.map2 
                            (fun d (stDev,mean) -> if nan.Equals((d-mean)/stDev) then 0. else (d-mean)/stDev) ) 
                            features.Data 
                            norm
                }
            else
                failwithf "feature vector and normalization vector have different length"

        let private chlamyNorm = 
            [|
                (0.1294759775, 0.1477564866); (0.03232439131, 0.0120497691);
                (0.06583974433, 0.05254771891); (0.08037249763, 0.06582908406);
                (0.04923115097, 0.02856099006); (0.09625444791, 0.08862113627);
                (0.0409499754, 0.01903257066); (0.0524744837, 0.03142502873);
                (0.04812202378, 0.03354896847); (0.08688410957, 0.09624922596);
                (0.04372886103, 0.02224862021); (0.04796601885, 0.02697248923);
                (0.07505402555, 0.05901261068); (0.06734344394, 0.04329616126);
                (0.05108587462, 0.05081075304); (0.07635354655, 0.06585390926);
                (0.06308348418, 0.04958665822); (0.07538097997, 0.07221419823);
                (0.03104479598, 0.01143679638); (0.04449184194, 0.02294694945);
                (18.08936628, 17.66412604); (1722.917992, 1829.439902);
                (3.039747471, 4.021484475); (0.9115152624, 1.442723413);
                (2.740599321, 2.578761062); (0.2259591883, -0.07734019599);
                (0.1348471884, 0.251175164); (0.1460896078, 0.5460883068);
                (0.114005905, 0.153373458); (0.9449223245, 17.50726637);
                (1.869898332, 6.934009825); (0.08655133387, 0.06496131673);
                (0.2762850846, 0.2276330281); (0.1053224169, 0.9909929427);
                (0.2372540707, 6.112844572); (1.124168016, 6.598078652);
                (0.1087448998, 0.01658923627); (0.2635107797, -0.1762355472);
                (0.1007580986, 0.930990863); (0.1207067007, 1.148202475);
                (0.6813763089, 6.154813386); (0.2165025154, 0.8372242502);
                (0.5278203242, 0.2050764499); (0.0, 0.0); (0.6044132199, 0.2977169821)
            |]

        let private yeastNorm = 
            [|
                (0.07205449942, 0.05870317244); (0.03292579913, 0.01172451697);
                (0.0738234841, 0.06475416441); (0.08378288208, 0.07431514303);
                (0.06013549634, 0.04385188562); (0.06678701753, 0.05148753581);
                (0.04301839169, 0.02170466904); (0.07128574467, 0.06705647111);
                (0.05220592539, 0.05707495112); (0.08706581448, 0.09882663785);
                (0.0413525213, 0.02031959783); (0.07346084846, 0.06278570344);
                (0.06218915668, 0.04630561542); (0.06262204346, 0.04122256361);
                (0.04660452507, 0.03070757211); (0.08959641681, 0.08842291913);
                (0.06891261589, 0.05839174474); (0.06871686103, 0.05922046758);
                (0.02926166995, 0.009963582353); (0.05267493232, 0.03316121013);
                (10.36244966, 15.40208044); (1114.854432, 1712.257911);
                (2.860379011, 4.239515625); (0.7426156118, 1.409460591);
                (2.640116869, 2.830055033); (0.2325694909, -0.07824047436);
                (0.1423364521, 0.3273767806); (0.1441495189, 0.4457713024);
                (0.1219682278, 0.183954965); (0.8638290629, 17.83917358);
                (1.97944068, 6.432096404); (0.07668969239, 0.005624749244);
                (0.2632108286, 0.09569136325); (0.0912584431, 1.000864484);
                (0.2694954289, 6.070490988); (1.027388398, 5.972532616);
                (0.08758176723, 0.01262545237); (0.2132952054, -0.1766576158);
                (0.07707357534, 0.9373354032); (0.1087780497, 1.173638978);
                (0.567622227, 5.768608549); (0.229437042, 0.8949232498);
                (0.4736182826, 0.1529837898); (0.0, 0.0); (0.5517373002, 0.2606125317)
            |]

        ///returns the z normalized version of the input feature vector by using the normalization vector dppop uses for its plant model
        let zNormalizePlantFeatureVector (features:PredictionInput) =
            zNormalizePeptideFeaturesBy chlamyNorm features

        ///returns the z normalized version of the input feature vector by using the normalization vector dppop uses for its non-plant model
        let zNormalizeNonPlantFeatureVector (features:PredictionInput) =
            zNormalizePeptideFeaturesBy yeastNorm features

    ///Contains functions to execute peptide observability prediction using the deep neural network dppop.
    module Prediction =
        open System.IO

        ///Options concerning the model used for prediction.
        type Model =
            ///dppops original plant model used in the web API.
            | Plant
            ///dppops original non-plant model used in the web API.
            | NonPlant
            ///Option to use a custom model at the given path
            | Custom of string

            ///returns a byte array from the ressource stream. Either reads the original models from the manifest resources or reads the custom model from the given path
            static member getModelBuffer =
                let assembly = Assembly.GetExecutingAssembly()
                let resnames = assembly.GetManifestResourceNames();
                function
                | Plant         ->  match Array.tryFind (fun (r:string) -> r.Contains("Chlamy5Times128.model")) resnames with
                                    | Some path -> 
                                        use stream = assembly.GetManifestResourceStream(path)
                                        let length = int stream.Length
                                        use bReader = new BinaryReader(stream)
                                        bReader.ReadBytes(length)

                                    | _ -> failwithf "could not plant load model from embedded ressources, check package integrity"

                | NonPlant      ->  match Array.tryFind (fun (r:string) -> r.Contains("Yeast5Times128.model.model")) resnames with
                                    | Some path ->                                         
                                        use stream = assembly.GetManifestResourceStream(path)
                                        let length = int stream.Length
                                        use bReader = new BinaryReader(stream)
                                        bReader.ReadBytes(length)
                                    | _ -> failwithf "could not load non-plant model from embedded ressources, check package integrity"
                | Custom path   ->  use stream = new FileStream(path,FileMode.Open)
                                    let length = int stream.Length
                                    use bReader = new BinaryReader(stream)
                                    bReader.ReadBytes(length)

        /// For expert use.
        /// Returns the observability prediction for the input peptides.
        /// Loads a trained CNTK model (either dppops plant/nonPlant models or a custom model) and evaluates the scores for the given collection of features (PredictionInput)
        /// No feature normalization or determination of distinct peptides for the organism is done.
        let scoreBy (model:Model) (data:PredictionInput []) = 
            let device = DeviceDescriptor.CPUDevice

            let PeptidePredictor : Function = 
                Function.Load(Model.getModelBuffer model,device)

            ///////////Input 
            let inputVar: Variable = PeptidePredictor.Arguments.Item 0

            let inputShape = inputVar.Shape
            /// Gets Size of one Feature Vector
            let featureVectorLength = inputShape.[0] 

            /// Extracts all Features and appends them, stores Values in a List
            let featureData = 
                let tmp = new System.Collections.Generic.List<float32>()
                data |> Array.iter(fun x -> 
                                    let data' = x.Data |> Array.map (fun x -> float32 (x))
                                    tmp.AddRange(data')
                                   )
                tmp

            /// Creates an input Batch
            let inputValues = Value.CreateBatch(inputShape,featureData,device)

            let inputMap = new Dictionary<Variable,Value>()
            inputMap.Add(inputVar,inputValues)

            ///////////Output
            let outputVar : Variable = PeptidePredictor.Output

            let outputMap = new Dictionary<Variable,Value>()
            outputMap.Add(outputVar,null)

            PeptidePredictor.Evaluate(inputMap,outputMap,device)

            let outputValues = outputMap.[outputVar]

            let preds = 
                outputValues.GetDenseData<float32>(outputVar)
                |> Seq.concat
                |> Array.ofSeq

            let res = 
                Array.map2 (fun (data:PredictionInput) preds -> createPredictionOutput data.ProtId data.Sequence (float preds)) data preds
            res

        ///Returns relative observability scores for uniquely mapping peptides of proteins of interest given a model, normalization procedure for features, and the proteome of the organism.
        let scoreProteinsAgainstProteome (model:Model) (featureNormalization: PredictionInput -> PredictionInput) (proteome: seq<FastA.FastaItem<BioArray<AminoAcids.AminoAcid>>>) (proteinsOfInterest: seq<FastA.FastaItem<BioArray<AminoAcids.AminoAcid>>>)  =
            printfn "Determining distinct peptides..."
            //only uniquely mapping peptides in the given proteome will be considered candidate peptides.
            let distinctPeptides = Classification.getDistinctTrypticPeptidesFromFasta proteome
            let digestionEfficiencyMap = Classification.createDigestionEfficiencyMapFromFasta proteinsOfInterest
            printfn "determining peptide features and predicting observability..."
            proteinsOfInterest
            |> Seq.map (fun protein ->
                                        let protId = protein.Header
                                        //uniquely mapping digested peptides
                                        let digested =
                                            Classification.digestTryptic protein.Sequence
                                            |> Seq.map (fun x -> BioArray.toString x)
                                            |> Seq.filter (fun peptide -> distinctPeptides.Contains peptide)
                                            |> List.ofSeq
                                        let candidatePeptides = 
                                            digested
                                            |> Seq.filter (fun p -> distinctPeptides.Contains(p))
                                            |> Seq.map (fun p -> Classification.getPeptideFeatures digestionEfficiencyMap protId (BioArray.ofAminoAcidSymbolString p))
                                            |> Array.ofSeq
                                            |> Array.choose id
                                        candidatePeptides
                                        |> Array.map (featureNormalization)
                                        |> scoreBy model
                                        |> Array.sortByDescending (fun (x) -> x.PredictionScore)
                                        |> fun x -> let max = (Array.maxBy (fun (x) -> x.PredictionScore) x).PredictionScore 
                                                    x |> Array.map (fun (x) -> if x.PredictionScore >= 0. then {x with PredictionScore = (x.PredictionScore/max)} else {x with PredictionScore = 0.0})
                )

        ///Returns relative observability scores for uniquely mapping peptides of proteins of interest using dppops plant model and feature normalization procedure, given the proteome of the organism.
        let scoreDppopPlant (proteome: seq<FastA.FastaItem<BioArray<AminoAcids.AminoAcid>>>) (proteinsOfInterest: seq<FastA.FastaItem<BioArray<AminoAcids.AminoAcid>>>) =
            scoreProteinsAgainstProteome Model.Plant Classification.zNormalizePlantFeatureVector proteome proteinsOfInterest

        ///Returns relative observability scores for uniquely mapping peptides of proteins of interest using dppops non-plant model and feature normalization procedure, given the proteome of the organism.
        let scoreDppopNonPlant (proteome: seq<FastA.FastaItem<BioArray<AminoAcids.AminoAcid>>>) (proteinsOfInterest: seq<FastA.FastaItem<BioArray<AminoAcids.AminoAcid>>>) =
            scoreProteinsAgainstProteome Model.NonPlant Classification.zNormalizeNonPlantFeatureVector proteome proteinsOfInterest