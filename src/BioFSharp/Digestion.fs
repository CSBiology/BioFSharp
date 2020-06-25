namespace BioFSharp

///Contains types and functions needed to digest amino acid sequences with proteases
module Digestion =
    
    open System
    open AminoAcids
    open FSharpAux

    /// p4 p3 p2 p1 || p1' p2'
    type Protease = {
        ///
        Name : string
        ///
        Expression : AminoAcid option -> AminoAcid option
                  -> AminoAcid option -> AminoAcid option
                  -> AminoAcid option-> AminoAcid option -> bool    
        }

    ///Creates a Protease from given name and motifFunction f
    let createProtease name f =
         {Name = name; Expression = f}

    /// Digested peptide
    type DigestedPeptide<'a> = {
        ProteinID: 'a
        MissCleavages: int
        CleavageStart:int
        CleavageEnd: int
        PepSequence: AminoAcid list
        }

    ///Creates digested peptide from given information
    let createDigestedPeptide proteinID missCleavages cleavageStart cleavageEnd pepSequence = {
         ProteinID=proteinID
         MissCleavages=missCleavages
         CleavageStart=cleavageStart
         CleavageEnd=cleavageEnd
         PepSequence=pepSequence
         }
   
    ///Returns true, if AminoAcid array resembles cutting site of given protease, else returns false
    let isCuttingSite (protease:Protease) (arr:AminoAcid option[]) =
        match arr with
        | [|p4; p3; p2; p1; p1'; p2';|] -> protease.Expression p4 p3 p2 p1 p1' p2'
        | _ -> false

    ///Returns true, if AminoAcid array resembles cutting site of given protease, else returns false
    [<Obsolete("Use isCuttingSite instead")>]
    let isCutingSite (protease:Protease) (arr:AminoAcid option[]) =
        isCuttingSite protease arr

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
            |> groupAfter (fun (c,arr) -> isCuttingSite protease arr)       


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
        let digest (protease: Protease) proteinID (aas: AminoAcid []) =
            let aasLength = aas.Length
            if aasLength = 0 then [||]
            else
            let rec groupAfter f acc lowercounter counter (aasWithOption: (AminoAcid*'a []) []) =
                if counter = aasLength-1 then (createDigestedPeptide proteinID 0 (lowercounter) (counter) (aas.[lowercounter.. counter]|> Array.toList))::acc |> List.rev 
                else 
                    match (f aasWithOption.[counter]) with
                    | true  -> groupAfter f ((createDigestedPeptide proteinID 0 (lowercounter) (counter) (aas.[lowercounter.. counter]|> Array.toList))::acc) (counter+1) (counter+1) aasWithOption 
                    | false -> groupAfter f acc lowercounter (counter+1) aasWithOption
            aas 
            |> motivy 3 2 
            |> (groupAfter (fun (c,arr) -> isCuttingSite protease arr) [] 0 0) 
            |> List.toArray



        /// Takes Array of DigestedPeptides and and returns Array of DigestedPeptides including those resulting of one or more Misscleavage events
        let concernMissCleavages (minMissCleavages:int) (maxMisscleavages:int) (digestedPeptidesA:(DigestedPeptide<'a>) []) =
            if digestedPeptidesA = [||] then [||]
            else
            let lengthOfPeptideL = digestedPeptidesA.Length
            let minToMaxMissCleavagesL = [minMissCleavages.. maxMisscleavages]
            let rec connectDigestedPeptides acc (digestedPeptidesA: DigestedPeptide<'a> []) (fstPepIdx:int)  (lastPepIdx:int) currentMissCleavages =
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
                        createDigestedPeptide digestedPeptidesA.[0].ProteinID (currentMissCleavages) digestedPeptidesA.[fstPepIdx].CleavageStart 
                            digestedPeptidesA.[lastPepIdx].CleavageEnd currentPeptideSeq
                    
                    connectDigestedPeptides (currentPeptide::acc) digestedPeptidesA (fstPepIdx+1) (lastPepIdx+1) currentMissCleavages
        
            minToMaxMissCleavagesL
            |> List.collect (fun x ->  (connectDigestedPeptides [] digestedPeptidesA 0 x x)) 
            |> Array.ofList

    ///Contains frequently needed proteases
    //TODO: switch to better list system
    module Table = 

        let Trypsin =
            createProtease "Trypsin" (let _p1 = [AminoAcid.Lys;AminoAcid.Arg] |> Set.ofList 
                                      fun p4 p3 p2 p1 p1' p2' -> 
                                      match p1,p1' with
                                      | Some a1,Some a1' -> _p1.Contains(a1) && not (a1' = AminoAcid.Pro)
                                      | _   -> false                     
                                      )       
                 
        let Lys_C =             
            createProtease "Lys-C" (let _p1 = [AminoAcid.Lys] |> Set.ofList
                                    fun p4 p3 p2 p1 p1' p2' -> 
                                    match p1 with
                                    | Some a1 -> _p1.Contains(a1)
                                    | _ -> false
                                    )    

        let getProteaseBy name = 
            match name with 
            | "Trypsin" -> Trypsin 
            | "Lys-C"   -> Lys_C
    
    
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

    //let rec searchFrw (str:string) (pos:int) (counter:int) (result:array<char>)=    
    //    if pos < counter then        
    //        result.[pos] <- str.Chars(pos)
    //        searchFrw str (pos+1) counter result
    //    //elif pos > counter then
    //    elif pos = counter then
    //        result.[pos] <- '-'
    //        searchFrw str (pos+1) counter result
    //    else
    //        result





    //let rec searchRev (str:string) (pos:int) (counter:int) (result:array<char>)=    
    //    if pos < counter then        
    //        result.[8-pos] <- str.Chars(str.Length-pos-1)
    //        searchRev str (pos+1) counter result
    //    //elif (str.Length-pos) < 0 then
    //    elif (str.Length-pos) = 0 then
    //        result.[8-pos] <- '-'
    //        searchRev str (pos+1) counter result
    //    else
    //        result


    //let calculateCleavageScore (peptideSequence:string) =
    //    let tmp:array<char> = Array.create 9 ' '
    //    let input = tmp |> searchFrw peptideSequence 0 4 |> searchRev peptideSequence 0 5 
    //    let calcCleavedP  =
    //        input
    //        |> Array.mapi (fun i c -> getCleavedP c i )
    //        |> Array.sum
    //    let calcMissedP =
    //        input
    //        |> Array.mapi (fun i c -> getMissedP c i )
    //        |> Array.sum
    //    calcMissedP - calcCleavedP



