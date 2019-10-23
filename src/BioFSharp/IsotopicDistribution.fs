namespace BioFSharp

///Contains functionality for working with isotopic abundancies of molecules
module IsotopicDistribution =
    open BioFSharp.Formula

    module BRAIN = 

        // Implementation according to BRAIN - algortihm
        /// Generates the distribution of the isotopic abundancy
        /// The predicted distribution is only valid for Elements with isotopes at natural abundance
        let ofFormula (limit:int) (fml:Formula) =
            let calcP0 (fml:Formula) = 
                fml 
                |> Map.fold 
                    (fun s k v -> 
                        let stdIso = Elements.getMainIsotope k
                        s * stdIso.NatAbundance**float(v) ) 1.       
        
            // Calculate coefficient ql (Equ. 12 + 7)
            let calcPhiL (fml:Formula) (l:int) = 
                let lf = float l 
                fml
                |> Map.fold 
                    (fun s k v -> 
                        s + (Elements.getSinglePhiL k (float v) lf ) ) 0.


            let rec calcPJ (ps:List<float>) (phis:List<float>) (state:float)=
                match ps,phis with
                | [],[] -> state
                | pj::ps,phi::phis ->  calcPJ ps phis (state + pj * phi)
                | _ ,[] -> raise ( System.Exception("ps can't be longer then phis see function w") )
                | [],_  -> state
            

            let rec w (ps:List<float>) (phis:List<float>) (i:int) =
                if ps.Length < phis.Length then
                    let np = calcPJ ps phis 0.
                    w ((np / - (float i))::ps) phis (i + 1)
                else
                    ps |> List.rev // TODO : Continuation passing

            let allPhis = [ for i=1 to limit do yield (calcPhiL fml i) ]
            let p0 = calcP0 fml
            let ps = w [p0] allPhis 1
            ps   

    module MIDA =

        open System

        /// Computes the log gamma function using the Lanczos Coefficients described in Numerical Recipes (Press et al)
        let private gammaLn z = 
            let lanczosCoefficients = [76.18009172947146;-86.50532032941677;24.01409824083091;-1.231739572450155;0.1208650973866179e-2;-0.5395239384953e-5]
            let rec sumCoefficients acc i coefficients =
                match coefficients with
                | []   -> acc
                | h::t -> sumCoefficients (acc + (h/i)) (i+1.0) t
            let gamma = 5.0
            let x = z - 1.0
            let tmp = x + gamma + 0.5 
            -(tmp - ((x + 0.5) * log(tmp))) + log(Math.Sqrt( 2.0 * Math.PI ) * sumCoefficients 1.000000000190015 (x + 1.0) lanczosCoefficients)

        // The size of the table of factorials in logarithm. The value FactorialLn_NTop D 2 should be increased if your integer arguments are often larger
        let private FactorialLn_NTop = 2000

        let private FactorialLn_cache =
            let cache = Array.zeroCreate (FactorialLn_NTop + 1)
            for i=0 to FactorialLn_NTop do
                cache.[i] <-gammaLn ((float i) + 1.0)
            cache

        /// Computes the natural logarithm of the factorial function.
        let private factLN (x: int) : float =
            if x < 0 then failwith "Log factorial not defined for n < 0"
            //if x <= 1 then 0.0 else Gamma.gammaLn ((float x) + 1.0)
            if x <= FactorialLn_NTop then 
                FactorialLn_cache.[x]
            else 
                gammaLn ((float x) + 1.0)

        type Polynomial = {
            Power       : float 
            Probability : float 
            }

        ///
        let private createPolynomial power prob = 
            {Power = power ; Probability = prob}

        let private powerOfElem mwResolution mw = 
            floor(mw / mwResolution + 0.5)
  
        let private computeMeanElemOccurence (nInFormula:float) (elemXcomp:float) =
            (nInFormula * elemXcomp) |> int

        let private computeVariance (nc:float) (ncAdd:float) (nInFormula:float) (elemXcomp:float) = 
            ncAdd + nc * sqrt(nInFormula * elemXcomp * (1. - elemXcomp))
            |> ceil
            |> int
 
        type private IsotopeInFormula<'a,'b> = {
            Isotope: Isotopes.Isotope
            Probability: float
            Tag1: 'a
            Tag2: 'b
            }

        let private addChargeState (chargeState: int) (formula: Formula.Formula) =
            if chargeState < 0 then
                failwith "only positive charge states can be considered"
            let hydrogenAtoms = 
                "H" + chargeState.ToString()
                |> Formula.parseFormulaString
            Formula.add formula hydrogenAtoms

        let private setResolution resolution (*(monoisotopicMass:float) *)= 
            let mergeResolution = 
                if resolution >= 1. then failwith "Mass resolution > 1 Da is not supported, please choose a value smaller than 1 Da"
                elif resolution <= 1e-4 (*&& monoisotopicMass < 1e5*) then 1e-4 
                elif resolution <= 1e-3 (*&& monoisotopicMass < 1e6*) then 1e-3 
                elif resolution <= 1e-2 (*&& monoisotopicMass < 2e6*) then 1e-2
                else resolution
            let resolution' = 
                mergeResolution /// 2.
            mergeResolution, resolution'

        let private mwResolution = (1.*exp(-12.))

        let private multiplyFineGrainedPolynomial resolution minProb charge (fml:BioFSharp.Formula.Formula) = 
            let nc = 10.
            let ncAddValue = 1.0
            let nAtoms = 200
            let maxPolynomialSize = log(1e13)
            let fml' = addChargeState charge fml
            let rec loop nInFormula (v:(Isotopes.Isotope*float*int) list) (acc:(Isotopes.Isotope*float*int) list list) (ll:(IsotopeInFormula<int,int>)list) =
                match ll with 
                | h::[] ->
                    let sum = v |> List.fold (fun sum (iso,prob,count) -> count+sum) 0
                    let innerCount = nInFormula-sum
                    let v' = ((h.Isotope,h.Probability,innerCount)::v)
                    match v' |> List.tryFind (fun (iso,prob,innerCount) -> innerCount < 0) with
                    | Some x -> acc
                    | None   -> v'::acc
                | h::t ->
                    let temp = [h.Tag2 .. h.Tag1] 
                    [
                    for i in temp do 
                            yield! loop nInFormula ((h.Isotope,h.Probability,i)::v) acc t
                    ]
    
            let polynomialExpansion nInFormula isotopesWithProperties = 
                loop (int nInFormula) [] [] isotopesWithProperties
                |> List.fold (fun acc isos -> 
                                let prob = 
                                    isos 
                                    |> List.fold (fun sumProb (isos,prob,count) -> 
                                                                    if count >= 0 then 
                                                                        sumProb - (factLN count) + (float count * (log prob))
                                                                    else 0.
                                                    ) (factLN (int nInFormula))
                                    |> exp                                
                                if prob >= minProb then 
                                    let power = 
                                        isos 
                                        |> List.fold (fun sumPower (isos,prob,count) -> sumPower + (float count) * (powerOfElem mwResolution isos.Mass)) 0.
                                    (createPolynomial power prob)::acc 
                                else 
                                    acc
                            ) []       
            ///
            let numberOfElements = 
                fml'
                |> Map.fold (fun acc iso xComp -> 
                                match iso with 
                                | Elements.Element.Mono     _    -> 1.+acc
                                | Elements.Element.Di       _    -> 2.+acc
                                | Elements.Element.Tri      _    -> 3.+acc
                                | Elements.Element.Multi    x -> 
                                  match Array.length x.XL with 
                                  | nI when nI <= 10 -> acc + 2. + (Array.length x.XL |> float) 
                                  | _              -> failwith "Isotopic Distribution can not be calculated for elements with more than 10 isotopes."
                            ) 0.
            ///
            let f_Polynomial = 
                fml'
                |> Map.fold (fun acc iso nInFormula -> 
                                match iso with 
                                | Elements.Element.Mono elem    -> 
                                    let meanOccX0 = computeMeanElemOccurence nInFormula elem.Xcomp
                                    //let variance = left out in original source code was not mentioned in the paper 
                                    let prob = 
                                        (factLN (int nInFormula)) - (factLN meanOccX0) + ((float meanOccX0) * log elem.Xcomp )
                                        |> exp 
                                    let power = 
                                        float meanOccX0 * (powerOfElem mwResolution elem.X.Mass)
                                    [(createPolynomial power prob)]::acc
                                | Elements.Element.Di elem     -> 
                                    let ncAdd =
                                        if (int nInFormula) < nAtoms then 
                                            10.
                                        else
                                            ncAddValue
                                    // compute means
                                    let meanOccX0 = computeMeanElemOccurence nInFormula elem.Xcomp
                                    // compute variances 
                                    let varX0 = computeVariance ncAdd nc nInFormula elem.Xcomp
                                    let varX1 = computeVariance ncAdd nc nInFormula elem.X1comp
                                    let meanOccX1 = (computeMeanElemOccurence nInFormula elem.X1comp) + varX1

                                    let nPolTerms = log((meanOccX0 + varX0) |> float) + log((meanOccX1 + varX1) |> float) + log(2.**2.)
                                    if nPolTerms > maxPolynomialSize then failwith "number of Polynomial terms is to large for current implementation, sorry" // implement Fourier transformation 
                                    else
                                    let isotopesWithProperties =
                                        [(elem.X,elem.Xcomp,meanOccX0,varX0);(elem.X1,elem.X1comp,meanOccX1,varX1);]
                                        |> List.map (fun (elemX,elemXComp,mean,var) -> 
                                                        let lowerBorder = 
                                                            let tmp = mean-var-1
                                                            if tmp >= 0 then tmp else 0
                                                        {Isotope=elemX;Probability=elemXComp;Tag1=(mean+var);Tag2=lowerBorder} 
                                                    ) 
                                    polynomialExpansion nInFormula isotopesWithProperties
                                    :: acc
                                | Elements.Element.Tri elem     -> 
                                    let ncAdd =
                                        if (int nInFormula) < nAtoms then 
                                            10.
                                        else
                                            ncAddValue
                                    // compute means
                                    let meanOccX0 = computeMeanElemOccurence nInFormula elem.Xcomp
                                    let meanOccX1 = computeMeanElemOccurence nInFormula elem.X1comp
                                    // compute variances 
                                    let varX0 = computeVariance ncAdd nc nInFormula elem.Xcomp
                                    let varX1 = computeVariance ncAdd nc nInFormula elem.X1comp
                                    let varX2 = computeVariance ncAdd nc nInFormula elem.X2comp
                                    let meanOccX2 = computeMeanElemOccurence nInFormula elem.X2comp + varX2

                                    let nPolTerms = log((meanOccX0 + varX0) |> float) + log((meanOccX1 + varX1) |> float) + log((meanOccX2 + varX2) |> float)+ log(2.**3.)
                                    if nPolTerms > maxPolynomialSize then failwith "number of Polynomial terms is to large for current implementation, sorry" // implement Fourier transformation 
                                    else
                                    let isotopesWithProperties =
                                        [(elem.X,elem.Xcomp,meanOccX0,varX0);(elem.X1,elem.X1comp,meanOccX1,varX1);(elem.X2,elem.X2comp,meanOccX2,varX2)]
                                        |> List.map (fun (elemX,elemXComp,mean,var) -> 
                                                        let lowerBorder = 
                                                            let tmp = mean-var-1
                                                            if tmp >= 0 then tmp else 0
                                                        {Isotope=elemX;Probability=elemXComp;Tag1=(mean+var);Tag2=lowerBorder} 
                                                    )                           
                                    polynomialExpansion nInFormula isotopesWithProperties
                                    :: acc
                                | Elements.Element.Multi elem ->
                                    let ncAdd = 
                                        if (int nInFormula) < nAtoms then 
                                            10.
                                        else
                                            ncAddValue 
                                    // compute means
                                    let meanOccX0 = computeMeanElemOccurence nInFormula elem.Xcomp
                                    let meanOccX1 = computeMeanElemOccurence nInFormula elem.X1comp
                                    let meanOccX2 = computeMeanElemOccurence nInFormula elem.X2comp 
                                    // compute variances 
                                    let varX0 = computeVariance ncAdd nc nInFormula elem.Xcomp
                                    let varX1 = computeVariance ncAdd nc nInFormula elem.X1comp
                                    let varX2 = computeVariance ncAdd nc nInFormula elem.X2comp
                                    let meansAndVars =
                                        elem.XL
                                        |> Array.mapi (fun i iso -> 
                                                        // compute variances 
                                                        let var = computeVariance ncAdd nc nInFormula iso.NatAbundance
                                                        let meanOcc = computeMeanElemOccurence nInFormula iso.NatAbundance
                                                        if i = elem.XL.Length-1 then 
                                                            iso, iso.NatAbundance, meanOcc + var, var
                                                        else 
                                                            iso, iso.NatAbundance, meanOcc, var
                                                      ) 
                                    let nPolTerms =
                                        log((meanOccX0 + varX0) |> float) + log((meanOccX1 + varX1) |> float) + log((meanOccX2 + varX2) |> float) 
                                        |> (+) (meansAndVars |> Array.sumBy (fun (iso,natAb,mean,var) -> log((mean + var) |> float)))
                                        |> (+) (log(2.**(3. + float meansAndVars.Length)))
                                    if nPolTerms > maxPolynomialSize then failwith "number of Polynomial terms is to large for current implementation, sorry" // implement Fourier transformation 
                                    else    
                                    let isotopesWithProperties =
                                        let mainIsos = [(elem.X,elem.Xcomp,meanOccX0,varX0);(elem.X1,elem.X1comp,meanOccX1,varX1);(elem.X2,elem.X2comp,meanOccX2,varX2)]
                                        let additionalIsos = meansAndVars |> List.ofArray
                                        mainIsos@additionalIsos 
                                        |> List.map (fun (elemX,elemXComp,mean,var) -> 
                                                        let lowerBorder = 
                                                            let tmp = mean-var-1
                                                            if tmp >= 0 then tmp else 0
                                                        {Isotope=elemX;Probability=elemXComp;Tag1=(mean+var);Tag2=lowerBorder} 
                                                    )                           
                                    polynomialExpansion nInFormula isotopesWithProperties
                                    :: acc                                                                                
                           ) []
                |> List.map List.rev
                |> List.rev
            let emptyPolynomial = createPolynomial 0. 0. 
            let FGIDPolynomialSize = 
                2000. / resolution + 0.5 
                |> int
            let FGIDPolynomial = 
                ResizeArray<Polynomial>(FGIDPolynomialSize)
            for i = 0 to FGIDPolynomialSize do
                FGIDPolynomial.Add emptyPolynomial
        
            let multiplicationFinalOperation minProb mwResolution fineResolution (fgidPolynomial:ResizeArray<Polynomial>) (t_pol: Polynomial list) (fi_pol: Polynomial list) = 
                let deltaMass = fineResolution/mwResolution
                let minMass = t_pol.[0].Power + fi_pol.[0].Power
                //printfn "%f minmass" minMass
                let maxMass = (List.last t_pol).Power + (List.last fi_pol).Power
                //printfn "%f maxmass" maxMass
                let maxIndex = abs(maxMass-minMass) / deltaMass 
                t_pol
                |> List.iteri (fun i tPol ->
                                fi_pol 
                                |> List.iteri (fun j fPol -> 
                                                 let probTemp = tPol.Probability * fPol.Probability
                                                 if probTemp > minProb then
                                                    let powerTemp = tPol.Power + fPol.Power
                                                    let index = 
                                                        abs(powerTemp - minMass) / deltaMass + 0.5
                                                        |> int
                                                    //printfn "%i" index
                                                    let tempPol = 
                                                        let power = fgidPolynomial.[index].Power + powerTemp* (* warum wird dieser term multipliziert? als gewichtung?*)probTemp
                                                        let prob = fgidPolynomial.[index].Probability + probTemp
                                                        createPolynomial power prob 
                                                    fgidPolynomial.[index] <- tempPol    
                                              )
                             )
                let index = t_pol.Length   
                //printfn "%i index" index
                let rec updateT_pol i j acc t_pol = 
                    if i = fgidPolynomial.Count then
                        if j < index then 
                            acc 
                            |> List.rev (*|> List.take (j)*)
                        else 
                            acc |> List.rev
                    else
                        if fgidPolynomial.[i].Probability <> 0. then
                            match t_pol with 
                            | []      -> 
                                let tempPol = createPolynomial (fgidPolynomial.[i].Power/fgidPolynomial.[i].Probability) fgidPolynomial.[i].Probability  
                                fgidPolynomial.[i] <- emptyPolynomial
                                updateT_pol (i+1) j (tempPol::acc) [] 
                    
                            | h::tail ->
                                let tempPol = createPolynomial (fgidPolynomial.[i].Power/fgidPolynomial.[i].Probability) fgidPolynomial.[i].Probability  
                                fgidPolynomial.[i] <- emptyPolynomial
                                updateT_pol (i+1) (j+1) (tempPol::acc) tail  
                        else 
                            fgidPolynomial.[i] <- emptyPolynomial
                            updateT_pol (i+1) (j) acc t_pol  
                let t_pol' = updateT_pol 0 0 [] t_pol
                t_pol'                           
    
            match f_Polynomial with 
            | f1 :: [] -> 
                f1 
            | f1 :: fn -> 
                //multiply operation 
                fn
                |> List.fold (fun t_pol fi_pol -> multiplicationFinalOperation minProb mwResolution resolution FGIDPolynomial t_pol fi_pol ) f1 
            //f_Polynomial

        ///   
        let private mergeFinePolynomial mergeResolution (t_pol:Polynomial list) = 
            let sortedFPol = 
                t_pol |> List.sortBy (fun x -> x.Power)    
            ///used to collect those masses which are closest first     
            let rec merge resFactor acc (t_pol:Polynomial list) = 
                match t_pol with 
                | h::[] ->  
                    h::acc
                    |> List.rev 
                | h1::h2::tail ->
                    if resFactor < 8. then 
                       let mwDistance = abs((h1.Power * mwResolution) - (h2.Power * mwResolution)) 
                       //printfn "mwd1 = %f" mwDistance
                       if mwDistance <= (resFactor * mergeResolution / 8.) then
                            //printfn "merge!"
                            let mergedPol = 
                                let pow = (h1.Power*h1.Probability) + (h2.Power*h2.Probability) 
                                let prob = h1.Probability + h2.Probability
                                createPolynomial (pow/prob) prob
                            merge resFactor (acc) (mergedPol::tail)
                       else 
                            merge resFactor (h1::acc) (h2::tail)
                    else
                        let mwDistance = abs((h1.Power * mwResolution) - (h2.Power * mwResolution)) 
                        //printfn "mwd2 = %f" mwDistance
                        if mwDistance <= (mergeResolution * mergeResolution / 100.) then
                            //printfn "merge2!"
                            let mergedPol = 
                                let pow = h1.Power*h1.Probability + h2.Power*h2.Probability 
                                let prob = h1.Probability + h2.Probability
                                createPolynomial (pow/prob) prob       
                            merge resFactor (acc) (mergedPol::tail)
                        else 
                            merge resFactor (h1::acc) (h2::tail)
                | []   -> []
            [1. .. 9.]
            |> List.fold (fun acc resF -> merge resF [] acc) (sortedFPol)
    

        /// normalizes istopic distribution probabilities to sum up to 1.
        let normalizeByProbSum minProb (pols: Polynomial list) = 
            let sum = 
                pols
                |> List.filter (fun x -> x.Probability > minProb)
                |> List.sumBy (fun x -> x.Probability)
            pols 
            |> List.map (fun x -> {x with Probability = x.Probability/sum})

        /// normalizes istopic distribution probabilities to sum up to 1.
        let normalizeByMaxProb minProb (pols: Polynomial list) = 
            let max = 
                pols
                |> List.maxBy (fun x -> x.Probability)
                |> fun x -> x.Probability
            pols 
            |> List.filter (fun x -> x.Probability > minProb)
            |> List.map (fun x -> {x with Probability = x.Probability/max})


        // Could be done together with normalization but at cost of modularity
        // can be done on arrays to safe computation time
        /// converts the field "Power" of the record type Polynomial to a molecular weight
        let private powerToMw charge (pols: Polynomial list) = 
            if charge = 0 then
                pols 
                |> List.map (fun x -> x.Power * mwResolution,x.Probability )
            else 
                pols 
                |> List.map (fun x -> x.Power * mwResolution / (charge |> float |> abs),x.Probability )
    
    
        /// Generates the distribution of the isotopic abundancy using MIDAs polynomial based algorithm (MIDAs_a)
        //(doi: 10.1007/s13361-013-0733-7)
        // tested ín comparison to: https://www.ncbi.nlm.nih.gov/CBBresearch/Yu/midas/index.html
        let ofFormula normF resolution minProb charge (fml:Formula) =
            let mergeResolution,resolution' = 
                setResolution resolution
            multiplyFineGrainedPolynomial resolution' minProb charge fml
            |> mergeFinePolynomial (mergeResolution)
            |> (normF minProb)
            |> powerToMw charge

