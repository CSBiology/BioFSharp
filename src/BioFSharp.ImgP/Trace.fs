namespace BioFSharp.ImgP


open System
open BioFSharp.ImgP
open FSharpAux

module Ricker =
    type myRicker = {
        ScaleDecay  :   float
        ScaleRise   :   float
        Values      :   float []
        PadArea     :   int        
                    }


    let createmyRicker scaleRise scaleDecay =  
        let rickerMHwithScale scale x = (2./(sqrt(3.*scale)*(Math.PI**0.25)))*(1.-((x**2.)/(scale**2.)))*(Math.E**(-(x**2.)/(2.*(scale**2.))))
        let padArea = ceil (7. * scaleDecay)
        let padAreaInt = padArea |> int
        
        //correctionVal is a value that corrects admissibility. That is due to the modified ricker where the left part of the ricker at x=0 <> right(x=0) resulting in a area under the curve <> 0.
        let correctionVal = (sqrt scaleRise) * scaleDecay**(- 0.5)
        let values = 
            Array.append 
                (Array.map (fun x -> correctionVal * ( rickerMHwithScale scaleRise x)) [|-(padArea)..(- 1.)|]) 
                (Array.map (fun x ->                   rickerMHwithScale scaleDecay x) [|0.0..(padArea)|])

        let amplitudeAdjValues =
            let sum = values |> Array.fold (fun acc x -> acc + Math.Abs(x)) 0.
            let max = values |> Array.max
            let corrFacTwo = 
                0.15 / max
            values |> Array.map (fun x -> corrFacTwo * x)      

        {
        ScaleDecay  = scaleDecay
        ScaleRise   = scaleRise
        //Values      = amplitudeAdjValues
        Values      = values
        PadArea     = padAreaInt
        }

    let myRicker1__1   = createmyRicker 1. 1.  
    let myRicker1__1_25= createmyRicker 1. 1.25
    let myRicker1__1_5 = createmyRicker 1. 1.5 
    let myRicker1__1_75= createmyRicker 1. 1.75
    let myRicker1__2   = createmyRicker 1. 2.  
    let myRicker1__2_5 = createmyRicker 1. 2.5 
    let myRicker1__3   = createmyRicker 1. 3.  
    let myRicker1__3_5 = createmyRicker 1. 3.5 
    let myRicker1__4   = createmyRicker 1. 4.  
    let myRicker1__4_5 = createmyRicker 1. 4.5 
    let myRicker1__5   = createmyRicker 1. 5.  
    let myRicker1__5_5 = createmyRicker 1. 5.5 
    let myRicker1__6   = createmyRicker 1. 6.  
    let myRicker1__6_5 = createmyRicker 1. 6.5  
    let myRicker1__7   = createmyRicker 1. 7.  
    let myRicker1__8   = createmyRicker 1. 8.  
    let myRicker10__20 = createmyRicker 10. 20.
    let myRicker10__40 = createmyRicker 10. 40.
    let myRicker10__50 = createmyRicker 10. 50.
    let myRicker10__60 = createmyRicker 10. 60.
    let myRicker10__70 = createmyRicker 10. 70.
    let myRicker10__80 = createmyRicker 10. 80.

    let myricker2 scR scD= 
        let funct sc x = 
            let xS = x / sc
            - (1./sc) * ( (1. / Math.Sqrt(2. * Math.PI))) * Math.Exp(-(xS**2.)/2.)  * (xS ** 2. - 1.)
        let corrf = scR/scD
        let values = Array.append (Array.map (fun x -> (corrf * funct scR x)) [|-2000.0..(-1.0)|]) (Array.map (funct scD) [|0.0..2000.0|]  )
        {
        ScaleDecay  =   scD
        ScaleRise   =   scR
        Values      =   values
        PadArea     =   2000      
                    }


            
module Trace =

    [<Obsolete("Do not use anymore. Switch to getCorrelationAtCoor instead.")>]
    let inline cwtDataOfCoor (marr: Marr.MarrWavelet) y x (paddedData:seq<'a[,]>)=
        ///calculates the wavelet transformation at position 'x,y' with MarrWavelet 'marr' 
        let C3DWTatPositionAndFrame (marr: Marr.MarrWavelet) (paddedframe:'a[,]) y x = 
            let offset = marr.PadAreaRadius
            let offsetpadding = 40
            let mutable acc = []
            for a = 0 to 2*offset do
                for b = 0 to 2*offset do
                    acc <- ((marr.Values).[a,b] * ((paddedframe).[((x+offsetpadding)+(a-offset)),((y+offsetpadding)+(b-offset))] |> float) )::acc
            acc 
            |> List.sum 
            |> fun x -> x / (sqrt (Math.Abs marr.Scale)) //CONSTANT ENERGY times 1/(sqrt |scale|)
        let result =
            [|0..(Seq.length paddedData) - 1|]       
            |> Array.map (fun fr -> 
                C3DWTatPositionAndFrame marr (paddedData |> Seq.item fr) y x)
        result
    
    ///calculates the correlations value at a specified coordinate
    let inline getCorrelationAtCoor borderpadding (marr: Marr.MarrWavelet) y x (paddedData:seq<'a[,]>)=
        ///calculates the wavelet transformation at position 'x,y' with MarrWavelet 'marr' 
        let C3DWTatPositionAndFrame (paddedframe:'a[,]) y x = 
            let offset = marr.PadAreaRadius
            Array2D.init (2*offset) (2*offset) (fun a b -> 
                let waveletvalue = (marr.Values).[a,b] 
                let datavalue = float paddedframe.[((x+borderpadding)+(a-offset)),((y+borderpadding)+(b-offset))]
                waveletvalue * datavalue)
            |> Array2D.toLongColumnArray 
            |> Array.sum
            |> fun x -> x / (sqrt (Math.Abs marr.Scale))
           
        let result =
            Array.init (Seq.length paddedData) (fun fr -> C3DWTatPositionAndFrame (paddedData |> Seq.item fr) y x)
        result

    [<Obsolete("Do not use anymore. Switch to getTraceOfBestCorrelation instead.")>]
    let inline findTraceOfBestCoor marrList (paddedData:seq<'a[,]>) (coor:float*float) =
        let yCoor = (fst coor) |> Core.Operators.round |> int
        let xCoor = (snd coor) |> Core.Operators.round |> int 
        //sums up all wavelet transformed data. Highest score represents the best matching waveletfunction.
        let intensityList = 
            marrList  
            |> List.map (fun x -> 
                let corrcoeff = cwtDataOfCoor x yCoor xCoor paddedData
                corrcoeff)
        //searches for the index of the wavelet in marrList with the highest score (representing the best matching waveletfunction).
        let indexOfBestScale = 
            let foldedIntensities=
                intensityList 
                |> List.map (fun x -> 
                    let integral = x |> Array.fold (fun t acc -> t + acc) 0.
                    integral) 
            foldedIntensities
            |> List.findIndex (fun x -> x = (foldedIntensities |> List.max))
        
        //gives the wavelet transformed data of the best matching waveletfunction at this position
        let trace = intensityList.[indexOfBestScale]
        
        //gives the radius of the best matching waveletfunction at this position
        let radius = (marrList.[indexOfBestScale]).Zero 
        (radius,trace)

    ///calculates the correlation values of a specified coordinate with given wavelets and returns the trace of highest correlation
    let inline getTraceOfBestCorrelation borderpadding marrList (paddedData:seq<'a[,]>) (coor:float*float) =
        let yCoor = (fst coor) |> round 0 |> int
        let xCoor = (snd coor) |> round 0 |> int 
        //calculates the summed correlation values of each wavelet transformed trace
        let intensityList = 
            marrList  
            |> List.map (fun x -> 
                getCorrelationAtCoor borderpadding x yCoor xCoor paddedData
                |> Array.sum)
        //gives the index of the best matching waveletfunction based on summed correlation values for each wavelet
        let indexOfBestScale = 
            intensityList 
            |> List.indexed
            |> List.maxBy snd
            |> fst
        //trace of highest correlation (best matching waveletfunction at this position)
        let trace = intensityList.[indexOfBestScale]
        //radius of the best matching waveletfunction at this position
        let radius = (marrList.[indexOfBestScale]).Zero 
        (radius,trace)

    [<Obsolete("Do not use anymore. Use FSharp.Stats.Signal.Padding.Discrete.padRnd with borderpadding=20,000 instead.")>]
    ///padds the trace with 20k data points randomly selected for 
    let paddTrace (cwtTrace:float[])= 
        let rnd = System.Random() 
        let padding = 20000  //number of zeros the data is padded with
        let listRnd = Array.init padding (fun _ -> cwtTrace.[rnd.Next(0,cwtTrace.Length-1)]|> float )
        let paddedCWT3Dtrace = Array.append (Array.append listRnd cwtTrace) listRnd
        paddedCWT3Dtrace

    ///computes the continuous wavelet transform of the padded trace with wavelet type myRicker.
    let transform2D borderpadding (myRicker: Ricker.myRicker) (paddedTrace: float []) =
        let padding =  borderpadding
        let myRickerOffsetRise = (6. * myRicker.ScaleRise) |> ceil |> int 
        let myRickerOffset = myRicker.PadArea
        let arr = Array.zeroCreate (paddedTrace.Length - (2 * padding))
        for i= padding to paddedTrace.Length-padding-1 do 
            let rec loop acc2 j =
                if j < myRickerOffset then
                    loop ((paddedTrace.[i+j] * myRicker.Values.[(myRickerOffset)+j]) + acc2) (j+1)
                //else (arr.[i-padding] <- acc2)
                else (arr.[i-padding] <- (acc2 / Math.Sqrt(myRicker.ScaleDecay))) //corrects for admissibility (see Polikar wavelet tutorial)
            loop 0. (- myRickerOffsetRise) 
        arr
    
    [<Obsolete("Do not use anymore. Use transform2D instead.")>]
    let cWT1D (paddedTrace: float []) (myRicker: Ricker.myRicker) = transform2D 20000 myRicker paddedTrace

    ///calculates the positions of maxima with given neighbourPoints and returns a maximaArray and the wavelet transformed trace
    let isolateLocalMaxima borderpadding paddedTrace ricker neighbourPoints = 
        let (cWTArray: float []) = 
            transform2D borderpadding ricker paddedTrace  
        let arrayOfMaxima = Array.zeroCreate (Array.length cWTArray)
        ///gets coordinates of pixel and number of pixel in surrounding and gives bool, if coor has highest value
        let checkListsForContinuousDecline pointindex =      
            let surroundingList =
                //writes values of surrounding values (accW = points left) (accE = points right) in order in single lists 
                let rec loop i accW accE =
                    if i <= neighbourPoints then 
                        loop (i+1) (cWTArray.[pointindex-i]::accW )
                                   (cWTArray.[pointindex+i]::accE )
                    else [accW;accE]
                loop 0 [] []
                
            ///checks if created intensity lists are in an ascending order
            let rec isSortedAsc (list: float list) = 
                match list with
                | [] -> true
                | [x] -> true
                | x::((y::_)as t) -> if x > y then false else isSortedAsc(t)
            //checks order of each direction (left/right)
            let boolList = surroundingList |> List.map  (fun x -> isSortedAsc x)
            //if any list is not sorted -> false
            not (List.exists (fun x -> x = false) boolList)
        //calculates decline for every point
        for i=neighbourPoints to (cWTArray.Length)-(neighbourPoints+1) do
            if checkListsForContinuousDecline i  = true  
                then arrayOfMaxima.[i] <- cWTArray.[i]
            else arrayOfMaxima.[i] <- 0.
        arrayOfMaxima,cWTArray
    
    [<Obsolete("Do not use anymore. Use isolateLocalMaxima instead.")>]
    let findLocalMaxima2D = isolateLocalMaxima

    ///takes a collection of wavelets and a padded trace and computes a merged trace, where maxima of each trace are merged
    let collectMaxima rickerarray neighbourPoints borderpadding (paddedTrace:float[]) = 
        let numberOfFrames = paddedTrace.Length - (2 * borderpadding)
        let arr = Array.zeroCreate numberOfFrames
        let arrCurve = Array.zeroCreate numberOfFrames
        let maximaArray =
            rickerarray
            |> Array.map (fun x ->  fst (isolateLocalMaxima borderpadding paddedTrace x neighbourPoints))
            |> fun x -> x |> JaggedArray.transpose
            |> Array.map (fun x -> x |> Array.max)
        let curveArray =
            rickerarray
            |> Array.map (fun x ->  snd (isolateLocalMaxima borderpadding paddedTrace x neighbourPoints))
            |> fun x -> x |> JaggedArray.transpose
            |> Array.map (fun x -> x |> Array.max)

        for i = neighbourPoints to (numberOfFrames - neighbourPoints - 1) do
            if maximaArray.[i] <= ((maximaArray.[(i-neighbourPoints)..(i+neighbourPoints)]) |> Array.max) then 
                arr.[i] <- maximaArray.[i]
        for i = neighbourPoints to (numberOfFrames - neighbourPoints - 1) do
            if curveArray.[i] <= ((curveArray.[(i-neighbourPoints)..(i+neighbourPoints)]) |> Array.max) then 
                arrCurve.[i] <- curveArray.[i]
        arr,arrCurve

    
    ///Gets single cell trace and fits a predefined calcium signal to the signal. Additionally you can set a threshold to discard low intensity signals.         
    let fitting j (maximaArr:float[]) threshold numberOfFrames rise=
        let fittingFunction2 factorX x =
            let sigma = 1.1
            let µ = Math.Log(factorX) + ((sigma*2.) |> sqrt |> sqrt)
            let factorY = 5.1 * factorX
            let streching = 0.1
            let fstTerm = factorY/(2.506628275*((x*streching) + factorX) * sigma)
            let sndTerm = Math.Exp(-((Math.Log(x*streching+factorX) - µ ) ** 2.)/(2. * sigma**2.))
            let result = fstTerm * sndTerm
            result

        let fittingFunction rise x =
            let factorSndTerm = 2. / rise
            let factorFstTerm = 3.4071 * factorSndTerm
            ((factorFstTerm * (x + rise)) / (Math.Sqrt(2. * Math.PI)))*Math.Exp(- 0.5 * (factorSndTerm * (x + rise)))

        //let blitarray = Array.zeroCreate (numberOfFrames+221)
        let blitarray = Array.zeroCreate (numberOfFrames+501)
        let fitOfStandardCurve j =
                if maximaArr.[j] >= threshold then 
                    //for i =j-7 to j+220 do
                        //blitarray.[i] <- (fittingFunction2 0.6 ((i - j) |> float)) * maximaArr.[j] 
                    //for i =j-(int rise) to j+220 do 
                    for i =j-(int rise) to j+500 do 
                        blitarray.[i] <- (fittingFunction2 rise ((i - j) |> float)) * maximaArr.[j] 
                    blitarray.[0 .. numberOfFrames - 1]
                else
                    blitarray.[j   ] <- 0.
                    blitarray.[0 .. numberOfFrames - 1]
        fitOfStandardCurve j  


    /// gets finalcentroidlist and cellnumber and fits the fitting with threshold to trace. With numberOfSurroundingIntensities you can set the area in which a maxima has to be the highest point
    let fittingCaSignalToMaxima rickerarray threshold borderpadding numberOfSurroundingIntensities rise (paddedTrace:float[]) = 
        let numberOfFrames = paddedTrace.Length - (2 * borderpadding)
        let (maximaarray,cWTCurve) = collectMaxima rickerarray numberOfSurroundingIntensities borderpadding paddedTrace

        let fittedList =
            [|(int rise)..numberOfFrames-1|]
            |> Array.map (fun x -> fitting x maximaarray threshold numberOfFrames rise)
            |> JaggedArray.transpose
            |> Array.map (fun x -> x |> Array.max)
            |> Array.map (fun x -> if nan.Equals x  then 0. else x)

        fittedList 