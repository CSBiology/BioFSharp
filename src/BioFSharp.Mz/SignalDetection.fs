namespace BioFSharp.Mz

open BioFSharp
open BioFSharp.IO

open System
open FSharp.Care
open FSharp.Care.Collections
open FSharp.Care.Monads
open AminoAcids 
open ModificationInfo



module SignalDetection =
    
    open BioFSharp.Mz.PeakArray

    module Wavelet =

        type RidgeLine = {
            Col: int
            Row: int
            }

        let createRidgeLine col row = {Col=col; Row=row}

        type peakDetermParameter = {
            MZTol        : float
            SNRThreshold : float
            }
    
        let createPeakDetermParameter mzTol snrThreshold = {
            MZTol=mzTol; SNRThreshold=snrThreshold
            }

        /// Returns a Array containing the distances between adjacent mz values of the input array.
        let createXspacing (xSpacing:float[]) (mzData:float[]) =
            for mzi=1 to mzData.Length-1 do
                    xSpacing.[mzi] <- mzData.[mzi] - mzData.[mzi-1]
            xSpacing.[0] <- xSpacing.[1]
            xSpacing.[mzData.Length-1] <- xSpacing.[mzData.Length-2]
            

        /// Helper function to accumulate Array values
        let accumulate fromI toI startV f (data:'T[]) =
            let rec loop i acc =
                if i = toI then 
                    acc
                else
                    loop (i+1) (f acc data.[i])
            loop fromI startV  

        /// Helper function for determining the score in a (sorted) vector at a given percentile
        /// end of getScoreAtPercentile. Allow passing of length of vector in case you only 
        /// want a slice of the first portion of a vector;
        /// perc should not be a fraction (e.g. 5th per centile = 5.0)
        let scoreAtPercentile (perc:float) (sortedData: float []) nTot = 
            let nBelow = (float (nTot-1)) * perc / 100. //Anzahl mal percentile 
            if (ceil nBelow) = nBelow 
                then sortedData.[int nBelow] //whole number
            else 
                let lowIndex = floor nBelow
                let highIndex = ceil nBelow
                let fraction = nBelow - lowIndex
                let low = sortedData.[int lowIndex]
                let high = sortedData.[int highIndex]
                (low + (high - low) * fraction)
        
        /// Helperfunction to calculate the mz values left and right from the target value which will be included in the computation of 
        /// the CWT correlation coefficient
        let getNpointsX mzi scaling maxMZwindow (nPoints: int [,,]) (mzData:float[])  =
            ///
            let rec getNPointsLeft acc counter (mzData':float[])  =  
                if counter >= 0 then
                    if (mzData.[mzi] - mzData.[counter]) > maxMZwindow  // || acc=20
                        then 
                            nPoints.[0, scaling, mzi] <- acc                     
                        else getNPointsLeft (acc+1) (counter-1) mzData
                else nPoints.[0, scaling, mzi] <- acc          
            getNPointsLeft 0 mzi mzData
            ///
            let rec getNPointsRight acc' counter (mzData':float[])  =
                if counter < mzData.Length then      
                    if ((mzData.[counter] - mzData.[mzi]) > maxMZwindow) // || acc'=20
                        then 
                            
                            nPoints.[1, scaling, mzi] <- acc'
                        else getNPointsRight (acc'+1) (counter+1) mzData
                else 
                     nPoints.[1, scaling, mzi] <- acc'
            getNPointsRight 0 mzi mzData    
                
        ///
        let getScales nScales (scalings:float []) windowMaxLow windowMaxHigh (mzData:float[])  (intensityData: float[]) (xSpacingAvg:float[]) (nPoints: int [,,]) (xspaced:float[]) =
            let maxSpacingToAvgSpacing = 1.
            for mzi = 1 to mzData.Length-2 do
                let windowLow' = if mzi < windowMaxLow then 0 else mzi - windowMaxLow
                let windowHigh' = if mzi >= mzData.Length - windowMaxHigh then mzData.Length - 1 else mzi + windowMaxHigh
                let nTot = windowHigh' - windowLow' |> float
                xSpacingAvg.[mzi] <- ((accumulate windowLow' windowHigh' 0. (+) xspaced) / nTot)

            // calculating noise threshold in bins
            let noise_perc = 50. 
            let mutable windowSize = 300
            if (windowSize > xSpacingAvg.Length-1) then
                windowSize <- (xSpacingAvg.Length-1) / 2
            let hf_window = windowSize / 2
            windowSize <- 2 * hf_window
            let nSpacingBins = ((xSpacingAvg.Length-1) / (windowSize + 1)) //number of NoiseBins
            let spacings = Array.create nSpacingBins 0.0

            for i = 0 to nSpacingBins-1 do
                let windowLow = i * windowSize
                let mutable windowHigh = windowLow + windowSize //not inclusive
                if i = nSpacingBins-1 
                    then windowHigh <- xSpacingAvg.Length
                let nTot = (windowHigh - windowLow)
                let unsortedData = Array.create nTot 0.0 //nTot ist = windowsize, es sei den die Distanz von windowlow zum array Ende ist größer als windowsize
                for j = windowLow to windowHigh-1 do
                    unsortedData.[j-windowLow] <- xSpacingAvg.[j] 
                let sortedData = unsortedData |> Array.sort 
                let mutable spacing = scoreAtPercentile noise_perc sortedData nTot //calcs Noisethreshold in bin i 
                
                spacings.[i] <- spacing

            for mzi = 1 to mzData.Length-2 do
                
                let mutable scalesToInclude = nScales    
                if intensityData.[mzi] < (0.75*intensityData.[mzi-1]) || intensityData.[mzi] < 0.75*intensityData.[mzi+1] then 
                     scalesToInclude <- 1
                let spacingBin = 
                    let tmp = mzi/windowSize
                    match tmp with
                    | tmp when tmp > nSpacingBins-1 -> nSpacingBins-1 
                    | _ -> tmp
                let spacingToAvgSpacing = xSpacingAvg.[mzi] / spacings.[spacingBin]                  
                if spacingToAvgSpacing > maxSpacingToAvgSpacing then
                        scalesToInclude <- 1    
                // figure out the number of wavelet points you'll need to sample for each m/z point
                for i = 0 to scalesToInclude-1 do 
                    let maxMZwindow = xSpacingAvg.[mzi] * scalings.[i] * 1.5 //3.0
                    getNpointsX mzi i maxMZwindow nPoints mzData
            

        ///
        let createPadding paddingPoints (mzDataPadded:float[]) (intensityDataPadded:float[]) (mzData:float[]) (intensityData: float[]) = 
            for i = paddingPoints to mzData.Length - 1 + paddingPoints do 
                mzDataPadded.[i] <- mzData.[i-paddingPoints]
                intensityDataPadded.[i] <- intensityData.[i-paddingPoints]

        ///
        let ricker2d (mzDataPadded:float []) focusedPaddedMzColIndex nPointsLeft nPointsRight waveletParam1 waveletParam2 focusedPaddedMzValue (waveletData:float []) =  //(PadMz, paddedCol, nPointsLeft, nPointsRight, param1, param2, PadMz[paddedCol], waveletData)
            let computation cnt i =
                let vec = mzDataPadded.[i]-focusedPaddedMzValue
                let tsq = vec*vec
                let modi = 1. - tsq / waveletParam2
                let gauss = exp(-1. * tsq / (2.0 *waveletParam2) )
                waveletData.[cnt] <- waveletParam1 * modi * gauss
            let rec ricker cnt i = 
                if i = focusedPaddedMzColIndex+nPointsRight then 
                     computation cnt i
                else computation cnt i
                     ricker (cnt+1) (i+1)
            if nPointsLeft - nPointsRight >= waveletData.Length 
                then printfn "invalid input Parameters for ricker2d"
            ricker 0 (focusedPaddedMzColIndex-nPointsLeft)



        let calcCorrWaveletAndDataMatrix nScales paddingPoints mzDataPadded (intensityDataPadded:float[]) (mzData:float[]) (intensityData: float[]) waveletData (nPoints: int[,,]) (xSpacingAvg: float []) (scalings: float []) (corrMatrix: float [,]) = 
            for i = 0 to nScales-1 do //nScales-1 
                let currentscaling = scalings.[i]
           
                let mutable matrixIndex = 2 
                let calcCorrelationAtPosX scaling targetCol =
                    let nPointsLeft = nPoints.[0,i,targetCol]
                    let nPointsRight = nPoints.[1,i,targetCol]
                    if nPointsLeft = 0 && nPointsRight = 0 then 
                        matrixIndex <- (matrixIndex+2)
                    else
                        let targetPaddedCol = targetCol + paddingPoints
                        
                        let width = xSpacingAvg.[targetCol]*currentscaling

                        let waveletParam1 =  2.0 / ( sqrt(3.0 * width) * (sqrt( sqrt(3.141519) ) ) )
                        let waveletParam2 = width * width
                        // ricker, schreibt in "waveletdata", ein Array dem die Abstände zwischen den punkten zugrunde liegen. 
                        // und gibt die Werte der Waveletfunktion in Abhängigkeit des fokussierten m/z values wieder 
                        ricker2d mzDataPadded targetPaddedCol nPointsLeft nPointsRight waveletParam1 waveletParam2 mzDataPadded.[targetPaddedCol] waveletData
        
                        //daraufhin wird die Korrelationsmatrix gefüllt unzwar an der Spalte des aktuellen scalings gepaart mit einem aufsteigenden Index welcher der Nr des m/z values
                        //im Urpsprungs m/z Array entspricht. Dafür wird der "waveletData" Array durchlaufen und mit der Intensität an der entsprechenden Stelle multipliziert, die Summe der Ergebnisse wird in 
                        //die Korrelationsmatrix eingetragen. Es handelt sich um den CWT Koeffizienten
                        let startpoint = targetPaddedCol - nPointsLeft
                        for k = 0 to nPointsLeft+nPointsRight do
            
                            corrMatrix.[i, matrixIndex] <- (corrMatrix.[i, matrixIndex] +  (waveletData.[k] * intensityDataPadded.[k+startpoint] ) )
            
                        matrixIndex <- matrixIndex+1

                        if targetCol < mzData.Length-1 then //falls j = mzData.Length.1 dann ist man am ende des m/z Arrays angekommen und die corrMatrix ist fertig
                                                            // falls nicht : Dieser Vorgang wird wiederholt unzwar wird dazu ein waveletdataarray erzeugt, der auf den Punkten zwischen den m/z werden basiert.
                            let moverzShift =  ( (mzDataPadded.[targetPaddedCol] + mzDataPadded.[targetPaddedCol+1]) / 2.0 )
                            ricker2d mzDataPadded targetPaddedCol nPointsLeft nPointsRight waveletParam1 waveletParam2 moverzShift waveletData
                            
                            for k = 0 to nPointsLeft+nPointsRight do                
                                corrMatrix.[i, matrixIndex] <- (corrMatrix.[i, matrixIndex] + (waveletData.[k] * intensityDataPadded.[k + startpoint] ) )
                            
                            matrixIndex <- matrixIndex+1
                                    
                   
                for j = 1 to mzData.Length-2 do 
                    
                    if i>0 then 
                        if intensityData.[j] < 0.75*intensityData.[j-1] || intensityData.[j] < 0.75*intensityData.[j+1] then 
                             matrixIndex <- (matrixIndex+2) 
                        else calcCorrelationAtPosX i j                   
                    else
                        calcCorrelationAtPosX i j 
                   
            
        /// Helperfunction to get mz back of Columnnumber
        let convertColToMz (mzData: float []) (col: int) =
            let mapIndex = col / 2
            if ( col % 2 = 1)
                then (mzData.[mapIndex] + mzData.[mapIndex+1]) / 2.0
            else 
                mzData.[mapIndex] 

        ///
        let getColLowBound  (interpolatedPoints: float[]) i mzTol = 
            let rec loop  (interpolatedPoints: float []) center i mzTol =
                if i = 0 then i
                elif i <= 0 then i+1  
                else                                               //prevents from jumping out of Array boundaries 
                    match (interpolatedPoints.[center] - interpolatedPoints.[i]) with 
                    | x when x >= mzTol -> i   
                    | x when x <= mzTol -> loop interpolatedPoints center (i-1) mzTol
            loop interpolatedPoints i (i-1) mzTol

        /// 
        let getColHighBound (interpolatedPoints: float []) i mzTol = 
            let rec loop (interpolatedPoints: float []) center i mzTol = 
                if i = interpolatedPoints.Length-1
                    then i
                elif i = interpolatedPoints.Length
                    then i-1
                else                                              //prevents from jumping out of Array boundaries 
                    match interpolatedPoints.[i] - interpolatedPoints.[center] with 
                    | x when x >= mzTol -> i 
                    | x when x <= mzTol -> loop interpolatedPoints center (i+1) mzTol
            loop interpolatedPoints i (i+1) mzTol

        let getPeakLines nScales mzData (allLines:Collections.Generic.List<RidgeLine>) (snrs:Collections.Generic.List<float>) (corrMatrix: float[,]) =                                           
            let corrMatrixLength = corrMatrix.[0,*].Length
            // step 1: find maxima in each column (row = scales, columns = m/z)
            /// contains scaling with the highest correlationvalue for each Column
            let colMaxes = Array.create (corrMatrixLength) 0 
            for i = 0 to corrMatrixLength-1 do 
                
                let mutable corrMax = 0.0
                
                for j = 0 to nScales-1 do 
                
                    if corrMatrix.[j,i] > corrMax then
                        corrMax <- corrMatrix.[j,i]
                        colMaxes.[i] <- j //Eintrag des scalings mit der höchsten correlation

            let interpolatedXpoints = Array.create corrMatrixLength 0.0 
            for i = 0 to corrMatrixLength-1 do
                let interpolatedpointOfI = convertColToMz mzData i 
                interpolatedXpoints.[i] <- interpolatedpointOfI

            // step 3, find local maxima that are separated by at least mzTol_
            let findLocalMaxima i = 
                let mzCol = convertColToMz mzData i //mz Value of Column
                // mzTol "sets the mz tolerance. It defaults to 0.01 in high resoltion mode, otherwise it defaults to 0.5."
                // in the paper 0.1 was used
                let mzTol = 0.1 
                let highTol = mzCol + mzTol
                // get the indices for the lower and upper bounds   
                let lowBound  = getColLowBound interpolatedXpoints i mzTol
                let highBound = getColHighBound interpolatedXpoints i mzTol
                // Now all m/z Values in the space between lowBound and highBound get analyzed and 
                // the Column of the m/z with the  maxCorr is identified
                /// contains correlationvalue of the m/z Value with the highest correlation in the current bin 
                let mutable maxCorr = 0.0
                let mutable maxCol = 0
                
                for j = lowBound to highBound do 
                    let row = colMaxes.[j]
                    if corrMatrix.[row,j] > maxCorr
                        then
                                maxCorr <- corrMatrix.[row,j]
                                maxCol <- j

                let nLines = allLines.Count

                if ( nLines > 0) then
                    let mzNewLine = convertColToMz mzData maxCol 
                    let mzPrevLine = convertColToMz mzData allLines.[(nLines-1)].Col
                    let mzDiff = mzNewLine - mzPrevLine
     
                    let corrPrev = corrMatrix.[allLines.[nLines-1].Row, allLines.[nLines-1].Col]
          
                    if mzDiff > mzTol then 
                        let newLine = createRidgeLine maxCol colMaxes.[maxCol]
                        allLines.Add newLine

                    elif maxCorr > corrPrev then 
                        allLines.[allLines.Count-1] <- createRidgeLine maxCol colMaxes.[maxCol]
                 else let newLine = createRidgeLine maxCol colMaxes.[maxCol]
                      allLines.Add newLine

            let rec findLocalMaximaLoop i (corrMatrix: float [,]) = 
                if i = corrMatrixLength-3 then 
                    findLocalMaxima i
                    
                else let correlationVal = corrMatrix.[colMaxes.[i],i]
                     if  correlationVal < corrMatrix.[colMaxes.[i-1],i-1] ||
                         correlationVal < corrMatrix.[colMaxes.[i-2],i-2] ||
                         correlationVal < corrMatrix.[colMaxes.[i+1],i+2] ||
                         correlationVal < corrMatrix.[colMaxes.[i+1],i+1] then 
                              findLocalMaximaLoop (i+1) corrMatrix
                         else findLocalMaxima i
                              findLocalMaximaLoop (i+1) corrMatrix
            findLocalMaximaLoop 2 corrMatrix  

        ///
        let getSNRSFilteredPeakLines nScales mzData (allLines:Collections.Generic.List<RidgeLine>) (snrs:Collections.Generic.List<float>) (corrMatrix: float[,]) =                                            
            let minSnr = 1.
            let corrMatrixLength = corrMatrix.[0,*].Length
            // step 1: find maxima in each column (row = scales, columns = m/z)
            /// contains scaling with the highest correlationvalue for each Column
            let colMaxes = Array.create (corrMatrixLength) 0 
            for i = 0 to corrMatrixLength-1 do 
                
                let mutable corrMax = 0.0
                
                for j = 0 to nScales-1 do 
                
                    if corrMatrix.[j,i] > corrMax then
                        corrMax <- corrMatrix.[j,i]
                        colMaxes.[i] <- j //Eintrag des scalings mit der höchsten correlation
            // step 2, setup bins of 300 points and calculate noise threshold within each bin
            let noise_perc = 95.0 
            let mutable windowSize = 300
            if (windowSize > corrMatrixLength) 
                then windowSize <- corrMatrixLength / 2
            let hf_window = windowSize / 2
            windowSize <- 2 * hf_window

            let nNoiseBins = corrMatrixLength / windowSize + 1 //number of NoiseBins
            let noises = Array.create nNoiseBins 0.0

            for i = 0 to nNoiseBins-1 do
                let windowLow = i * windowSize  // inclusive
                let mutable windowHigh = windowLow + windowSize //not inclusive
                if i = nNoiseBins-1 
                    then windowHigh <- corrMatrixLength
                let nTot = windowHigh - windowLow // don't need +1 because windowHigh is not inclusive

                let unsortedData = Array.create nTot 0.0 //nTot ist = windowsize, es sei den die Distanz von windowlow zum array Ende ist größer als windowsize
                
                for j = windowLow to windowHigh - 1 do
                    unsortedData.[j-windowLow] <- corrMatrix.[0, j] // first row of correlation matrix
                
                let sortedData = unsortedData |> Array.sort
                let mutable noise = scoreAtPercentile noise_perc sortedData nTot //calcs Noisethreshold in bin i 
                if noise < 1.0 then noise <- 1.0
                noises.[i] <- noise

            let interpolatedXpoints = Array.create corrMatrixLength 0.0 
            for i = 0 to corrMatrixLength-1 do
                let interpolatedpointOfI = convertColToMz mzData i 
                interpolatedXpoints.[i] <- interpolatedpointOfI

            // step 3, find local maxima that are separated by at least mzTol_
            let findLocalMaxima i = 
                let mzCol = convertColToMz mzData i //mz Value of Column
                // mzTol "sets the mz tolerance. It defaults to 0.01 in high resoltion mode, otherwise it defaults to 0.5."
                // in the paper 0.1 was used
                let mzTol = 0.1 
                let highTol = mzCol + mzTol
                // get the indices for the lower and upper bounds   
                let lowBound  = getColLowBound interpolatedXpoints i mzTol
                let highBound = getColHighBound interpolatedXpoints i mzTol
                // Now all m/z Values in the space between lowBound and highBound get analyzed and 
                // the Column of the m/z with the  maxCorr is identified
                /// contains correlationvalue of the m/z Value with the highest correlation in the current bin 
                let mutable maxCorr = 0.0
                let mutable maxCol = 0

                for j = lowBound to highBound do 
                    let row = colMaxes.[j]
                    if corrMatrix.[row,j] > maxCorr
                        then
                                maxCorr <- corrMatrix.[row,j]
                                maxCol <- j
                let noiseBin = 
                    let tmp = maxCol/windowSize
                    match tmp with
                    | tmp when tmp > nNoiseBins-1 -> nNoiseBins-1 
                    | _ -> tmp    
 
                
                let snr = maxCorr / noises.[noiseBin]
                if snr > minSnr then 
        
                    let nLines = allLines.Count

                    if ( nLines > 0) then
                        let mzNewLine = convertColToMz mzData maxCol 
                        let mzPrevLine = convertColToMz mzData allLines.[(nLines-1)].Col
                        let mzDiff = mzNewLine - mzPrevLine
     
                        let corrPrev = corrMatrix.[allLines.[nLines-1].Row, allLines.[nLines-1].Col]
          
                        if mzDiff > mzTol then 
                            let newLine = createRidgeLine maxCol colMaxes.[maxCol]
                            allLines.Add newLine
                            snrs.Add snr

                        elif maxCorr > corrPrev then 
                            allLines.[allLines.Count-1] <- createRidgeLine maxCol colMaxes.[maxCol]
                            snrs.[snrs.Count-1] <- snr
                    else let newLine = createRidgeLine maxCol colMaxes.[maxCol]
                         allLines.Add newLine
                         snrs.Add snr

            let rec findLocalMaximaLoop i (corrMatrix: float [,]) = 
                if i = corrMatrixLength-3 then 
                    findLocalMaxima i
                    
                else let correlationVal = corrMatrix.[colMaxes.[i],i]
                     if  correlationVal < corrMatrix.[colMaxes.[i-1],i-1] ||
                         correlationVal < corrMatrix.[colMaxes.[i-2],i-2] ||
                         correlationVal < corrMatrix.[colMaxes.[i+1],i+2] ||
                         correlationVal < corrMatrix.[colMaxes.[i+1],i+1] then 
                              findLocalMaximaLoop (i+1) corrMatrix
                         else findLocalMaxima i
                              findLocalMaximaLoop (i+1) corrMatrix
            findLocalMaximaLoop 2 corrMatrix  
               
        /// 
        let refinePeaks (scalings: float[]) (mzData:float []) (intensityData:float [])  (allLines: ResizeArray<RidgeLine>) (xSpacingAvg:float []) = // (snrs: ResizeArray<float>) = 
            
            let xPeakValues = ResizeArray<float>()
            let yPeakValues = ResizeArray<float>()

            if allLines.Count = 0 then 
                let finalX = xPeakValues |> Seq.toArray 
                let finalY = yPeakValues |> Seq.toArray
                finalX, finalY 
            else
                for i = 0 to allLines.Count-1 do

                    let mzCol = convertColToMz mzData allLines.[i].Col    

                    let row = allLines.[i].Row
                    let currentScaling = scalings.[row]
                    let offset = currentScaling * xSpacingAvg.[int (allLines.[i].Col / 2)]
                    // get the indices for the lower and upper bounds that encapsulate the peak
                    let startFittingPoint = match mzData |> Array.tryFindIndex (fun x -> x > mzCol-offset) with
                                            | Some x -> x 
                                            | None   -> 0
                    let endFittingPoint = match mzData |> Array.tryFindIndexBack (fun x -> x < mzCol+offset) with
                                            | Some x -> x
                                            | None   -> mzData.Length-1
                    // sum up the intensity and find the highest point. If there are multiple
                    // points with the maxIntensity value, take the one with the highest m/z.
                    let mutable maxIntensity = 0.0 
                    let mutable intensityAccumulator = 0.0 
                    let mutable maxIntensityMZ = 0.0


                    for j = startFittingPoint to endFittingPoint do 
                        intensityAccumulator <- intensityAccumulator + intensityData.[j] //is never used, maybe important to determine the spectra intensity?
                        if intensityData.[j] >= maxIntensity
                            then
                                maxIntensity   <- intensityData.[j]
                                maxIntensityMZ <- mzData.[j]

                    
                    if i > 1 && (maxIntensityMZ - xPeakValues.[xPeakValues.Count-1] ) > 0.1 then
                            xPeakValues.Add maxIntensityMZ
                            yPeakValues.Add maxIntensity
       
                    elif i=0 then
                            xPeakValues.Add maxIntensityMZ
                            yPeakValues.Add maxIntensity

            
                let finalX = xPeakValues |> Seq.toArray 
                let finalY = yPeakValues |> Seq.toArray

                finalX, finalY 

        /// Returns a MzIntensityArray that containing the spectral centroids of the input spectra. 
        let toCentroidWith peakLineF (mzData: float []) (intensityData: float [])  =
            if mzData.Length < 3 then
                [||], [||] 
            else  
            //FixedParameters
            let nScales = 10;
            let windowMaxLow = 5
            let windowMaxHigh = 5
            let initialWidthScaling = 1.; 
            let finalWidthScaling = 6.; 
            let incrementScaling = (finalWidthScaling-initialWidthScaling) / double(nScales-1);
    
            let scalings = 
                Array.init nScales (fun index ->
                                        initialWidthScaling + float index * incrementScaling
                                   )

            // Calculation of the mzSpacing of each mz value
            let xSpacing = Array.create (mzData.Length) 0.
            createXspacing xSpacing mzData 

            // Calculation of the average mz spacing of each mz value. This information is used to determine
            // the number of mz values for each individual mz value that are included in the computation of the individual
            // correlation
            let xSpacingAvg = Array.create (mzData.Length) 0.
            let nPoints = Array3D.create 2 nScales mzData.Length 0
            getScales nScales scalings windowMaxLow windowMaxHigh mzData intensityData  xSpacingAvg nPoints xSpacing    
            
            // Creates padded versions of the given data arrays
            let paddingPoints = 10000 // Can propably be reduced
            let mzDataPadded = Array.create (mzData.Length+2*paddingPoints) 0.
            let intensityDataPadded = Array.create (mzData.Length+2*paddingPoints) 0.   
            createPadding paddingPoints mzDataPadded intensityDataPadded mzData intensityData
            
            // Calculation of the wavelet correlation for each mzValue       
            let waveletData = Array.create (paddingPoints) 0.   
            let corrMatrixLength = 2*mzData.Length-1 //almost twice as long because also the correlation for Midpoints will be calculated.
            let corrMatrix = Array2D.create nScales corrMatrixLength 0. 
            calcCorrWaveletAndDataMatrix nScales paddingPoints mzDataPadded intensityDataPadded mzData intensityData waveletData nPoints xSpacingAvg scalings corrMatrix

            // Compares the previously selected correlation values of each mz value locally and selects the best scoring point
            let allLines = ResizeArray<RidgeLine>() 
            let snrs = ResizeArray<float>() 
            peakLineF nScales mzData allLines snrs corrMatrix
            
            // 
            refinePeaks scalings mzData intensityData allLines xSpacingAvg 
        
        /// Returns a MzIntensityArray that containing the spectral centroids of the input spectra. 
        let toCentroid (mzData: float []) (intensityData: float [])  =
            toCentroidWith getPeakLines mzData intensityData
        
        /// Returns a MzIntensityArray that containing the spectral centroids of the input spectra. 
        let toSNRSFilteredCentroid (mzData: float []) (intensityData: float [])  =
            toCentroidWith getSNRSFilteredPeakLines mzData intensityData    

    /// Returns mzIntensityArray consisting of centroided Peaks. 
    let windowToCentroid centroidF (mzData:float[]) (intensityData:float[]) lowerIdx upperIdx =
        if mzData.Length > 2  && lowerIdx>=0 && mzData.Length-2 >=upperIdx then 
            centroidF mzData.[lowerIdx.. upperIdx] intensityData.[lowerIdx.. upperIdx]  
        else [|float lowerIdx |], [|float upperIdx |]

    /// Returns a Index that accesses the mzData Array at a position determined by a precursorMz at the lower end of a given windowwidth 
    let lowerIdxBy (mzData:float []) windowWidth preCursorMZ =
        match Array.tryFindIndex (fun x -> x > preCursorMZ-(windowWidth/2.)) mzData with 
        | Some x -> x 
        | None -> mzData.Length-1 

    /// Returns a Index that accesses the mzData Array at a position determined by a precursorMz at the upper end of a given windowwidth 
    let upperIdxBy (mzData:float []) windowWidth preCursorMZ = 
        match Array.tryFindIndexBack (fun x -> x < preCursorMZ+(windowWidth/2.)) mzData with 
        | Some x -> x 
        | None -> mzData.Length-1 
    
    // Returns mzIntensityArray consisting of centroided Peaks. 
    let windowToCentroidBy centroidF (mzData:float[]) (intensityData:float[]) windowWidth centerMass =
        if mzData.Length > 2 then
            let lowerIdx = lowerIdxBy mzData windowWidth centerMass
            let upperIdx = upperIdxBy mzData windowWidth centerMass
            windowToCentroid centroidF mzData intensityData lowerIdx upperIdx
        else [||], [||]