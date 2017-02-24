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

    module Filtering =

        open FSharp.Care
        open FSharp.Care.Collections
        open MathNet.Numerics
        open MathNet.Numerics.LinearAlgebra

        /// Smooth (and optionally differentiate) data with a Savitzky-Golay filter.
        /// The Savitzky-Golay filter is a type of low-pass filter and removes high frequency noise from data.
        //  Parameters
        //  ----------
        //  data : array_like, shape (N,)
        //     the values of the time history of the signal.
        //  window_size : int
        //     the length of the window. Must be an odd integer number.
        //  order : int
        //     the order of the polynomial used in the filtering.
        //     Must be less then `window_size` - 1.
        //  deriv: int
        //     the order of the derivative to compute (default = 0 means only smoothing)
        //
        //  The Savitzky-Golay is a type of low-pass filter, particularly suited for smoothing noisy data. 
        //  The main idea behind this approach is to make for each point a least-square fit with a
        //  polynomial of high order over a odd-sized window centered at the point.
        let savitzky_golay (window_size:int) (order:int) deriv rate (data:float[]) =

            /// Calculates the pseudo inverse of the matrix
            let pseudoInvers (matrix:#Matrix<float>) =
                if matrix.RowCount > matrix.ColumnCount then
                    // Pseudo Inverse (A rows > columns)
                    matrix.QR().Solve(DenseMatrix.identity(matrix.RowCount))
                else
                    // Pseudo Inverse (A rows < columns):
                    matrix.Transpose().QR().Solve(DenseMatrix.identity(matrix.ColumnCount)).Transpose()

            ///             
            let correlate_valid (x:Vector<float>) (y:Vector<float>) =
                if x.Count >= y.Count then  
                    vector [x * y]
                else
                    let n = x.Count
                    vector [ for i=1 to y.Count-n do
                             yield x * y.[i..i+n-1] ]


            if window_size % 2 <> 1 || window_size < 1 then
                failwith "window_size size must be a positive odd number"
            if window_size < order + 2 then
                failwith "window_size is too small for the polynomials order"
            //let order_range = [0..order]
            let half_window = (window_size - 1) / 2
            // precompute coefficients
            let b = [|for colI=0 to order do
                        for k= -half_window to half_window do  yield float(k)**float(colI)|]
                    |> DenseMatrix.raw (half_window*2 + 1) (order+1)
    
            let m = (pseudoInvers b).Row(deriv) * ((float(rate)**float(deriv)) * SpecialFunctions.Factorial(deriv))
            //pad the signal at the extremes with values taken from the signal itself
    
            let firstvals = 
                let length = half_window + 1    
                Array.init length (fun i -> 
                    data.[0] - (abs data.[length-i] - data.[0]))
    
            let lastvals = 
                Array.init half_window (fun i -> 
                    data.[data.Length-1] - (abs data.[data.Length-(2+i)] - data.[data.Length-1]) ) 
           
            let y = 
                Array.concat [firstvals; data; lastvals;] |> DenseVector.raw    
    
            correlate_valid m y

    module Care =    
    
        ///
        type Extrema =
            | None = 0
            | Positive = 1 
            | Negative = 2

        ///
        let getColLowBound  (xData: float[]) centerIdx mzTol = 
            let rec loop  (xData: float []) center i mzTol =
                if i <= 0 then 0
                else                                              
                    match (xData.[center] - xData.[i]) with 
                    | x when x >= mzTol -> i+1   
                    | x when x <= mzTol -> loop xData center (i-1) mzTol
            loop xData centerIdx (centerIdx-1) mzTol

        /// 
        let getColHighBound (xData: float []) centerIdx mzTol = 
            let rec loop (xData: float []) center i mzTol = 
                if i >= xData.Length-1 then xData.Length-1
                else                                             
                    match xData.[i] - xData.[center] with 
                    | x when x >= mzTol -> i-1
                    | x when x <= mzTol -> loop xData center (i+1) mzTol
            loop xData centerIdx (centerIdx+1) mzTol
    
        ///
        let calc targetX state currentX currentY =
            let m,i = state
            (m+currentX*currentY,i+currentY)


        /// Returns a collection local maxima. Attention: The algorithm is very sensitive to noise and behaves irregulary for negative Y-values.
        let localMaxima yThreshold (xData:float[]) (smoothYData:float[]) =
            if xData.Length <= 5 then [||]
            else       
            [|for i = 3 to xData.Length-3 do
                // Peak must be concave in the interval [i-2 .. i+2] and exheat a yThreshold
                if (smoothYData.[i] > yThreshold && smoothYData.[i] > smoothYData.[i - 1] && smoothYData.[i] > smoothYData.[i + 1] 
                                        && smoothYData.[i - 1] >= smoothYData.[i - 2] && smoothYData.[i + 1] >= smoothYData.[i + 2]) then

                    // take the intensity at the apex of the profile peak
                    yield (xData.[i], smoothYData.[i])
                |]  
    
        /// Returns a collection of indices corresponding to local maxima. Attention: The algorithm is very sensitive to noise and behaves irregulary for negative Y-values.
        let localMaximaIdx yThreshold (xData:float[]) (smoothYData:float[]) =
            if xData.Length <= 5 then [||]
            else       
            [|for i = 3 to xData.Length-3 do
                // Peak must be concave in the interval [i-2 .. i+2] and exheat a yThreshold
                if (smoothYData.[i] > yThreshold && smoothYData.[i] > smoothYData.[i - 1] && smoothYData.[i] > smoothYData.[i + 1] 
                                        && smoothYData.[i - 1] >= smoothYData.[i - 2] && smoothYData.[i + 1] >= smoothYData.[i + 2]) then

                    // take the intensity at the apex of the profile peak
                    yield i
                |]  

        /// Returns a collection of local minima. Attention: The algorithm is very sensitive to noise   
        let localMinima (xData:float[]) (smoothYData:float[]) =
            if xData.Length <= 5 then [||]
            else
            [|for i = 3 to xData.Length-3 do
                // Peak must be concave in the interval [i-2 .. i+2] and exheat a min hight (min_dh)
                if (smoothYData.[i] < smoothYData.[i - 1] && smoothYData.[i] <= smoothYData.[i + 1]  //smoothYData.[i] > yThreshold
                    && smoothYData.[i - 1] < smoothYData.[i - 2] && smoothYData.[i + 1] <= smoothYData.[i + 2]) then

                    // take the intensity at the apex of the profile peak
                    yield (xData.[i], smoothYData.[i])
                |]    

        /// Returns a collection of indices corresponding to local minima. Attention: The algorithm is very sensitive to noise   
        let localMinimaIdx (xData:float[]) (smoothYData:float[]) =
            if xData.Length <= 5 then [||]
            else
            [|for i = 3 to xData.Length-3 do
                // Peak must be concave in the interval [i-2 .. i+2] and exheat a min hight (min_dh)
                if (smoothYData.[i] < smoothYData.[i - 1] && smoothYData.[i] <= smoothYData.[i + 1]  //smoothYData.[i] > yThreshold
                    && smoothYData.[i - 1] < smoothYData.[i - 2] && smoothYData.[i + 1] <= smoothYData.[i + 2]) then

                    // take the intensity at the apex of the profile peak
                    yield i
                |]    

        /// Returns a collection of local Maxima and Minima. Attention: The algorithm is very sensitive to noise   
        let labelPeaks negYThreshold posYThreshold (xData:float[]) (smoothYData:float[]) =
            if xData.Length <= 5 then [||]
            else
            [|for i = 3 to xData.Length-3 do
                if (smoothYData.[i] > posYThreshold && smoothYData.[i] > smoothYData.[i - 1] && smoothYData.[i] > smoothYData.[i + 1] 
                    && smoothYData.[i - 1] >= smoothYData.[i - 2] && smoothYData.[i + 1] >= smoothYData.[i + 2]) then
                    yield {Meta=Extrema.Positive; Data= xData.[i],smoothYData.[i]} //TODO: Typ is tin Peak.fs definiert, creatorFunktion verwenden

                // Peak must be concave in the interval [i-2 .. i+2] and exheat a min hight (min_dh)
                elif (smoothYData.[i] < negYThreshold && smoothYData.[i] < smoothYData.[i - 1] && smoothYData.[i] <= smoothYData.[i + 1]  //smoothYData.[i] > yThreshold
                    && smoothYData.[i - 1] < smoothYData.[i - 2] && smoothYData.[i + 1] <= smoothYData.[i + 2]) then

                    // take the intensity at the apex of the profile peak
                    yield {Meta=Extrema.Negative; Data= xData.[i],smoothYData.[i]}
                else
                    yield {Meta=Extrema.None; Data= xData.[i],smoothYData.[i]}
                |]    

        /// Returns a Array containing the distances between adjacent mz values of the input array.
        let createXspacing (mzData:float[]) =
            let xSpacing = Array.create (mzData.Length) 0.
            for mzi=1 to mzData.Length-1 do
                    xSpacing.[mzi] <- mzData.[mzi] - mzData.[mzi-1]
            xSpacing.[0] <- xSpacing.[1]
            xSpacing.[mzData.Length-1] <- xSpacing.[mzData.Length-2]
            xSpacing

        /// Helper function to accumulate Array values
        let accumulate fromIdx toIdx startV f (data:'T[]) =
            let rec loop i acc =
                if i = toIdx then 
                    acc
                else
                    loop (i+1) (f acc data.[i])
            loop fromIdx startV  

        /// Helper function for determining the score in a (sorted) vector at a given percentile
        /// end of getScoreAtPercentile. Allow passing of length of vector in case you only 
        /// want a slice of the first portion of a vector;
        /// perc should not be a fraction (e.g. 5th per centile = 5.0)
        let scoreAtPercentile (perc:float) nTot (sortedData: float [])  = 
            if nTot = 0 then 0.
            else
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
    
    module Padding = 

        let paddDataWithZeros (windowSize: int) (mzTol: float) (spacing_perc:float)  (xData: float []) (yData: float []) = 
            //////////////////////////////////////////////////////////////////////////
            //Step 1. Estimate the spacing between adjacent mzPoints (mz- (mz-1))
            let xSpacing = Care.createXspacing xData
            //////////////////////////////////////////////////////////////////////////
            //Step 2. Estimate the spacing threshold in bins
            let mutable windowSize = windowSize
            if (windowSize > xSpacing.Length-1) then
                windowSize <- (xSpacing.Length-1) / 2
            let hf_window = windowSize / 2
            windowSize <- 2 * hf_window

            let nSpacingBins = ((xSpacing.Length-1) / (windowSize + 1))
            let spacings = Array.zeroCreate (nSpacingBins+1) 
            for i = 0 to nSpacingBins-1 do
                let windowLow = i * windowSize
                let mutable windowHigh = windowLow + windowSize //not inclusive
                if i = nSpacingBins-1 
                    then windowHigh <- xSpacing.Length
                let nTot = (windowHigh - windowLow)

                let unsortedData = Array.zeroCreate nTot //nTot ist = windowsize, es sei den die Distanz von windowlow zum array Ende ist größer als windowsize
                for j = windowLow to windowHigh-1 do
                    unsortedData.[j-windowLow] <- xSpacing.[j] 
                let sortedData = unsortedData |> Array.sortDescending 
                let mutable spacingAtPerc = Care.scoreAtPercentile spacing_perc nTot sortedData  //calcs Noisethreshold in bin i                 
                spacings.[i] <- spacingAtPerc
            //////////////////////////////////////////////////////////////////////////
            //Step 3. If the mzSpacing to the average spacing in the selected bin is greater than 1. add mz points with zero intensity to the data
            let maxSpacingToAvgSpacing = 1.
            let xyData = Array.zip xData yData 
            let paddedData = ResizeArray<float*float>()
            for mzi = 1 to xyData.Length-2 do
                let spacingBin =  //TODO zu care
                    if  nSpacingBins = 0 then 0
                    else
                        let tmp = mzi/windowSize
                        match tmp with
                        | tmp when tmp > nSpacingBins-1 -> nSpacingBins-1 
                        | _ -> tmp
                let spacingToAvgSpacing = xSpacing.[mzi+1] / spacings.[spacingBin]                
                if spacingToAvgSpacing > maxSpacingToAvgSpacing then
                    if xSpacing.[mzi+1] > mzTol then 
                        paddedData.Add xyData.[mzi]
                        for i = 1 to (int ( xSpacing.[mzi+1] / mzTol ) - 1 )  do
                            paddedData.Add (xData.[mzi] + ((float i)*mzTol) ,1.)
                    else
                        paddedData.Add xyData.[mzi]
                else
                    paddedData.Add xyData.[mzi]
            let nTotFinal = paddedData.Count
            let xDataPadded = Array.zeroCreate nTotFinal
            let yDataPadded = Array.zeroCreate nTotFinal 
            paddedData 
            |> Seq.iteri (fun i (x,y) -> 
                            xDataPadded.[i] <- x
                            yDataPadded.[i] <- y
                        )  
            xDataPadded, yDataPadded   

    module Wavelet =

        // use struct.
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

        /// Helperfunction to calculate the mz values left and right from the target value which will be included in the computation of 
        /// the CWT correlation coefficient
        let getNpointsX centerIdx scaling maxMZwindow (nPoints: int [,,]) (mzData:float[])  =
            ///
            let rec getNPointsLeft acc counter (mzData':float[])  =  
                if counter >= 0 then
                    if (mzData.[centerIdx] - mzData.[counter]) > maxMZwindow  
                        then 
                            nPoints.[0, scaling, centerIdx] <- acc                     
                        else getNPointsLeft (acc+1) (counter-1) mzData
                else nPoints.[0, scaling, centerIdx] <- acc          
            getNPointsLeft 0 centerIdx mzData
            ///
            let rec getNPointsRight acc' counter (mzData':float[])  =
                if counter < mzData.Length then      
                    if ((mzData.[counter] - mzData.[centerIdx]) > maxMZwindow) 
                        then 
                            
                            nPoints.[1, scaling, centerIdx] <- acc'
                        else getNPointsRight (acc'+1) (counter+1) mzData
                else 
                     nPoints.[1, scaling, centerIdx] <- acc'
            getNPointsRight 0 centerIdx mzData    
                
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

        /// Helperfunction to get mz back of Columnnumber
        let convertColToMz (mzData: float []) (col: int) =
            let mapIndex = col / 2
            if ( col % 2 = 1)
                then (mzData.[mapIndex] + mzData.[mapIndex+1]) / 2.0
            else 
                mzData.[mapIndex] 

        ///
        let getScales nScales (scalings:float []) spacing_perc windowMaxLow windowMaxHigh (mzData:float[])  (intensityData: float[]) (xSpacingAvg:float[]) (nPoints: int [,,]) (xspaced:float[]) =
            for mzi = 1 to mzData.Length-2 do
                let windowLow' = if mzi < windowMaxLow then 0 else mzi - windowMaxLow
                let windowHigh' = if mzi >= mzData.Length - windowMaxHigh then mzData.Length - 1 else mzi + windowMaxHigh
                let nTot = windowHigh' - windowLow' |> float
                xSpacingAvg.[mzi] <- ((Care.accumulate windowLow' windowHigh' 0. (+) xspaced) / nTot)
            //////////////////////////////////////////////////////////////////////////
            //Step 2. Estimate the spacing threshold in bins
            let mutable windowSize = 150
            if (windowSize > xSpacingAvg.Length-1) then
                windowSize <- (xSpacingAvg.Length-1) / 2
            let hf_window = windowSize / 2
            windowSize <- 2 * hf_window
            let nSpacingBins = ((xSpacingAvg.Length-1) / (windowSize + 1))
            let spacings = Array.create (nSpacingBins+1) 0.0
            for i = 0 to nSpacingBins-1 do
                let windowLow = i * windowSize
                let mutable windowHigh = windowLow + windowSize //not inclusive
                if i = nSpacingBins-1 
                    then windowHigh <- xSpacingAvg.Length
                let nTot = (windowHigh - windowLow)
                let unsortedData = Array.create nTot 0.0 //nTot ist = windowsize, es sei den die Distanz von windowlow zum array Ende ist größer als windowsize
                for j = windowLow to windowHigh-1 do
                    unsortedData.[j-windowLow] <- xSpacingAvg.[j] 
                let sortedData = unsortedData |> Array.sortDescending 
                let mutable spacing = Care.scoreAtPercentile spacing_perc nTot sortedData  //calcs Noisethreshold in bin i                 
                spacings.[i] <- spacing
            //
            let maxSpacingToAvgSpacing = 1.
            for mzi = 1 to mzData.Length-2 do
                let mutable scalesToInclude = nScales    
                if intensityData.[mzi] = 1. ||intensityData.[mzi] < (0.75*intensityData.[mzi-1]) || intensityData.[mzi] < 0.75*intensityData.[mzi+1] then 
                        scalesToInclude <- 1
                let spacingBin = 
                    if  nSpacingBins = 0 then 0
                    else
                        let tmp = mzi/windowSize
                        match tmp with
                        | tmp when tmp > nSpacingBins-1 -> nSpacingBins-1 
                        | _ -> tmp
                let spacingToAvgSpacing = xSpacingAvg.[mzi] / spacings.[spacingBin]                
                if spacingToAvgSpacing > maxSpacingToAvgSpacing then
                        scalesToInclude <- 1    
                // figure out the number of wavelet points you'll need to sample for each m/z point
                for i = 0 to scalesToInclude-1 do 
                    let maxMZwindow = xSpacingAvg.[mzi] * scalings.[i] *  3.0 
                    getNpointsX mzi i maxMZwindow nPoints mzData

        ///
        let calcCorrWaveletAndDataMatrix nScales paddingPoints (mzDataPadded:float[]) (intensityDataPadded:float[]) (mzData:float[]) (intensityData: float[]) (waveletData:float[]) (nPoints: int[,,]) (xSpacingAvg: float []) (scalings: float []) (corrMatrix: float [,]) = 
            for i = 0 to nScales-1 do 
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

                        if targetCol < mzData.Length-1 then //falls j = mzData.Length.1 dann ist man am ende des m/z Arrays angekommen und die corrMatrix ist fertig            // falls nicht : Dieser Vorgang wird wiederholt unzwar wird dazu ein waveletdataarray erzeugt, der auf den Punkten zwischen den m/z werden basiert.
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

        ///
        let getSNRFilteredPeakLines noise_perc mzTol nScales mzData (allLines:Collections.Generic.List<RidgeLine>) (corrMatrix: float[,]) =   ///(snrs:Collections.Generic.List<float>)                                         
            let minSnr = 1.
            let corrMatrixLength = corrMatrix.[0,*].Length

            /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // step 1: find maxima in each column (row = scales, columns = m/z)
            /// contains scaling with the highest correlationvalue for each Column
            let colMaxes = Array.create (corrMatrixLength) 0 
            for i = 0 to corrMatrixLength-1 do 
                
                let mutable corrMax = 0.0
                
                for j = 0 to nScales-1 do 
                
                    if corrMatrix.[j,i] > corrMax then
                        corrMax <- corrMatrix.[j,i]
                        colMaxes.[i] <- j //Eintrag des scalings mit der höchsten correlation

            /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // step 2, setup bins of 300 points and calculate noise threshold within each bin
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
                let nTot = windowHigh - windowLow 
                
                let unsortedData = Array.create nTot 0.0 //nTot ist = windowsize, es sei den die Distanz von windowlow zum array Ende ist größer als windowsize
                for j = windowLow to windowHigh - 1 do
                    unsortedData.[j-windowLow] <- corrMatrix.[0, j] // first row of correlation matrix
                
                let sortedData = unsortedData |> Array.sort
                let mutable noise = Care.scoreAtPercentile noise_perc nTot sortedData  //calcs Noisethreshold in bin i 
                if noise < 1.0 then noise <- 1.0
                noises.[i] <- noise
            let interpolatedXpoints = Array.create corrMatrixLength 0.0 
            for i = 0 to corrMatrixLength-1 do
                let interpolatedpointOfI = convertColToMz mzData i 
                interpolatedXpoints.[i] <- interpolatedpointOfI

            /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // step 3, find local maxima that are separated by at least mzTol_
            let findLocalMaxima i = 
                let mzCol = convertColToMz mzData i //mz Value of Column
                // get the indices for the lower and upper bounds   
                let lowBound  = Care.getColLowBound interpolatedXpoints i mzTol
                let highBound = Care.getColHighBound interpolatedXpoints i mzTol
                // Now all m/z Values in the space between lowBound and highBound get analyzed and 
                // the Column of the m/z with the  maxCorr is identified
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
                        elif maxCorr > corrPrev then 
                            allLines.[allLines.Count-1] <- createRidgeLine maxCol colMaxes.[maxCol]
                    else 
                        let newLine = createRidgeLine maxCol colMaxes.[maxCol]
                        allLines.Add newLine

            let rec findLocalMaximaLoop i (corrMatrix: float [,]) = 
                if i = corrMatrixLength-3 then 
                    findLocalMaxima i
                else                  
                        findLocalMaxima i
                        let correlationVal = corrMatrix.[colMaxes.[i],i]
                        if  correlationVal < corrMatrix.[colMaxes.[i-1],i-1] ||
                            correlationVal < corrMatrix.[colMaxes.[i-2],i-2] ||
                            correlationVal < corrMatrix.[colMaxes.[i+1],i+1] ||
                            correlationVal < corrMatrix.[colMaxes.[i+2],i+2] then 
                                findLocalMaximaLoop (i+1) corrMatrix
                        else 
                            findLocalMaxima i
                            findLocalMaximaLoop (i+1) corrMatrix
            findLocalMaximaLoop 2 corrMatrix 

        /// 
        let refinePeaks mzTol (scalings: float[]) (mzData:float []) (intensityData:float [])  (allLines: ResizeArray<RidgeLine>) (xSpacingAvg:float []) =          
            let xPeakValues = ResizeArray<float>()
            let yPeakValues = ResizeArray<float>()
            if allLines.Count = 0 then 
                let finalX = xPeakValues |> Seq.toArray 
                let finalY = yPeakValues |> Seq.toArray
                finalX, finalY 
            else
                for i = 1 to allLines.Count-1 do
                    let mzCol = convertColToMz mzData allLines.[i].Col 
                    let mzColIdx = allLines.[i].Col  / 2
                    let row = allLines.[i].Row
                    let currentScaling = scalings.[row]
                    let offset = 
                        let offsetTmp = currentScaling * xSpacingAvg.[int (allLines.[i].Col / 2)]
                        if offsetTmp > mzTol then
                            mzTol
                        else 
                            offsetTmp 
                    let startFittingPoint = Care.getColLowBound mzData mzColIdx offset                   
                    let endFittingPoint = Care.getColHighBound mzData mzColIdx offset
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

                    if i>1 && maxIntensityMZ <> xPeakValues.[xPeakValues.Count-1] then //0.1 //(maxIntensityMZ - xPeakValues.[xPeakValues.Count-1] ) <> mzTol
                            xPeakValues.Add maxIntensityMZ
                            yPeakValues.Add maxIntensity
                    elif i=1 then
                            xPeakValues.Add maxIntensityMZ
                            yPeakValues.Add maxIntensity
                //
                let finalX = xPeakValues |> Seq.toArray 
                let finalY = yPeakValues |> Seq.toArray

                finalX, finalY                
                
        /// Returns a MzIntensityArray that containing the spectral centroids of the input spectra. 
        let toCentroid mzTol spacing_perc snrs_perc (mzData: float []) (intensityData: float [])  =
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
            let xSpacing = Care.createXspacing mzData 

            // Calculation of the average mz spacing of each mz value. This information is used to determine
            // the number of mz values for each individual mz value that are included in the computation of the individual
            // correlation
            let xSpacingAvg = Array.zeroCreate (mzData.Length) 
            let nPoints = Array3D.create 2 nScales mzData.Length 0
            getScales nScales scalings spacing_perc windowMaxLow windowMaxHigh mzData intensityData xSpacingAvg nPoints xSpacing    
            
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
             
            //
            getSNRFilteredPeakLines snrs_perc mzTol nScales mzData allLines corrMatrix

//            // 
            refinePeaks mzTol scalings mzData intensityData allLines xSpacingAvg 

    module SecondDerivative =
    
        let toCentroid windowSize mzTol noise_perc (mzData:float []) (intensityData:float []) =
            if mzData.Length < 3 || intensityData.Length < 3 then [||]
            else
            // Step 1: Calculate negative snd derivative of the intensity Data
            let negSndDev = 
                Filtering.savitzky_golay windowSize 3 2 5 intensityData 
                |> Array.ofSeq
                |> Array.map (fun x -> x * -1.)

            // Step 2: Calculate noise threshold in snd derivative space; if intensity is negative then 0. is set as a noise level
            let minSnr = 1.
            let nNoiseBins = negSndDev.Length / windowSize + 1 //number of NoiseBins
            let noises = Array.create nNoiseBins 0.0

            for i = 0 to nNoiseBins-1 do
                let windowLow = i * windowSize  // inclusive
                let mutable windowHigh = windowLow + windowSize 
                
                if i = nNoiseBins-1 
                    then windowHigh <- negSndDev.Length
                
                let nTot = windowHigh - windowLow 

                let unsortedData = Array.create nTot 0.0 //nTot ist = windowsize, es sei den die Distanz von windowlow zum array Ende ist größer als windowsize
                
                for j = windowLow to windowHigh - 1 do
                    if negSndDev.[j] > 0. then
                        unsortedData.[j-windowLow] <- negSndDev.[j]
                    else
                        unsortedData.[j-windowLow] <- 0. 
                    
                let sortedData = unsortedData |> Array.sort
                let mutable noise = Care.scoreAtPercentile noise_perc nTot sortedData  //calcs Noisethreshold in bin i 
                if noise < 1.0 then noise <- 1.0
                noises.[i] <- noise
    
            // Step 4: find maxima above the noise level
            let filteredPeaks =
                let filteredPeaks = ResizeArray<(float*float)*int>()
                negSndDev
                |> (Care.localMaximaIdx 0. mzData) 
                |> Array.iter (fun i -> 
                                let noiseBin = 
                                    let tmp = i/windowSize
                                    match tmp with
                                    | tmp when tmp > nNoiseBins-1 -> nNoiseBins-1 
                                    | _ -> tmp
                                let snr = intensityData.[i] / noises.[noiseBin]
                                if snr > minSnr then     
                                    filteredPeaks.Add ((mzData.[i],intensityData.[i]), i)
                               )
                filteredPeaks
                |> Array.ofSeq

            // Step 5: refine Peak positions to real maxima
            let refinedCentroids = 
                let refCentroids = ResizeArray<float*float>()
                filteredPeaks
                |> Array.iteri (fun idx ((mz,intensity),i) -> 
                                    let startFittingPoint  = Care.getColLowBound mzData i (mzTol/2.)
                                    let endFittingPoint    = Care.getColHighBound mzData i (mzTol/2.)
                                    let mutable maxIntensity = 0.0 
                                    let mutable intensityAccumulator = 0.0 
                                    let mutable maxIntensityMZ = 0.0
                                    for j = startFittingPoint to endFittingPoint do 
                                        intensityAccumulator <- intensityAccumulator + intensityData.[j] //is never used, maybe important to determine the spectra intensity?
                                        if intensityData.[j] >= maxIntensity
                                            then
                                                maxIntensity   <- intensityData.[j]
                                                maxIntensityMZ <- mzData.[j] 
                                    let refCentLength = refCentroids.Count    
                                    if refCentLength = 0 then 
                                        refCentroids.Add (maxIntensityMZ, maxIntensity)
                                    elif fst refCentroids.[refCentLength-1] - maxIntensityMZ < mzTol && snd refCentroids.[refCentLength-1] < maxIntensity then
                                         refCentroids.[refCentLength-1] <- (maxIntensityMZ, maxIntensity)
                                    else
                                         refCentroids.Add (maxIntensityMZ, maxIntensity) 
                               )
        
                refCentroids
                |> Array.ofSeq
            refinedCentroids

    /// Returns mzIntensityArray after noise reduction 
    let filterByIntensitySNR perc minSnr (mzData: float []) (intensityData:float [])  = 
        let noise = Care.scoreAtPercentile perc (intensityData.Length-1) intensityData 
        let filteredMz             = new ResizeArray<float>() 
        let filteredIntensityArray = new ResizeArray<float>()
        for i=0 to intensityData.Length-1 do
            let snr = intensityData.[i]/noise
            if snr > minSnr then
                filteredMz.Add mzData.[i]
                filteredIntensityArray.Add intensityData.[i]
        filteredMz |> Seq.toArray, filteredIntensityArray |> Seq.toArray

    /// Returns mzIntensityArray consisting of centroided Peaks. 
    let toCentroid centroidF (mzData:float[]) (intensityData:float[]) =
        if mzData.Length < 3  then 
            [||], [||]
        else
            centroidF mzData intensityData

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
        | None  -> mzData.Length-1 
    
    // Returns mzIntensityArray consisting of centroided Peaks. 
    let windowToCentroidBy centroidF (mzData:float[]) (intensityData:float[]) windowWidth centerMass =
        if mzData.Length > 2 then
            let lowerIdx = lowerIdxBy mzData windowWidth centerMass
            let upperIdx = upperIdxBy mzData windowWidth centerMass
            windowToCentroid centroidF mzData intensityData lowerIdx upperIdx
        else [||], [||]



