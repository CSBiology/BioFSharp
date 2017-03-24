namespace BioFSharp.Mz

module Quantification =
    
    open System
    open BioFSharp
    open FSharp.Care 

    open Peaks
    open PeakArray
    open PeakList
    open SignalDetection
    open SequestLike
    open MathNet.Numerics.LinearAlgebra.Double

    module Integration = 

        /// Returns the estimated area beneath the data using the trapezoidal rule. Requires uniform distributed datapoints. 
        let trapezEstAreaOfUniform (xData: float[]) (yData: float[]) =
            if xData.Length <> yData.Length then
                failwith "Both input collections must be of the same size"
            // lower border of area estimation
            let a = xData.[0]
            // upper border of area estimation
            let b = xData.[xData.Length-1]

            let mutable trapezSum = 0.0
    
            for i = 1 to xData.Length-2 do
                trapezSum <- trapezSum + 2. * yData.[i]
            trapezSum <- trapezSum + yData.[0]
            trapezSum <- trapezSum + yData.[xData.Length-1]
            let area = ( (b-a) / (2. * float xData.Length) ) * trapezSum
            area    
         
        /// Returns the estimated area beneath the data using the trapezoidal rule.
        let trapezEstAreaOf (xData: float[]) (yData: float[]) =
            if xData.Length <> yData.Length then
                failwith "Both input collections must be of the same size"
            let mutable trapezSum = 0.0
    
            for i = 0 to xData.Length-2 do
                trapezSum <-trapezSum + (xData.[i+1] - xData.[i]) * (yData.[i+1] + yData.[i])

            let area = 0.5 * trapezSum
            area    

    module GaussEstimation =
        ///
        type GaussParams = {
            Amplitude: float
            MeanX    : float
            STD      : float
            FWHM     : float
            }

        /// 
        let createGausParams amplitude meanX std fwhm = {
            Amplitude=amplitude; MeanX=meanX; STD=std; FWHM=fwhm }

        /// Returns the value of the standard deviation
        let toSTD fwhm = fwhm / ( 2. * sqrt(2. * log 2.) )

        /// Returns the full width at half maximum  
        let toFWHM std = 2. * sqrt(2. * log 2.) * std

        /// Returns the yValue of a gauss function at a given position x.
        let gaussFunc amplitude meanX std x = 
            amplitude * exp((-1.)*((((x-meanX)**2.)/(2.*std**2.))))
  
        /// Returns the yValue of a exponentially modified gauss function at a given position.
        /// The function parameter tau represents the exponential relaxation time which is the inverse of the exponential decay parameter.
        let expModGaussFunc amplitude meanX std tau x = 
            ((amplitude*std)/tau) * sqrt(System.Math.PI/2.) * exp(1./2. * ((std/tau)**2.) - ((x-meanX)/tau)) * MathNet.Numerics.SpecialFunctions.Erfc((1./sqrt(2.)) * ((std/tau)-((x-meanX)/std)))

        /// Estimates the Parameters of a Gaussian function
        /// Warning: This method is sensitive to noisy data. If the noise level of the input parameters is high, smoothing of 
        /// the data is strongly recommended. 
        let caruanaAlgorithm (mzData:float []) (intensityData:float []) =
            if mzData.Length < 3 || intensityData.Length < 3 then None 
            else 
            let logTransIntensityData = 
                intensityData
                |> Array.map log
            let polCoeff = MathNet.Numerics.Fit.Polynomial(mzData,logTransIntensityData,2)
            // f(x) = a1 + a2 * x + a3 * x**2
            let a = polCoeff.[0]
            let b = polCoeff.[1]
            let c = polCoeff.[2]
            let amplitude = exp(a-((b**2.)/(4.*c)))
            let meanX = -b/(2.*c) 
            let fwhm  = sqrt(-1./(2.*c)) * sqrt(2.*log(2.))*2.
            let std   = toSTD fwhm
            Some (createGausParams amplitude meanX std fwhm)

    module Fitting = 

        ///
        let standardErrorOfPrediction dOF (model:float []) (data:float [])  =
            let n = data.Length-1 |> float 
            match n with
            | x when x > dOF -> 
                let sumOfResSq = Array.fold2 (fun acc yReal yPred  -> acc + ((yPred-yReal)**2.) ) 0.0  data model
                sqrt( (sumOfResSq / (n - dOF)))
            | _             -> -1.

        ///
        type Model = 
            {
            DescFuncBody    : string
            ParameterNames  : string []
            ///originally GetValue; contains function body
            GetFunctionValue    : (DenseVector -> float -> float)
            ///Gradient: Vector of partial derivations of function body
            GetGradientValue        : (DenseVector -> DenseVector -> float -> DenseVector )
            }
        
        ///
        let createModel descFuncBody parameterNames getFunctionValue getGradientValue = {
            DescFuncBody = descFuncBody; ParameterNames = parameterNames; GetFunctionValue = getFunctionValue; GetGradientValue=getGradientValue }

        ///
        type SolverOptions = {
            MinimumDeltaValue: float
            MinimumDeltaParameters: float
            MaximumIterations: int
            InitialParamGuess: float []
            }

        ///
        let createSolverOption minimumDeltaValue minimumDeltaParameters maximumIterations initialParamGuess = {
            MinimumDeltaValue = minimumDeltaValue; MinimumDeltaParameters = minimumDeltaParameters; MaximumIterations = maximumIterations; InitialParamGuess=initialParamGuess}

        /// Returns the residual sum of squares (RSS) as a measure of discrepancy between the data and the used estimation model.
        let getRSS (model: Model) dataPointCount (xData: float[]) (yData: float []) (paramVector: DenseVector) =
            let sumOfSquaredResiduals =
                Array.fold2 (fun acc xValue yValue ->  
                                let yValueEst = model.GetFunctionValue paramVector xValue
                                acc + ((yValueEst - yValue) **2.)
                            ) 0.0 xData yData
        
            sumOfSquaredResiduals * 0.5

        /// Returns the Jacobian matrix of a given model and discrete parameters.
        let getJacobianOf (model: Model) dataPointCount (xData: float[]) (paramVector: DenseVector) (jacobian: Matrix ) =
            // Nr. of Parameters
            let paramCount = paramVector.Count
            // populate Jacobian Matrix
            for i = 0 to dataPointCount-1 do 
                let gradient = new DenseVector(paramCount)
                model.GetGradientValue paramVector gradient xData.[i] |> ignore
                jacobian.SetRow(i,gradient) 
          
        /// Returns the residual vector, each row i contains the difference between the yEst_i and the yData_i. 
        let getResidualVector (model: Model) dataPointCount (xData: float[]) (yData: float []) (paramVector: DenseVector) (residualVector: DenseVector) = 
            for i = 0 to dataPointCount-1 do 
                let yValueEst = model.GetFunctionValue paramVector xData.[i]
                residualVector.[i] <- (yValueEst - yData.[i])

        /// Returns true if convergence criteria are met or a user defined number of iiterations has been carried out
        let shouldTerminate (currentValueRSS: float) (newValueRSS: float) (iterationCount:int) (currentParamGuess:DenseVector) 
                (newParamGuess:DenseVector) (solverOptions: SolverOptions)  = 
            if abs (newValueRSS-currentValueRSS) <= solverOptions.MinimumDeltaValue ||
               newParamGuess.Subtract(currentParamGuess).Norm(2.0) <= solverOptions.MinimumDeltaParameters ||
               iterationCount >= solverOptions.MaximumIterations then
                false
            else 
                true       

        /// Returns a parameter vector as a possible solution for linear least square based nonlinear fitting of a given dataset (xData, yData) with a given 
        /// model function. 
        let gaussNewtonSolver (model: Model) (solverOptions: SolverOptions) (xData: float[]) (yData: float []) (paramsAtIteration: ResizeArray<_>)  = 

            let mutable anotherIteration = true
            /// Number of Parameters of modelFunction
            let paramCount = solverOptions.InitialParamGuess.Length
            let dataPointCount = xData.Length

            let currentParamGuess = new DenseVector(solverOptions.InitialParamGuess)
            let newParamGuess     = new DenseVector(paramCount)

            let mutable currentValueRSS = 0.0
            let mutable newValueRSS = 0.0
            ///
            currentValueRSS <- getRSS model dataPointCount xData yData currentParamGuess

            while (anotherIteration = true) do 

                let jacobian       = new DenseMatrix(dataPointCount, paramCount)
                let residualVector = new DenseVector(dataPointCount)
                /// 
                getJacobianOf model dataPointCount xData currentParamGuess jacobian |> ignore
            
                ///
                getResidualVector model dataPointCount xData yData currentParamGuess residualVector |> ignore  
                /// 
                let step = jacobian.Transpose().Multiply(jacobian).Cholesky().Solve(jacobian.Transpose().Multiply(residualVector))
        
                ///
                currentParamGuess.Subtract(step, newParamGuess)

                /// 
                newValueRSS <- getRSS model dataPointCount xData yData newParamGuess

                ///
                paramsAtIteration.Add(newParamGuess) |> ignore
        
                /// 
                anotherIteration <- shouldTerminate currentValueRSS newValueRSS paramsAtIteration.Count currentParamGuess newParamGuess solverOptions

                /// 
                newParamGuess.CopyTo currentParamGuess

                /// 
                currentValueRSS <- newValueRSS

            paramsAtIteration.[paramsAtIteration.Count-1]

        /// Returns a parameter vector as a possible solution for linear least square based nonlinear fitting of a given dataset (xData, yData) with a given 
        /// model function. 
        let levenbergMarquardtSolver (model: Model) (solverOptions: SolverOptions) (xData: float[]) (yData: float []) (paramsAtIteration: ResizeArray<_>)  = 
            
            let lambdaInitial = 0.001
            let lambdaFactor  = 10.0

            let mutable anotherIteration = true
            /// Number of Parameters of modelFunction
            let paramCount = solverOptions.InitialParamGuess.Length
            let dataPointCount = xData.Length

            let mutable lambda = lambdaInitial

            let currentParamGuess = new DenseVector(solverOptions.InitialParamGuess)
            let newParamGuess     = new DenseVector(paramCount)

            let mutable currentValueRSS = 0.0
            let mutable newValueRSS = 0.0
            
            ///
            currentValueRSS <- getRSS model dataPointCount xData yData currentParamGuess

            while (anotherIteration = true) do 

                let jacobian       = new DenseMatrix(dataPointCount, paramCount)
                let residualVector = new DenseVector(dataPointCount)
                /// 
                getJacobianOf model dataPointCount xData currentParamGuess jacobian 
            
                ///
                getResidualVector model dataPointCount xData yData currentParamGuess residualVector 

                /// 
                let hessian  = jacobian.Transpose().Multiply(jacobian)
        
                ///
                let diagonal = new DiagonalMatrix(paramCount, paramCount, hessian.Diagonal().ToArray() )

                ///
                let step     = (hessian.Add(diagonal.Multiply(lambda))).Cholesky().Solve(jacobian.Transpose().Multiply(residualVector))

                ///
                currentParamGuess.Subtract(step, newParamGuess)

                /// 
                newValueRSS <- getRSS model dataPointCount xData yData newParamGuess

                ///
                paramsAtIteration.Add(newParamGuess)
        
                /// 
                anotherIteration <- shouldTerminate currentValueRSS newValueRSS paramsAtIteration.Count currentParamGuess newParamGuess solverOptions

                ///

                if newValueRSS < currentValueRSS then
                    /// 
                    newParamGuess.CopyTo currentParamGuess

                    /// 
                    currentValueRSS <- newValueRSS

                else
                    lambda <- lambda * lambdaFactor

            paramsAtIteration.[paramsAtIteration.Count-1]


        module Table = 
        
        /////////////////////////
        /// Line  

            let lineModel = {
                DescFuncBody= "y = a * x + b"
                ParameterNames= [|"a";"b"|]
                GetFunctionValue = (fun (parameterVector:DenseVector) xValue -> (parameterVector.[0] * xValue) + parameterVector.[1])
                GetGradientValue = (fun (parameterVector:DenseVector) (gradientVector: DenseVector) xValue -> 
                                    gradientVector.[0] <- xValue  
                                    gradientVector.[1] <- 1.0 
                                    gradientVector)
                }


            let lineSolverOptions = {
                MinimumDeltaValue       = 0.00001
                MinimumDeltaParameters  = 0.00001  
                MaximumIterations       = 1000
                InitialParamGuess       = [|0.14;1.|]
                }
                
        /////////////////////////
        /// Parabola

            let parabolaModel = {
                DescFuncBody= "y = a * x^2 + b * x + c"
                ParameterNames= [|"a";"b";"c"|]
                GetFunctionValue = (fun (parameterVector:DenseVector) xValue -> (parameterVector.[0] * xValue**2.) + parameterVector.[1] * xValue + parameterVector.[2])
                GetGradientValue = (fun (parameterVector:DenseVector) (gradientVector: DenseVector) xValue -> 
                                    gradientVector.[0] <- xValue**2.  
                                    gradientVector.[1] <- xValue 
                                    gradientVector.[2] <- 1.0
                                    gradientVector)
                }

            let parabolaSolverOptions = {
                MinimumDeltaValue       = 0.00001
                MinimumDeltaParameters  = 0.00001  
                MaximumIterations       = 1000
                InitialParamGuess       = [|0.14;1.;10.|]
                }

        /////////////////////////
        /// Gaussian function
            let gaussModel = {
                DescFuncBody= "y = amp * exp( -1. * ( ( ( (x-meanX)**2. ) / (2.*std**2.)) ) )"
                ParameterNames= [|"amp";"meanX";"std"|]
                GetFunctionValue = (fun (parameterVector:DenseVector) xValue -> parameterVector.[0] * exp( (-1.) * ( ( ( (xValue-parameterVector.[1])**2. ) / (2.*parameterVector.[2]**2.)) ) ))
                GetGradientValue = (fun (parameterVector:DenseVector) (gradientVector: DenseVector) xValue -> 
                                    gradientVector.[0] <- exp( (-1.) * ( ( ( (xValue-parameterVector.[1])**2. ) / (2.*parameterVector.[2]**2.)) ) )
                                    gradientVector.[1] <- ( (parameterVector.[0] * (xValue-parameterVector.[1]) * exp( (-1.) * ( ( ( (xValue-parameterVector.[1])**2. ) / (2.*parameterVector.[2]**2.)) ) ) ) ) / (parameterVector.[2]**2.)
                                    gradientVector.[2] <- ( (parameterVector.[0] * ((xValue-parameterVector.[1])**2.) * exp( (-1.) * ( ( ( (xValue-parameterVector.[1])**2. ) / (2.*parameterVector.[2]**2.)) ) ) ) ) / (parameterVector.[2]**3.)
                                    gradientVector)
                }

            let gaussSolverOptions = {
                MinimumDeltaValue       = 0.01
                MinimumDeltaParameters  = 0.01  
                MaximumIterations       = 10000
                InitialParamGuess       = [|2.;12.;1.|]
                }


        
        /////////////////////////
        /// Exponentially modified Gaussian (EMG)

            let emgModel = {
                DescFuncBody= "y =  ((amp*std)/tau) * sqrt(System.Math.PI/2.) * exp(1./2. * ((std/tau)**2.) - ((x-meanX)/tau)) * MathNet.Numerics.SpecialFunctions.Erfc((1./sqrt(2.)) * ((std/tau)-((x-meanX)/std)))"
                ParameterNames= [|"amp";"meanX";"std";"tau"|]
                GetFunctionValue = (fun (parameterVector:DenseVector) xValue ->  ((parameterVector.[0]*parameterVector.[2])/parameterVector.[3]) * sqrt(System.Math.PI/2.) * exp(1./2. * ((parameterVector.[2]/parameterVector.[3])**2.) - ((xValue-parameterVector.[1])/parameterVector.[3])) * MathNet.Numerics.SpecialFunctions.Erfc((1./sqrt(2.)) * ((parameterVector.[2]/parameterVector.[3])-((xValue-parameterVector.[1])/parameterVector.[2]))) )
                GetGradientValue = (fun (parameterVector:DenseVector) (gradientVector: DenseVector) xValue -> 
                                    gradientVector.[0] <- (1./parameterVector.[3]) * 1.25331 * parameterVector.[2] * exp( (0.5*(parameterVector.[2]**2.) / (parameterVector.[3]**2.) ) - ( (xValue-parameterVector.[1]) / parameterVector.[3]) ) * MathNet.Numerics.SpecialFunctions.Erfc(0.707107 * ( (parameterVector.[2] / parameterVector.[3] ) - ( (xValue-parameterVector.[1]) / parameterVector.[2]) ) )
                                //0 passt
                                    gradientVector.[1] <-  ( (1./parameterVector.[3]**2.) *  1.25331 * parameterVector.[0] * parameterVector.[2] * exp( (0.5*(parameterVector.[2]**2.) / (parameterVector.[3]**2.) ) - ( (xValue-parameterVector.[1]) / parameterVector.[3]) ) * MathNet.Numerics.SpecialFunctions.Erfc(0.707107 * ( (parameterVector.[2] / parameterVector.[3] ) - ( (xValue-parameterVector.[1]) / parameterVector.[2]) ) ) )  
                                                           - ( (1./parameterVector.[3]) * parameterVector.[0] * exp( ( (0.5 * parameterVector.[2]**2.) / parameterVector.[3]**2. ) - (0.5 * (((parameterVector.[2]/parameterVector.[3]) - ( (xValue-parameterVector.[1]) / parameterVector.[2] ) )**2.) ) - ( (xValue-parameterVector.[1]) / parameterVector.[3] ) ) )
                                //1 passt
                                    gradientVector.[2] <-     ( (1./ (parameterVector.[3]))                                 * 1.25331 * parameterVector.[0] * exp( (0.5*(parameterVector.[2]**2.) / (parameterVector.[3]**2.) ) - ( (xValue-parameterVector.[1]) / parameterVector.[3]) ) * MathNet.Numerics.SpecialFunctions.Erfc(0.707107 * ( (parameterVector.[2] / parameterVector.[3] ) - ( (xValue-parameterVector.[1]) / parameterVector.[2]) ) )) 
                                                            + ( (1./ (parameterVector.[3]**3.)) * (parameterVector.[2]**2.) * 1.25331 * parameterVector.[0] * exp( (0.5*(parameterVector.[2]**2.) / (parameterVector.[3]**2.) ) - ( (xValue-parameterVector.[1]) / parameterVector.[3]) ) * MathNet.Numerics.SpecialFunctions.Erfc(0.707107 * ( (parameterVector.[2] / parameterVector.[3] ) - ( (xValue-parameterVector.[1]) / parameterVector.[2]) ) ))
                                                            - ( (1./ (parameterVector.[3]))  )  * (parameterVector.[2])     * 1.00000 * parameterVector.[0] * exp( (-0.5*( (parameterVector.[2] / parameterVector.[3])  - ( (xValue-parameterVector.[1]) / parameterVector.[2]) )**2. ) - ((xValue-parameterVector.[1]) / parameterVector.[3]) + (0.5*(parameterVector.[2]**2.) / (parameterVector.[3]**2.) ) ) * ( ((xValue-parameterVector.[1]) / (parameterVector.[2]**2.) ) + 1./parameterVector.[3] )    
                        
                                    gradientVector.[3] <-  - ( (1./ (parameterVector.[3]**2.))  * (parameterVector.[2])     * 1.25331 * parameterVector.[0] * exp( (0.5*(parameterVector.[2]**2.) / (parameterVector.[3]**2.) ) - ( (xValue-parameterVector.[1]) / parameterVector.[3]) ) * MathNet.Numerics.SpecialFunctions.Erfc(0.707107 * ( (parameterVector.[2] / parameterVector.[3] ) - ( (xValue-parameterVector.[1]) / parameterVector.[2]) ) )) 
                                               
                                                           + ( (1./ (parameterVector.[3]))      * (parameterVector.[2])     * 1.25331 * parameterVector.[0] * exp( (0.5*(parameterVector.[2]**2.) / (parameterVector.[3]**2.) ) - ( (xValue-parameterVector.[1]) / parameterVector.[3]) ) * MathNet.Numerics.SpecialFunctions.Erfc(0.707107 * ( (parameterVector.[2] / parameterVector.[3] ) - ( (xValue-parameterVector.[1]) / parameterVector.[2]) ) )) 
                                                           * ( ((xValue-parameterVector.[1]) / parameterVector.[3]**2. ) - ((parameterVector.[2]**2.) / (parameterVector.[3]**3.)) )
                                               
                                                           + ( (1./ (parameterVector.[3]**3.))  )   * (parameterVector.[2]**2.)     * 1.00000 * parameterVector.[0] * exp( (-0.5*( (parameterVector.[2] / parameterVector.[3])  - ( (xValue-parameterVector.[1]) / parameterVector.[2])**2. ) ) - ((xValue-parameterVector.[1]) / parameterVector.[3]) + (0.5*(parameterVector.[2]**2.) / (parameterVector.[3]**2.) ) ) 
                                    gradientVector ) 
                }


            let emgSolverOptions = {
                MinimumDeltaValue       = 0.00001
                MinimumDeltaParameters  = 0.00001  
                MaximumIterations       = 10000
                //[|"amp";"meanX";"std";"tau"|]
                InitialParamGuess       = [|3.;9.;1.;0.3|]
                }
    
        
    module MyQuant = 

        //
        let idxOfClosestLabeledPeak (labeledData: Tag<Care.Extrema,(float*float)>[]) (labelV:Care.Extrema) xValue = 
            if labeledData |> Array.isEmpty then None
            else
            labeledData  
            |> Array.mapi (fun i x -> i, x) 
            |> Array.filter (fun (i,x) -> x.Meta = labelV)
            |> fun farr -> 
                match farr with
                | farr when farr |> Array.isEmpty -> 
                    None     
                | _ ->  
                    farr 
                    |> Array.minBy (fun (idx,value) -> abs (fst value.Data - xValue) ) 
                    |> Some
            

        //
        let iterateTo step xData startIdx (test: 'a -> bool) =
            let rec loop  (xData: 'a []) currentIdx =
                if currentIdx <= 0 then None
                elif currentIdx >= xData.Length-1 then None
                else                                              
                    match test xData.[currentIdx] with 
                    | x when x = true -> Some currentIdx   
                    | _               -> loop xData (currentIdx+step) 
            loop xData (startIdx+step) 

        //
        type FitBothModels = 
            | True  
            | False  

        //
        type QuantificationResult = {
            FitBothModels               : FitBothModels
            SelectedModel               : Fitting.Model
            Area                        : float
            StandardErrorOfPrediction   : float
            //If negative: MS2 recorded prior to selected peak apex, if positive: MS2 recorded after selected peak
            DeltaScanTimePeakApex       : float
            PeakApexIntensity           : float
            MaxIntensityInXIC           : float 
            }

        //
        let createQuantificationResult fitBothModels selectedModel area standardErrorOfPrediction deltaScanTimePeakApex peakApexIntensity maxIntensityInXIC = {
            FitBothModels = fitBothModels; SelectedModel = selectedModel ;Area = area ; StandardErrorOfPrediction = standardErrorOfPrediction;
                DeltaScanTimePeakApex =deltaScanTimePeakApex; PeakApexIntensity=peakApexIntensity; MaxIntensityInXIC=maxIntensityInXIC}

        ///
        let createEMGSolverOption = Fitting.createSolverOption 0.001 0.001 10000
    
        ///
        let createGaussSolverOption = Fitting.createSolverOption 0.0001 0.0001 10000

        ///
        let private findRightFittingIdx (xAndYData: (float*float) []) (labeledSndDevData: Tag<Care.Extrema,(float*float)> []) (closestPeakIdx: int) (closestRightLiftOffIdx: int option) =
            let rec loopF (labeledSndDevData: Tag<Care.Extrema,(float*float)> []) (currentIdx: int) (kLiftOffs: int) (hasRightPeak:bool) = 
                if currentIdx = labeledSndDevData.Length-1 then 
                    currentIdx, kLiftOffs, hasRightPeak
                else
                    match labeledSndDevData.[currentIdx] with
                    | x when x.Meta = Care.Extrema.Positive ->  
                        currentIdx, kLiftOffs, true
                    | x when x.Meta = Care.Extrema.Negative ->
                        loopF labeledSndDevData (currentIdx+1) (kLiftOffs+1) hasRightPeak
                    |_ -> 
                        loopF labeledSndDevData (currentIdx+1) (kLiftOffs) hasRightPeak
            match closestRightLiftOffIdx with 
            | Some x -> 
                let (currentIdx, kLiftOffs, hasRightPeak) = loopF labeledSndDevData closestRightLiftOffIdx.Value 0 false 
                    // only one Liftoff and no flanking peak indicates a isolated peak and both models can be tested. 
                if kLiftOffs = 1 && hasRightPeak = false then
                    FitBothModels.True, 
                    match iterateTo (+1) xAndYData (closestRightLiftOffIdx.Value) (fun (x:float*float) -> snd x < snd xAndYData.[closestRightLiftOffIdx.Value] || snd x > snd xAndYData.[closestRightLiftOffIdx.Value]) with 
                    | None -> xAndYData.Length-1
                    | Some x -> x            
                // only one Liftoff indicates a convoluted peak, use only Gaussian model            
                elif kLiftOffs = 1 then
                    FitBothModels.False, 
                        match iterateTo (-1) xAndYData (closestRightLiftOffIdx.Value) (fun (x:float*float) -> snd x > snd xAndYData.[closestRightLiftOffIdx.Value]) with
                        | None ->  (closestPeakIdx)+1
                        | Some x -> x
                // if more than one Liftoff between two peaks is detected, the peaks are well separated and both Models can be tested
                elif kLiftOffs > 1 then 
                    FitBothModels.True,  
                    match iterateTo (+1) xAndYData (closestRightLiftOffIdx.Value) (fun (x:float*float) -> snd x < snd xAndYData.[closestRightLiftOffIdx.Value] || snd x > snd xAndYData.[closestRightLiftOffIdx.Value]) with 
                    | None -> xAndYData.Length-1
                    | Some x -> x        
                else
                    FitBothModels.False,  
                    match iterateTo (+1) xAndYData (closestPeakIdx) (fun (x:float*float) ->  snd x < 0.5 * snd xAndYData.[closestPeakIdx]) with 
                    | None -> xAndYData.Length-1
                    | Some x -> x
            | None   -> 
                FitBothModels.False, 
                    match iterateTo (+1) xAndYData (closestPeakIdx) (fun (x:float*float) -> snd x < 0.5 * snd xAndYData.[closestPeakIdx]) with
                    | None   -> xAndYData.Length-1 
                    | Some x -> x
            
    
        /// 
        let quantify windowSizeSGfilter negYThreshold posYThreshold (scanTime: float) (xData :float []) (yData: float [])= 
            if xData.Length < 6 || yData.Length < 6 then None, 0
            else
            // Step 0: zip xData and yData
            let xAndYData = 
                Array.zip xData yData
            // Step 1: Calculate negative snd derivative of the intensity Data
            let negSndDev = 
                SignalDetection.Filtering.savitzky_golay windowSizeSGfilter 3 2 3 yData 
                |> Array.ofSeq
                |> Array.map (fun x -> x * -1.)    
            // Step 2: label data points to be local Minima or maxima
            let labeledSndDevData = 
                let labeledDataTmp = SignalDetection.Care.labelPeaks negYThreshold posYThreshold xData negSndDev
                let maxPeakIntensity = 
                    labeledDataTmp 
                    |> Array.maxBy (fun x -> x.Meta = Care.Extrema.Positive)
                labeledDataTmp 
                |> Array.map (fun x -> if x.Meta = Care.Extrema.Positive then
                                         match snd x.Data with
                                         | validIntensity when validIntensity > 0.05 * (snd maxPeakIntensity.Data) -> x                             
                                         | _                                                                       -> 
                                            {Meta=Care.Extrema.None; Data= x.Data}
                                       else x
                             )
            // Step 3: find closest Peak to MS2 scantime
            let closestPeakIdx = 
                idxOfClosestLabeledPeak labeledSndDevData SignalDetection.Care.Extrema.Positive scanTime
            // TODO with F# 4.1 replace with OK, Error Pattern
            if closestPeakIdx.IsNone then None, 1
            else
            // Step 4I: find leftLiftOffIdx
            let closestLeftLiftOffIdx =
                iterateTo (-1) labeledSndDevData (fst closestPeakIdx.Value) (fun (x:Tag<Care.Extrema,(float*float)>) -> x.Meta = Care.Extrema.Negative)
            // Step4II: find leftFittingStartIdx
            let leftfittingBorderIdx = 
                match closestLeftLiftOffIdx with 
                | None   -> 0 
                | Some x -> x+1
            // Step 5: find rightLiftOff
            let closestRightLiftOffIdx = 
                iterateTo (+1) labeledSndDevData (fst closestPeakIdx.Value) (fun (x:Tag<Care.Extrema,(float*float)>) -> x.Meta = Care.Extrema.Negative)
            // Step 6: check if another peak is present to the right side of the chosen peak, count Liftoffs points, determine Model selection     
            let mutable kLiftoffs = 0 
            let mutable hasRightPeak = false
            let (modelStatus,rightFittingBorderIdx) =
                findRightFittingIdx xAndYData labeledSndDevData (fst closestPeakIdx.Value) closestRightLiftOffIdx
            //Step 7: Create sub-array of xData and yData that is considered for subsequent fitting procedures    
            let xDataForFit = xData.[leftfittingBorderIdx.. rightFittingBorderIdx] 
            let yDataForFit = yData.[leftfittingBorderIdx.. rightFittingBorderIdx]
            //Step 8: Use Caruanas algorithm to estimate parameters height, position and Full width at half maximum of 
            //        the selected peak
            let gausParamEstCaruana = 
                GaussEstimation.caruanaAlgorithm xDataForFit yDataForFit
            // TODO with F# 4.1 replace with OK, Error Pattern
            if gausParamEstCaruana.IsNone then None,2
            else 
            let gausParamEstCaruana = gausParamEstCaruana.Value
            //Step 9: Case A: if FitBithModels = True, the peak ending can be used to estimate a possible tailing    if FitBothModels = False then the first peak of a convoluted peak pair was chosen and is subsequently used to estimate the area
            //        Case B: if FitBothModels = False then the first peak of a convoluted peak pair was chosen and is subsequently used to estimate the area
            // Case A:
            if modelStatus = FitBothModels.True then
                let modelFunction = 
                    ///
                    let gausParamA = 
                        let tmpGauss = Array.create 3 0.
                        tmpGauss.[0] <- gausParamEstCaruana.Amplitude 
                        tmpGauss.[1] <- (gausParamEstCaruana.MeanX ) 
                        tmpGauss.[2] <- gausParamEstCaruana.STD 
                        tmpGauss
            
                    ///
                    let exponentialDecayEst = 
                        let startTime = xData.[closestRightLiftOffIdx.Value]
                        let startIntensity = yData.[closestRightLiftOffIdx.Value]
                        let idxHalfIntensity = iterateTo (+1) yData closestRightLiftOffIdx.Value (fun x -> x < 0.5*startIntensity)    
                        let endTime = 
                            match idxHalfIntensity with
                            | Some x -> xData.[x]
                            | None   -> xData.[xData.Length-1]
                            
                        let decayEst = (endTime-startTime) / (log 2.)
                        decayEst
                    printfn "%f decay" exponentialDecayEst
                    ///
                    let gausParamEMG = 
                        let tmpEmg = Array.create 4 0.
                        tmpEmg.[0] <- gausParamEstCaruana.Amplitude 
                        tmpEmg.[1] <- (gausParamEstCaruana.MeanX )
                        tmpEmg.[2] <- gausParamEstCaruana.STD 
                        tmpEmg.[3] <- exponentialDecayEst
                        tmpEmg
                    ///
                    let gaussPrediction =
                        let paramConti = new ResizeArray<DenseVector>()
                        let gaussSolOptions = createGaussSolverOption gausParamA
                        ///
                        let testgauss() = 
                            try
                                let gaussParamA = Fitting.levenbergMarquardtSolver Fitting.Table.gaussModel gaussSolOptions xDataForFit yDataForFit paramConti                                   
                                let gaussYPredicted = Array.map (fun xValue -> Fitting.Table.gaussModel.GetFunctionValue gaussParamA xValue) xDataForFit
                                Some (gaussParamA, gaussYPredicted)
                            with 
                            | :? System.ArgumentException as ex  -> 
                                                                None
                        testgauss()       
                    ///
                    let emgPrediction =
                        [|exponentialDecayEst/10. .. (exponentialDecayEst/10.)+0.1 ..  exponentialDecayEst|]
                        |> Array.map (fun x -> 
                                        let paramConti = new ResizeArray<DenseVector>()
                                        let startexponentialDecayEst = exponentialDecayEst
                                        gausParamEMG.[3] <- x
                
                                        let emgSolOptions = createEMGSolverOption gausParamEMG
                                        ///
                                        let testEmg() = 
                                            try 
                                                let emgParamA = Fitting.levenbergMarquardtSolver Fitting.Table.emgModel emgSolOptions xDataForFit yDataForFit paramConti
                                                let emgYPredicted = Array.map (fun xValue -> Fitting.Table.emgModel.GetFunctionValue emgParamA xValue) xDataForFit
                                                Some (emgParamA, emgYPredicted)
                                            with 
                                            | :? System.ArgumentException as ex  -> 
                                                                                None
                                        testEmg()  
                                        )
                        |> Array.filter (fun x -> x.IsSome)
                        |> fun possibleEMGFits -> 
                            match possibleEMGFits with
                            | pEmgF when Array.isEmpty pEmgF -> None
                            | _                      ->
                                possibleEMGFits  
                                |> Array.map (fun x ->
                                                let sEoE = Fitting.standardErrorOfPrediction 4. (snd x.Value) yDataForFit  
                                                printfn "%f" sEoE
                                                x, sEoE
                                            )
                                |> Array.minBy (fun x -> snd x)
                                |> fun x -> fst x
            
                    // If both models are fittable, choose the model with smaller standard error of the estimate
                    if gaussPrediction.IsSome && emgPrediction.IsSome then
                        ///
                        let yGauss = snd gaussPrediction.Value
                        let sEoE_Gauss = Fitting.standardErrorOfPrediction 3. yGauss yDataForFit
                        ///
                        let yEMG   = snd emgPrediction.Value
                        let sEoE_EMG   = Fitting.standardErrorOfPrediction 4. yEMG yDataForFit 
                        if sEoE_Gauss > -1. && sEoE_EMG > -1. && sEoE_Gauss > sEoE_EMG then
                            Some (Fitting.Table.emgModel, emgPrediction.Value, sEoE_EMG)
                        else        
                            Some (Fitting.Table.gaussModel, gaussPrediction.Value, sEoE_Gauss)
                    elif emgPrediction.IsSome then
                        let yEMG   = snd emgPrediction.Value
                        let sEoE_EMG   = Fitting.standardErrorOfPrediction 4. yEMG yDataForFit      
                        Some (Fitting.Table.emgModel , emgPrediction.Value, sEoE_EMG)
                    elif gaussPrediction.IsSome then
                        let yGauss = snd gaussPrediction.Value
                        let sEoE_Gauss = Fitting.standardErrorOfPrediction 3. yGauss yDataForFit
                        Some (Fitting.Table.gaussModel, gaussPrediction.Value, sEoE_Gauss)
                    else
                        None
                // compute area beneath curve
                match modelFunction with
                // TODO with F# 4.1 replace with OK, Error Pattern
                | None -> 
                    None, 3
            
                | Some (modelF,(paramV, yData), sEoE) ->
                    let medianX = paramV.[1]
                    let xData = [|0. .. 0.05 .. medianX+300.|]
                    let yData = 
                        xData
                        |> Array.map (modelF.GetFunctionValue paramV)
                    if xData |> Array.isEmpty || yData |> Array.isEmpty then
                        None, 5
                    else
                    let yMaxModel = 
                        if yData |> Array.isEmpty then 0.
                        else
                        yData |> Array.max
                    // Maybe this is reduntant and one can also choose 0 and 10000 as intervalStarts.
                    let intervalBegin = 
                        match iterateTo (+1) yData 1 (fun (x:float) -> x > 0.01*yMaxModel) with 
                        | Some idx -> idx
                        | None     -> 0 
                    let intervalEnd   =
                        match iterateTo (-1) yData (yData.Length-2)  (fun (x:float) -> x > 0.01*yMaxModel) with
                        | Some idx -> idx
                        | None     -> yData.Length-1
                    let area = 
                        MathNet.Numerics.Integrate.OnClosedInterval((fun x -> (modelF.GetFunctionValue paramV x)),xData.[intervalBegin], xData.[intervalEnd]) 
                    let deltaScanTimePeakApex =  (scanTime - gausParamEstCaruana.MeanX)
                     //
                    let (xMax,yMaxXic) = xAndYData |> Array.maxBy (fun x -> snd x)
                    //
                    Some (createQuantificationResult FitBothModels.True modelF area sEoE deltaScanTimePeakApex yMaxModel yMaxXic), 10
            // Case B:
            else
                let modelFunction = 
                    ///
                    let gausParamA = 
                        let tmpGauss = Array.create 3 0.
                        tmpGauss.[0] <- gausParamEstCaruana.Amplitude 
                        tmpGauss.[1] <- (gausParamEstCaruana.MeanX ) 
                        tmpGauss.[2] <- gausParamEstCaruana.STD 
                        tmpGauss
                    ///
                    let gaussPrediction =
                        let paramConti = new ResizeArray<DenseVector>()
                        let gaussSolOptions = createGaussSolverOption gausParamA
                        ///
                        let testgauss() = 
                            try
                                let gaussParamA = Fitting.levenbergMarquardtSolver Fitting.Table.gaussModel gaussSolOptions xDataForFit yDataForFit paramConti
                                                    
                                let gaussYPredicted = Array.map (fun xValue -> Fitting.Table.gaussModel.GetFunctionValue gaussParamA xValue) xDataForFit
                                Some (gaussParamA, gaussYPredicted)
                            with 
                            | :? System.ArgumentException as ex  -> 
                                                                None
                        testgauss()

                    if gaussPrediction.IsSome then
                        let yGauss = snd gaussPrediction.Value
                        let sEoE_Gauss = Fitting.standardErrorOfPrediction 3. yGauss yDataForFit
                        Some (Fitting.Table.gaussModel, gaussPrediction.Value, sEoE_Gauss)
                    else
                        None
                // compute area beneath curve
                match modelFunction with
                // TODO with F# 4.1 replace with OK, Error Pattern
                | None -> 
                    None, 4
            
                | Some (modelF,(paramV, yData), sEoE) ->
                    let medianX = paramV.[1]
                    let xData = [|-300. .. 0.05 .. medianX+300.|]
                    if Array.isEmpty xData then None,5 
                    else
                    let yData = 
                        xData
                        |> Array.map (modelF.GetFunctionValue paramV)
                    if xData |> Array.isEmpty || yData |> Array.isEmpty then
                        None, 5
                        else
                    let yMaxModel = 
                        if yData |> Array.isEmpty then 0.
                        else
                        yData |> Array.max
                    // Maybe this is reduntant and one can also choose 0 and 10000 as intervalStarts.
                    let intervalBegin = 
                        match iterateTo (+1) yData 1 (fun (x:float) -> x > 0.01*yMaxModel) with 
                        | Some idx -> idx
                        | None     -> 0 
                    let intervalEnd   =
                        match iterateTo (-1) yData (yData.Length-2)  (fun (x:float) -> x > 0.01*yMaxModel) with
                        | Some idx -> idx
                        | None     -> yData.Length-1
                    //
                    let area = 
                        MathNet.Numerics.Integrate.OnClosedInterval((fun x -> (modelF.GetFunctionValue paramV x)),xData.[intervalBegin], xData.[intervalEnd]) 
                    //
                    let deltaScanTimePeakApex = abs (scanTime - gausParamEstCaruana.MeanX)
                    //
                    let (xMax,yMaxXic) = xAndYData |> Array.maxBy (fun x -> snd x)
                    //
                    Some (createQuantificationResult FitBothModels.True modelF area sEoE deltaScanTimePeakApex yMaxModel yMaxXic), 11
    
