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
        let caruanaAlgorithm mzData intensityData =
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
            createGausParams amplitude meanX std fwhm

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