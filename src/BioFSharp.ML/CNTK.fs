namespace BioFSharp.ML

module CNTKExtensions =

    open CNTK
    open System.IO 
    open System.Collections.Generic

    //Loads a model as binary modelBuffer from the given path and returns the resulting CNTK.Function using the given DeviceDescriptor
    let loadModelWithDevice (device : DeviceDescriptor) (modelPath) =
        let modelBuffer = 
            use stream = new FileStream(modelPath,FileMode.Open)
            let length = int stream.Length
            use bReader = new BinaryReader(stream)
            bReader.ReadBytes(length)

        Function.Load(modelBuffer,device)

    ///Loads a model as binary modelBuffer from the given path and returns the resulting CNTK.Function
    let loadModel (modelPath) = 
        loadModelWithDevice (DeviceDescriptor.UseDefaultDevice()) modelPath

    ///Returns an input batch from the given feature vectors attached to the given model using the given DeviceDescriptor
    let toInputBatchWithDevice (device: DeviceDescriptor) (model:Function) (featureVectors: seq<#seq<'Feature>>) =
        let inputVar: Variable = model.Arguments.Item 0
        
        let inputShape = inputVar.Shape
        
        /// Extracts all Features and appends them, stores Values in a List
        let featureData = 
            let tmp = new List<'Feature>()
            featureVectors 
            |> Seq.iter
                (fun x -> 
                    let data' = x
                    tmp.AddRange(data')
                )
            tmp
        
        /// Creates an input Batch
        let inputValues = Value.CreateBatch(inputShape,featureData,device)
        
        let inputMap = new Dictionary<Variable,Value>()
        inputMap.Add(inputVar,inputValues)
        inputMap

    ///Returns an input batch from the given feature vectors attached to the given model
    let toInputBatch (model:Function) (featureVectors: seq<#seq<'Feature>>) = 
        toInputBatchWithDevice (DeviceDescriptor.UseDefaultDevice()) model featureVectors

    ///Returns evaluations of the given model function for the input batch of attached feature vectors using the given DeviceDescriptor.
    ///
    ///To determine the type to cast the output to, use the following notation:
    ///
    /// predictAsWithDevice<float> ...
    let predictAsWithDevice<'Output> (device: DeviceDescriptor) (model:Function) (inputBatch: Dictionary<Variable,Value>) =
        
        //Set up output variables
        let outputVar : Variable = model.Output

        let outputMap = new Dictionary<Variable,Value>()
        outputMap.Add(outputVar,null)

        //Evaluate prediction
        model.Evaluate(inputBatch,outputMap,device)

        let outputValues = outputMap.[outputVar]

        //Return predicted data casted to the target type
        outputValues.GetDenseData<'Output>(outputVar)
        |> Seq.concat

    ///Returns evaluations of the given model function for the input batch of attached feature vectors.
    ///
    ///To determine the type to cast the output to, use the following notation:
    ///
    /// predictAs<float> ...
    let predictAs<'Output> (model:Function) (inputBatch: Dictionary<Variable,Value>) =
        predictAsWithDevice<'Output> (DeviceDescriptor.UseDefaultDevice()) model inputBatch


