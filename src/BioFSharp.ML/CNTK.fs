namespace BioFSharp.ML

module CNTKExtensions =

    open CNTK
    open System.IO 
    open System.Collections.Generic

    ///returns a byte array from the ressource stream.
    let loadModelWithDevice (device : DeviceDescriptor) (modelPath) =
        let modelBuffer = 
            use stream = new FileStream(modelPath,FileMode.Open)
            let length = int stream.Length
            use bReader = new BinaryReader(stream)
            bReader.ReadBytes(length)

        Function.Load(modelBuffer,device)

    let loadModel (modelPath) = 
        loadModelWithDevice (DeviceDescriptor.UseDefaultDevice()) modelPath

    let toInputBatchWithDevice (device: DeviceDescriptor) (predictor:Function) (data: seq<#seq<'Feature>>) =
        let inputVar: Variable = predictor.Arguments.Item 0
        
        let inputShape = inputVar.Shape
        
        /// Extracts all Features and appends them, stores Values in a List
        let featureData = 
            let tmp = new List<'Feature>()
            data |> Seq.iter(fun x -> 
                                let data' = x
                                tmp.AddRange(data')
                                )
            tmp
        
        /// Creates an input Batch
        let inputValues = Value.CreateBatch(inputShape,featureData,device)
        
        let inputMap = new Dictionary<Variable,Value>()
        inputMap.Add(inputVar,inputValues)
        inputMap

    let toInputBatch (predictor:Function) (data: 'Feature [] []) = 
        toInputBatchWithDevice (DeviceDescriptor.UseDefaultDevice()) (predictor:Function) (data: 'Feature [] [])

    let predictAsWithDevice<'Output> (device: DeviceDescriptor) (predictor:Function) (inputBatch: Dictionary<Variable,Value>) =
        
        ///////////Output
        let outputVar : Variable = predictor.Output

        let outputMap = new Dictionary<Variable,Value>()
        outputMap.Add(outputVar,null)

        predictor.Evaluate(inputBatch,outputMap,device)

        let outputValues = outputMap.[outputVar]


        outputValues.GetDenseData<'Output>(outputVar)
        |> Seq.concat
        |> Array.ofSeq

    let predictAs<'Output> (predictor:Function) (inputBatch: Dictionary<Variable,Value>) =
        predictAsWithDevice<'Output> (DeviceDescriptor.UseDefaultDevice()) predictor inputBatch


