(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
//#I "../../bin"

(**
BioFSharp
======================


*)
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.Mz.dll"
#r "../../bin/BioFSharp.IO.dll"
//#r "../../bin/Plotly.dll"

open BioFSharp
open BioFSharp.Mz
open BioFSharp.IO
//open Plotly

/// Reads first entry out of a example mgf File
let ms1DataTest = 
    Mgf.readMgf (__SOURCE_DIRECTORY__ + "/data/ms1Example.mgf")  
    |> List.head

/// Converts the mzData and intensityData into Arrays 
let rawMZData        = ms1DataTest.Mass //|> List.toArray 
let rawIntensityData = ms1DataTest.Intensity// |> List.toArray 

/// Returns a mzIntensityArray containing the processed data.
let centroidedSpectra = 
    SignalDetection.Wavelet.toCentroid rawMZData rawIntensityData

///// Creates pointCharts of the raw and the processed data.
//let rawDataChart        = Chart.Point(mzData, intensityData)
//let centroidedDataChart = Chart.Point(centroidedSpectra.MzData, centroidedSpectra.IntensityData)
//let combChart           = Chart.Combine [rawDataChart;processedDataChart]
//
//combChart
//|> Chart.Show

/// Returns a mzIntensityArray containing the processed data. 
let centroidedSpectraInWindow = 
     SignalDetection.Wavelet.windowToCentroidBy rawMZData rawIntensityData 1.5 450.

///// Creates pointCharts of the processed data.
//let centroidedSpectraInWindow = Chart.Point(centroidedSpectraInWindow.MzData, centroidedSpectraInWindow.IntensityData)
//let combChart                 = Chart.Combine [rawDataChart;centroidedSpectraInWindow]
//
//combChart
//|> Chart.Show
