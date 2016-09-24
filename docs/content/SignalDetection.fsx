(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
//#I "../../bin"

(**
SignalDetection
======================


*)
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.Mz.dll"
#r "../../bin/BioFSharp.IO.dll"
#r "../../packages/build/FSharp.Plotly/lib/net40/Fsharp.Plotly.dll"

open BioFSharp
open BioFSharp.Mz
open BioFSharp.IO
open FSharp.Plotly

/// Returns the first entry of a examplary mgf File
let ms1DataTest = 
    Mgf.readMgf (__SOURCE_DIRECTORY__ + "/data/ms2Example.mgf")  
    |> List.head

/// Returns a tuple of float arrays (mzData[]*intensityData[]) each containing the processed data
let centroidedSpectra = 
    SignalDetection.Wavelet.toCentroid ms1DataTest.Mass ms1DataTest.Intensity

/// Creates pointCharts of the raw and the processed data
let rawDataChart = 
    Chart.Point(ms1DataTest.Mass, ms1DataTest.Intensity)
let centroidedDataChart =   
    Chart.Point(fst centroidedSpectra, snd centroidedSpectra)


(*** define-output:spectrum1 ***)
/// Creates a combined chart 
let combChart = 
    Chart.Combine [rawDataChart;centroidedDataChart]
(*** include-it:spectrum1 ***)

/// Shows the chart in a browser
combChart
|> Chart.Show

// If only a window of the input data shall be processed the following functions can be used.
// This can be a favourable approach if only a subgroup of the data is of interest to the user 
// and computation time is a limiting factor.

/// Returns a tuple of float arrays (mzData[]*intensityData[]) containing only the centroids in a
/// window of a user given width centered around a user given m/z value.
let centroidsInWindow = 
     SignalDetection.Wavelet.windowToCentroidBy ms1DataTest.Mass ms1DataTest.Intensity 3. 750.3157086

///// Creates pointCharts of the processed data.
let centroidsInWindowChart = 
    Chart.Point(fst centroidsInWindow, snd centroidsInWindow )

(*** define-output:spectrum2 ***)
/// Creates a another combined chart of the unprocessed data and the centroided data
let anotherCombChart = 
    Chart.Combine [rawDataChart;centroidsInWindowChart]
(*** include-it:spectrum2 ***)
/// Shows the chart in a browser
anotherCombChart
|> Chart.Show

