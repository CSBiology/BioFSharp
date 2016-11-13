(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
//#I "../../bin"
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.IO.dll"
#r "../../bin/BioFSharp.Mz.dll"
#r "../../packages/build/FSharp.Plotly/lib/net40/Fsharp.Plotly.dll"
(**
Spectrum centroidization
========================
*)

(**
This part of the documentation aims to give a brief overview of the workflow used to detect the spectral centroids of MS Spectra.
*)
open BioFSharp
open BioFSharp.Mz
open BioFSharp.IO
open FSharp.Plotly

/// Returns the first entry of a examplary mgf File
let ms1DataTest = 
    Mgf.readMgf (__SOURCE_DIRECTORY__ + "/data/ms1Example.mgf")  
    |> List.head

/// Returns a tuple of float arrays (mzData[]*intensityData[]) each containing the processed data
let centroidMS1Spectrum = 
    SignalDetection.Wavelet.toSNRFilteredCentroid ms1DataTest.Mass ms1DataTest.Intensity

(*** define-output:spectrum1 ***)
/// Creates point charts of the raw and the processed data
[
    Chart.Point(ms1DataTest.Mass, ms1DataTest.Intensity,Name="raw data");
    Chart.Point(fst centroidMS1Spectrum, snd centroidMS1Spectrum,Name="centroid")
]
|> Chart.Combine
|> Chart.Show
(*** include-it:spectrum1 ***)

(**
If only a window of the input data shall be processed the following functions can be used.
This can be a favourable approach if only a subgroup of the data is of interest to the user 
and computation time is a limiting factor.
*)

/// Returns a tuple of float arrays (mzData[]*intensityData[]) containing only the centroids in a
/// window of a user given width centered around a user given m/z value.
let ms1CentroidsInWindow = 
     SignalDetection.windowToCentroidBy SignalDetection.Wavelet.toSNRFilteredCentroid ms1DataTest.Mass ms1DataTest.Intensity 7.5 643.8029052

 
(*** define-output:spectrum2 ***)
/// Creates a another combined chart of the unprocessed data and the centroided data
[
    Chart.Point(ms1DataTest.Mass, ms1DataTest.Intensity,Name="raw data");
    Chart.Point(fst ms1CentroidsInWindow, snd ms1CentroidsInWindow,Name="processed data");
]
|> Chart.Combine

(*** include-it:spectrum2 ***)

/// Returns the first entry of a examplary mgf File
let ms2DataTest = 
    Mgf.readMgf (__SOURCE_DIRECTORY__ + "/data/ms2Example.mgf")  
    |> List.head

/// Returns a tuple of float arrays (mzData[]*intensityData[]) each containing the processed data
let centroidMS2Spectrum = 
    SignalDetection.Wavelet.toCentroid ms2DataTest.Mass ms2DataTest.Intensity

//
let snrFilteredCentroidMS2Spectrum = SignalDetection.filterByIntensitySNR  55. 1.1 (fst centroidMS2Spectrum) (snd centroidMS2Spectrum)     
        
(*** define-output:spectrum3 ***)
/// Creates a another combined chart of the unprocessed data and the centroided MS2 data
[
    Chart.Point(ms2DataTest.Mass, ms2DataTest.Intensity,Name="raw data");
    Chart.Point(fst centroidMS2Spectrum, snd centroidMS2Spectrum,Name="centroided data");
    Chart.Point(fst snrFilteredCentroidMS2Spectrum, snd snrFilteredCentroidMS2Spectrum,Name="centroided & filtered");
]
|> Chart.Combine
(*** include-it:spectrum3 ***)
