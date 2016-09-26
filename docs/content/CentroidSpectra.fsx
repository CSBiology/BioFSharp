(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
//#I "../../bin"
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.Mz.dll"
#r "../../bin/BioFSharp.IO.dll"
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
let centroidSpectrum = 
    SignalDetection.Wavelet.toCentroid ms1DataTest.Mass ms1DataTest.Intensity


(*** define-output:spectrum1 ***)
/// Creates point charts of the raw and the processed data
[
    Chart.Point(ms1DataTest.Mass, ms1DataTest.Intensity,Name="raw data");
    Chart.Point(fst centroidSpectrum, snd centroidSpectrum,Name="centroid")
]
|> Chart.Combine
(*** include-it:spectrum1 ***)

(**
If only a window of the input data shall be processed the following functions can be used.
This can be a favourable approach if only a subgroup of the data is of interest to the user 
and computation time is a limiting factor.
*)

/// Returns a tuple of float arrays (mzData[]*intensityData[]) containing only the centroids in a
/// window of a user given width centered around a user given m/z value.
let centroidsInWindow = 
     SignalDetection.Wavelet.windowToCentroidBy ms1DataTest.Mass ms1DataTest.Intensity 3. 643.8029052

 
(*** define-output:spectrum2 ***)
/// Creates a another combined chart of the unprocessed data and the centroided data
[
    Chart.Point(ms1DataTest.Mass, ms1DataTest.Intensity,Name="raw data");
    Chart.Point(fst centroidsInWindow, snd centroidsInWindow,Name="processed data");
]
|> Chart.Combine
(*** include-it:spectrum2 ***)
