(*** hide ***)
#r "../../lib/FSharp.FGL.dll"
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.Mz.dll"
#r "../../bin/FSharp.Care.dll"
#r "../../bin/MathNet.Numerics.dll"
#r "../../packages/build/FSharp.Plotly/lib/net40/Fsharp.Plotly.dll"
#load "./data/Spectrum.fs"
let exampleMzList,exampleIntList,charge,precursor = ExampleData.peakMassList,(ExampleData.peakIntensities |> List.map (fun x -> float x)),ExampleData.mz,ExampleData.precursorMass
(**
De novo identification of an exemplary mass spectrum.
======================================================

This part of the documentation aims at giving a brief introduction to the workflow of a de novo identification of real MS spectrum data using the DeNovoAlgorithm.fs.
Therefore an already identified spectrum representing a synthetic fusion protein with the sequence **APLDNDIGVSEATR** is identified.

To interpret a mass spectrum several information are needed:

- The m/z values of the spectrum.
- The peak intensities of the spectrum.
- The precursor mass.
- The charge state of the spectrum.

Also the spectral centroids of the MS data should be detected. Further information about this process can be found in the [Spectrum centroidization documentation](https://csbiology.github.io/BioFSharp/CentroidSpectra.html).  

As a first step the mass spectrum data are represented as a list of peaks.
*)

open System
open BioFSharp
open BioFSharp.Mz
open FSharp.FGL
open FSharp.FGL.Directed

open FSharp.Plotly

/// Creates a Peak list from the list of m/z values & the list of intensity values.
let peakList = PeakList.zipMzInt exampleMzList exampleIntList

(*** define-output:exampleSpectrum ***)
/// Creates and shows a point chart of the spectrum data with FSharp.Plotly.
Chart.Point(exampleMzList, exampleIntList) |> Chart.Show
(*** include-it:exampleSpectrum ***)

(**
With this list of peaks ion nodes are created. The creation of the SpectrumGraphNode list is based on: 
Chen T., Kao M., Tepel M., Rush J., Church G. (2001). A Dynamic Programming Approach to De Novo Peptide Sequencing via Tandem Mass Spectrometry.
Each peak of the spectrum is represented by a specific ion node. By default only a C-ion node list is created instead of both a C & N - ion list. 
Calculating and including both ion types would allow to further determine if the resulting path is part of the N- or C-terminal ion ladder. 
As this information isn't essential for the identification of the spectrum it can be skipped in favor of a better performance.
*)

/// Calculates the neutral precursor mass.
let neutralPrecursorMass = Mass.ofMZ precursor charge

/// Returns a list of nodes representing C-terminal ions resulting from the Peak list.
let cTerminalNodeList = FunctionalGraph.cIonsOfSpectrumWCharges neutralPrecursorMass (charge |> int) peakList
                        //FunctionalGraph.cIonsOfSpectrum neutralPrecursorMass peakList


(**
The mass spectrum data is represented as a functional graph, consisting of a list of contexts,
whereas each context is related to a specific node and contains the information about the nodes predecessor and successor list, as well as its label. 
An edge between two nodes is created if the difference between their masses matches the mass of an amino acid within a defined accuracy range.
The representation is based on Erwig, M. (2001). Inductive graphs and functional graph algorithms. 
To create the functional graph the [Functional Graph Library](https://github.com/CSBiology/FSharp.FGL) was used.
*)

/// Calculates edges between the nodes with an accuracy of 40ppm and represents the mass spectrum data as functional graph.
let funGraphofMS = FunctionalGraph.createfunGraph 40. FunctionalGraph.references cTerminalNodeList
                   
(**
<img src="@Root/img/TG1.JPG" alt="TG1" style="width:150px;margin:10px" />
*)

(**
A depth first search algorithm is used to find the longest path of connected edges in the graph resulting from the position of a specified node.
The maximum gap size must be defined as an integer value that decides how many adjacent nodes are considered if a potential gap is found.
The algorithm returns the amino acid list of the longest path and the masses of found gaps.
*)
 
(*** define-output:specificTestMSResult ***)
/// Searches the longest path of connected edges from the first node of the graph with a maximum gap size of 5.
let longestPath = DeNovoAlgorithm.findLongestPath neutralPrecursorMass funGraphofMS 1 5                  
(*** include-it:specificTestMSResult ***)

(**
For closer investigation of the spectrum a more memory expensive approach can be used which stores all paths with gaps and chooses the most fitting ones based on the overall mass of the found path.
*)

(*** define-output:specificTestMSResult2 ***)
/// Stores all paths with gaps in the spectrum in a collection and returns the one with the highest overall mass.
let longestPath2 = DeNovoAlgorithm.collectAllLongestPaths neutralPrecursorMass funGraphofMS 1 10
                   |> DeNovoAlgorithm.chooseResultsBasedOnMass 1  
(*** include-it:specificTestMSResult2 ***)
 