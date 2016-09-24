(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "../../packages/build/FSharp.Plotly/lib/net40/Fsharp.Plotly.dll"
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.Mz.dll"
#r "../../bin/BioFSharp.IO.dll"
#r "../../bin/Mathnet.Numerics.dll"
#r "../../bin/Mathnet.Numerics.FSharp.dll"
open FSharp.Plotly
//TODO: Add FILES and LINKS to other tutorials 
(**
Peptide Identification using SequestLike.fs
===========================================
This part of the documentation aims to give a brief overview of the functionality of SequestLike.fs. This Module implements a peptide identification
algorithm that is based on the mass spectrometry data analysis program SEQUEST. More specifically this algorithm matches a aquired MS2 spectrum against 
a hypothetical fragment spectrum of a amino acid sequence with a matching peptide mass. This process is based on the computation of cross- 
and autocorrelation of the spectra. 

The following code will return the first entry of a examplary mgf File. In this case this will return a aquired MS2 spectrum.
*)
open BioFSharp
open BioFSharp.Mz
open BioFSharp.IO
///Returns the first entry of a examplary mgf File
let ms2DataTest = 
    Mgf.readMgf (__SOURCE_DIRECTORY__ + "/data/ms2Example.mgf")  
    |> List.head

(**
This entry already contains information allocated to the spectrum, such as an assigned chargestate and an inferred peptide mass. 
(for further information how to allocate these features to the spectra have a look at the ChargeStateDetermination tutorial)
The following functions show how to acess the features of this entry.
*)

///Returns chargestate of the mgf entry
let ms2Charge = Mgf.tryGetPrecursorCharges ms2DataTest
///Returns precursorMZ of the mgf entry
let precursorMZ = Mgf.tryGetPrecursorMZ ms2DataTest
///Returns peptide mass of the mgf entry
let ms2pepMass = Mgf.tryGetPrecursorMass ms2DataTest
///Returns mz array of the mgf entry 
let ms2rawMzData = ms2DataTest.Mass
///Returns intensity array of the mgf entry
let ms2rawIntensityData = ms2DataTest.Intensity

(**
Since these arrays contain raw MS2 data they have to be preprocessed to gain a reasonable output of the Sequestlike algorithm.
As demonstrated in the SignalProcessing tutorial the spectral centroids of the MS2 spectrum can be optained using the following function:
*)  
/// Returns a tuple of float arrays (mzData[]*intensityData[]) each containing the processed data
let centroidedSpectrum = 
    SignalDetection.Wavelet.toCentroid ms2rawMzData ms2rawIntensityData
    |> (fun (m,i) -> PeakArray.zipMzInt m i)

(**
With this function evaluated, we have the centroided spectrum data at hand and therefore almost all parameters needed to run the Sequestlike 
algorithm. As mentioned in the brief description, this algorithm compares the aquired MS2 spectrum to hypothetical fragment spectra of amino acid 
sequences with matching peptide masses. Such candidate sequences can be optained by a mass lookup in a peptide database. 

The following code snippet accesses a demo database and returns peptide sequences with a matching mass wrapped in the type LookUpResult. 
First the database is accessed and the parameters used to create the db are retrieved. These parameters can be used to asshure that this 
database fits the lookUp criteria. Furthermore these parameters can be used to access the database. 
(for further information how to gain these features have a look at the peptideLookUp tutorial)
*) 

/// Returns the filepath to a demo database 
let dbPath = (__SOURCE_DIRECTORY__ + "/data/Creinhardtii_CpN14Seq.db")
/// Returns a instance of the type SearchDbParams
let sdbParams = SearchDB.getSDBParamsBy dbPath
/// Returns a function that takes two masses as input parameters and returns a list of peptides (wrapped in the type LookUpResult)
let n14DBLookUpBy =  
        SearchDB.getPeptideLookUpBy sdbParams           
/// Returns the result of the peptide lookup
let n14PeptideLookUp mass =
        let lowerBorder = mass-0.5
        let upperBorder = mass+0.5 
        n14DBLookUpBy lowerBorder upperBorder
/// Returns a list of candidate peptide sequences 
let candidatePeps = n14PeptideLookUp ms2pepMass.Value


(**
This list of sequences matching the putative precursor mass assigned to the spectra and the centroided ms2 spectrum data can then used as 
input parameters of the function SequestLike.calcSequestLikeScoresRevDecoy. The result of this function is a list of the type SequestlikeScore.
*) 

/// Returns 
let scoreList = 
    SequestLike.calcSequestLikeScoresRevDecoy 
        Formula.monoisoMass (300, 1200) centroidedSpectrum 2 precursorMZ.Value candidatePeps "1"
        |> Seq.sortBy (fun s -> -s.XCORR)

(**
This list of sequences matching the putative precursor mass assigned to the spectra and the centroided ms2 spectrum data can then used as 
input parameters of the function SequestLike.calcSequestLikeScoresRevDecoy. The result of this function is a list of the type SequestlikeScore.
Each putative peptide is assigned with a XCORR value and a DeltaCN value. The XCORR value is deduced from the cross- and autocorrelation of the 
theoretical fragment spectra and the centroided ms2 data and can be used as to rate their level of agreement. A look at the best scoring peptide 
reveals that its sequence corresponds to the actual sequence assigned to the mgf entry. 
*) 