namespace BioFSharp.Mz

open BioFSharp
//open Fragmentation
//open Spectra
//
//open MathNet.Numerics
//open MathNet.Numerics.LinearAlgebra.Double

module SequestLike =

    type SequestLikeScore =
        { SpectrumID      : string;
          PrecursorCharge : int;
          IsTarget        : bool;
          Peptide         : seq<AminoAcids.AminoAcid>;
          TheoMass        : float;      
          MeasuredMass    : float;      
          PeptideLength   : int;
          XCORR           : float;
          DeltaCN         : float; }

    let createSequestLikeScore spectrumID precursorCharge isTarget peptide theoMass measuredMass peptideLength xcorr deltaCN = 
        { SpectrumID = spectrumID; PrecursorCharge = precursorCharge; IsTarget = isTarget; Peptide = peptide; TheoMass = theoMass; MeasuredMass = measuredMass; PeptideLength = peptideLength; XCORR = xcorr; DeltaCN = deltaCN; }



//    /// normalize the intensities within a window to maximum of the window
//    /// Attention shortens the array  (cuts)
//    let windowNormalizeIntensities (intensities:float[]) (numberOfWindows:int) =
//        let windowSize =  (intensities.Length / numberOfWindows)
//        seq { for i = 1 to numberOfWindows do
//                //printfn "window: %i lower: %i counter: %i " i (windowSize * (i - 1)) (windowSize * i - 1)
//                yield! normToMaxSqrt intensities (windowSize * (i - 1)) (windowSize * i - 1) }