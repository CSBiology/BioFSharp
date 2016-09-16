(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
//#I "../../bin"

(**
BioFSharp
======================

C:\Users\david\Source\Repos\BioFSharp\packages\MathNet.Numerics\lib\net40\MathNet.Numerics.dll
*)
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.Mz.dll"
#r "../../bin/BioFSharp.IO.dll"
#r "../../bin/MathNet.Numerics.dll"
#r "../../bin/MathNet.Numerics.FSharp.dll"


open BioFSharp
open BioFSharp.Mz
open BioFSharp.IO
open System
open MathNet.Numerics

/// The chargestate determination algorithm implemented in ChargeState.fs analyzes the spacing in between 
/// single peaks of a isotope Cluster. 

/// Reads first entry out of a example mgf File
let ms1DataTest = 
    Mgf.readMgf (__SOURCE_DIRECTORY__ + "/data/ms1Example.mgf")  
    |> List.head

/// Converts the rawMZData and rawIntensityData of ms1DataTest into arrays.   
let rawMZData        = ms1DataTest.Mass |> List.toArray 
let rawIntensityData = ms1DataTest.Intensity |> List.toArray 

/// TestParameters used do determine the width of the peak window analyzed during chargestate determination and the
/// relative peak intensity thresholds.

let chargeDetParamTest  = 
    ChargeState.createChargeDetermParams
        1
        8
        1.1
        0.15
        0.3
        10000

/// PrecursorMZ that was picked to a
let ms2PrecursorMZ = 749.308432 

/// Instance of a Mersenne Twister, a random number generator used to 
let rnd = Random.MersenneTwister() 



///
let initGen = ChargeState.initGenerateMzSpecDevWithMem rnd chargeDetParamTest 0.015//peakPosStdDev


///// 
//let determinedCharge = 
//    ///
//    let (mzdata,intensityData) = 
//        SignalDetection.Wavelet.windowToCentroidBy rawMZData rawIntensityData 3. ms2PrecursorMZ  
//    ///
//    let putchargeList = 
//        ChargeState.putativePrecursorChargeStatesBy chargeDetParamTest mzdata intensityData ms2PrecursorMZ
//    putchargeList
//    ///
//    let testedItems = 
//        putchargeList 
//        |> List.map (fun assCh -> ChargeState.createTestedItem assCh (ChargeState.empiricalPValueOf initGen (assCh.NrOfPeaksInSubSet ,float assCh.Charge) assCh.MZChargeDev ))
//        |> ChargeState.removeSubSetsOfBestHit
//        |> List.map (fun testedI -> testedI.TestedObject)
//    testedItems