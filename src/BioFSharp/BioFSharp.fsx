#light

#r "../../bin/FSharp.Care.dll"

#load "PhysicalConstants.fs"
#load "Isotopes.fs"
#load "Elements.fs"
#load "Formula.fs"
#load "ModificationInfo.fs"
#load "IBioItem.fs"
#load "AminoAcids.fs"
#load "Nucleotides.fs"
#load "IBioSequence.fs"
#load "BioArray.fs"
#load "BioList.fs"

#load "Digestion.fs"
//#load "Mass.fs"

open FSharp.Care
open BioFSharp

//open BioArray


open IBioSequence

//type IBioSequence<[<EqualityConditionalOn; ComparisonConditionalOn >]'a when 'a :> IBioItem> =  seq<'a>
type BioArray<'a when 'a :> IBioItem> = array<'a>


let ofAminoAcidString (s:#seq<char>) : BioArray<AminoAcids.AminoAcid> =
    IBioSequence.ofAminoAcidString s 
    |> Seq.toArray

let t' = ofAminoAcidString "ACAS"


let t : array<AminoAcids.AminoAcid> = 
    BioArray.ofAminoAcidString "ACAS"
    
//let t' : array<IBioItem> = upcast t
 

/// Returns formula
let toFormula (bs:BioArray<_>) =
    bs |> Array.fold (fun acc item -> Formula.add acc  (BioItem.formula item)) Formula.emptyFormula

/// Returns average mass of the given sequence !memoization
let toAverageMass<'a when 'a :> IBioItem> : (BioArray<'a> -> float) =
    let memAverageMass =
        Memoization.memoizeP (BioItem.formula >> Formula.averageMass)
    (fun bs -> 
        bs 
        |> Array.sumBy memAverageMass)

toAverageMass t'//[|Nucleotides.A|]// [|AminoAcids.Ala|]

BioArray.toFormula [|Nucleotides.A|]// [|AminoAcids.Ala|]


//
//toAverageMass t


//#if INTERACTIVE
//    module InstallFsiAutoDisplay =
//        // Single
//        fsi.AddPrinter( fun (nuc:Nucleotides.Nucleotide) -> (Nucleotides.symbol nuc).ToString() )
//        fsi.AddPrinter( fun (aa:AminoAcids.AminoAcid)    -> (AminoAcids.symbol aa).ToString() )
//    
////        // Sequences
////        fsi.AddPrinter( fun (nucs:BioSequences. NUC.NucleotideSequence) -> new string [|for n in nucs  -> Nucleotides.symbol n|] )
////        fsi.AddPrinter( fun (aas:BioSequences.AAS.AminoAcidSequence)   -> new string [|for a in aas   -> AminoAcids.symbol a |] )
////        fsi.AddPrinter( fun (bs:BioSequences.BioSequence)          -> BioSequences.toString bs )
//
//        // other
//        fsi.AddPrinter( fun (forumla:Formula.Formula) -> Formula.toString forumla )
//    
//
//#endif
