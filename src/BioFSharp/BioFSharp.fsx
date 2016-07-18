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
#load "IBioSequences.fs"

#load "Digestion.fs"
//#load "Mass.fs"

open FSharp.Care
open BioFSharp

let t = BioA .ofAminoAcidString "ACAS"



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
