#r "../../../packages/FSharpAux/lib/netstandard2.0/FSharpAux.dll"

#load "../PhysicalConstants.fs"
#load "../BioID.fs"
#load "../Isotopes.fs"
#load "../Elements.fs"
#load "../Formula.fs"
#load "../Mass.fs"
#load "../IBioItem.fs"
#load "../TaggedSequence.fs"
#load "../IsotopicDistribution.fs"
#load "../ModificationInfo.fs"
#load "../AminoAcidSymbols.fs"
#load "../AminoAcids.fs"
#load "../Nucleotides.fs"
#load "../GlobalModificationInfo.fs"
#load "../BioItemsConverter.fs"
#load "../BioSeq.fs"

open BioFSharp

"AUGGUACUGACGAUUUAUCCUGACGAACUC" 
|> BioSeq.ofNucleotideString
|> BioSeq.mapInTriplets id

let a = 
    "AUGGUACUGACGAUUUAUCCUGACGAACUCTT" 
    |> BioSeq.ofNucleotideString
    |> BioSeq.translate 0

let b = 
    "AUGGUACUGACGAUUUAUCCUGACGAACUC" 
    |> BioSeq.ofNucleotideString
    |> BioSeq.translate 0
