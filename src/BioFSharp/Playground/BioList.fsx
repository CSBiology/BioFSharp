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
#load "../BioArray.fs"
#load "../BioList.fs"

open BioFSharp

"AUGGUACUGACGAUUUAUCCUGACGAACUC" 
|> BioList.ofNucleotideString
|> BioList.mapInTriplets id

let a = 
    "AUGGUACUGACGAUUUAUCCUGACGAACUC" 
    |> BioList.ofNucleotideString
    |> BioList.translate 0

let b = 
    "AUGGUACUGACGAUUUAUCCUGACGAACUC" 
    |> BioList.ofNucleotideString
    |> BioList.translate 0
    
a = b