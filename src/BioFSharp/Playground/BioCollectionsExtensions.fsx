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
#load "../BioCollectionsExtensions.fs"

open BioFSharp

let testSeq =
    BioSeq.ofAminoAcidString "TESTESTESTLUL"

testSeq |> BioSeq.toString

let meem = 

    testSeq

    |> BioList.ofBioSeq
    |> BioSeq.toBioList

    |> BioArray.ofBioList
    |> BioArray.toBioList

    |> BioSeq.ofBioList
    |> BioSeq.toBioArray

    |> BioList.ofBioArray
    |> BioList.toBioArray

    |> BioSeq.ofBioArray
    |> BioSeq.toBioList

    |> BioList.toBioSeq
    |> BioArray.ofBioSeq
    |> BioArray.toBioSeq

(BioSeq.isEqual testSeq meem ) = 0