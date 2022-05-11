#r "nuget: FSharpAux"
#r "nuget: FSharpAux.IO"
#r "nuget: FSharp.Stats"
#r "nuget: Newtonsoft.Json"

#load "Refactor.fs"
#load "PhysicalConstants.fs"
#load "BioID.fs"
#load "Isotopes.fs"
#load "Elements.fs"
#load "Formula.fs"
#load "Mass.fs"
#load "IBioItem.fs"
#load "TaggedSequence.fs"
#load "SequenceFeature.fs"
#load "AnnotatedSequence.fs"
#load "IsotopicDistribution.fs"
#load "ModificationInfo.fs"
#load "AminoAcidSymbols.fs"
#load "AminoAcids.fs"
#load "Nucleotides.fs"
#load "GlobalModificationInfo.fs"
#load "BioItemsConverter.fs"
#load "BioSeq.fs"
#load "BioArray.fs"
#load "BioList.fs"
#load "AminoProperties.fs"
#load "IsoelectricPoint.fs"
#load "Digestion.fs"
#load "PhylTree.fs"
#load "Alignment.fs"
#load "PeptideClassification.fs"
#load "WorkflowLanguage.fs"
#load "Algorithm/PatternQuery.fs"
#load "Algorithm/PairwiseAlignment.fs"
#load "Algorithm/ScoringMatrix.fs"

open FSharpAux
open FSharpAux.IO
open FSharp.Stats

open BioFSharp

open BioFSharp
open BioFSharp.Algorithm
open BioFSharp.Algorithm.PairwiseAlignment

let aaScoring = ScoringMatrix.getScoringMatrixAminoAcid ScoringMatrix.ScoringMatrixAminoAcid.BLOSUM62
let nucScoring = ScoringMatrix.getScoringMatrixNucleotide  ScoringMatrix.ScoringMatrixNucleotide.EDNA

//For aminoacids
let costAA = {
    Open = 5
    Continuation = -1
    Similarity = aaScoring 
    }

//For nucleotides
let costN = {
    Open = -5
    Continuation = -1
    Similarity = nucScoring 
    }

let query1AA = "NLFVAAAAQTKNGQGWVPSNYITPVNSAAA" |> BioArray.ofAminoAcidSymbolString
let query2AA = "NLFVALYDFVASGDNTLSITKGEKLRVLGYNHNGEWCEAQTKNGQGWVPSNYITPVNS" |> BioArray.ofAminoAcidSymbolString

let localAA = 
    PairwiseAlignment.Local.SmithWaterman.align(query1AA,query2AA,costAA)

let globalAA =
    PairwiseAlignment.Global.NeedlemanWunsch.align(query1AA,query2AA,costAA)

localAA.MetaData.Score