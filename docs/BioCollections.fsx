(*** hide ***)

(*** condition: prepare ***)
#r "nuget: Plotly.NET, 2.0.0-beta6"
#r "nuget: FSharpAux, 1.0.0"
#r "nuget: FSharpAux.IO, 1.0.0"
#r "nuget: FSharp.Stats, 0.4.0"
#r "../bin/BioFSharp/netstandard2.0/BioFSharp.dll"
#r "../bin/BioFSharp.IO/netstandard2.0/BioFSharp.IO.dll"
#r "../bin/BioFSharp.BioContainers/netstandard2.0/BioFSharp.BioContainers.dll"
#r "../bin/BioFSharp.ML/netstandard2.0/BioFSharp.ML.dll"
#r "../bin/BioFSharp.Stats/netstandard2.0/BioFSharp.Stats.dll"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-beta6"
#r "nuget: FSharpAux, 1.0.0"
#r "nuget: FSharpAux.IO, 1.0.0"
#r "nuget: FSharp.Stats, 0.4.0"
#r "nuget: Plotly.NET.Interactive, 2.0.0-beta6"
#r "nuget: BioFSharp, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.IO, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.BioContainers, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.ML, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.Stats, {{fsdocs-package-version}}"
#endif // IPYNB

(**
# BioCollections

Analogous to the build-in collections BioFSharp provides BioSeq, BioList and BioArray for individual collection specific optimized operations. 
The easiest way to create them are the `ofBioItemString` -functions
*)

open BioFSharp
open BioFSharp.IO

let s1 = "PEPTIDE" |> BioSeq.ofAminoAcidString 
let s2 = "PEPTIDE" |> BioList.ofAminoAcidSymbolString 
let s3 = "TAGCAT"  |> BioArray.ofNucleotideString 


///Peptide represented as a Bioseq
"PEPTIDE" |> BioSeq.ofAminoAcidString 

///Peptide represented as a BioList
"PEPTIDE"|> BioList.ofAminoAcidSymbolString 

///Nucleotide sequence represented as a BioArray
"TAGCAT" |> BioArray.ofNucleotideString 

(***hide***)
let s1Prnt     = FSIPrinters.prettyPrintBioCollection s1
let s2Prnt     = FSIPrinters.prettyPrintBioCollection s2
let s3Prnt     = FSIPrinters.prettyPrintBioCollection s3

(**Resulting BioSeq containing our peptide:*)
(*** include-value:s1 ***)
(**Resulting BioList containing our peptide:*)
(*** include-value:s2 ***)
(**Resulting BioArray containing our oligonucleotide:*)
(*** include-value:s3 ***)

(**
## Nucleotides

![Nucleotides1](img/Nucleotides.svg)

**Figure 1: Selection of covered nucleotide operations** (A) Bilogical principle. (B) Workflow with `BioSeq`. (C) Other covered functionalities.

Let's imagine you have a given gene sequence and want to find out what the according protein might look like.
*)
let myGene = BioSeq.ofNucleotideString "ATGGCTAGATCGATCGATCGGCTAACGTAA"

(***hide***)
let myGenePrnt     = FSIPrinters.prettyPrintBioCollection myGene

(*** include-value:myGene ***)

(**
Yikes! Unfortunately we got the 5'-3' coding strand. For proper transcription we should get the complementary strand first:
*)
let myProperGene = Seq.map Nucleotides.complement myGene

(***hide***)
let myProperGenePrnt = FSIPrinters.prettyPrintBioCollection myProperGene

(*** include-value:myProperGene ***)

(**
Now let's transcribe and translate it:
*)

let myTranslatedGene = 
    myProperGene
    |> BioSeq.transcribeTemplateStrand
    |> BioSeq.translate 0

(***hide***)
//let myTranslatedGenePrnt = FSIPrinters.prettyPrintBioCollection myTranslatedGene
 
(*** include-value:myTranslatedGene ***)

(**
Of course, if your input sequence originates from the coding strand, you can directly transcribe it to mRNA since the 
only difference between the coding strand and the mRNA is the replacement of 'T' by 'U' (Figure 1B)
*)

let myTranslatedGeneFromCodingStrand = 
    myGene
    |> BioSeq.transcribeCodingStrand
    |> BioSeq.translate 0

(***hide***)
//let myTranslatedGeneFromCodingStrandPrnt = FSIPrinters.prettyPrintBioCollection myTranslatedGeneFromCodingStrand

(*** include-value:myTranslatedGeneFromCodingStrand ***)

(**
Other Nucleotide conversion operations are also covered:
*)

let mySmallGene      = BioSeq.ofNucleotideString  "ATGTTCCGAT"

let smallGeneRev     = BioSeq.reverse mySmallGene 
//Original: ATGTTCCGAT
//Output:   TAGCCTTGTA

let smallGeneComp    = BioSeq.complement mySmallGene
//Original: ATGTTCCGAT
//Output:   TACAAGGCTA

let smallGeneRevComp = BioSeq.reverseComplement mySmallGene
//Original: ATGTTCCGAT
//Reverse:  TAGCCTTGTA
//Output:   ATCGGAACAT

(**

## AminoAcids

### Basics
Some functions which might be needed regularly are defined to work with nucleotides and amino acids:
*)

let myPeptide = "PEPTIDE" |> BioSeq.ofAminoAcidString 

(***hide***)
let myPeptidePrnt = FSIPrinters.prettyPrintBioCollection myPeptide
(*** include-value:myPeptide ***)
(**
*)

let myPeptideFormula = BioSeq.toFormula myPeptide |> Formula.toString 

(*** include-value:myPeptideFormula ***)
(**
*)

let myPeptideMass = BioSeq.toAverageMass myPeptide 

(*** include-value:myPeptideMass ***)

(**

### Digestion
BioFSharp also comes equipped with a set of tools aimed at cutting apart amino acid sequences. To demonstrate the usage, we'll throw some `trypsin` at the small RuBisCO subunit of _Arabidopos thaliana_:  
In the first step, we define our input sequence and the protease we want to use.
*)

let RBCS = 
    """MASSMLSSATMVASPAQATMVAPFNGLKSSAAFPATRKANNDITSITSNGGRVNCMQVWP
    PIGKKKFETLSYLPDLTDSELAKEVDYLIRNKWIPCVEFELEHGFVYREHGNSPGYYDGR
    YWTMWKLPLFGCTDSAQVLKEVEECKKEYPNAFIRIIGFDNTRQVQCISFIAYKPPSFT""" 
    |> BioArray.ofAminoAcidString

let trypsin = Digestion.Table.getProteaseBy "Trypsin"

(**
With these two things done, digesting the protein is a piece of cake. For doing this, just use the `digest` function.  
*)
let digestedRBCS = Digestion.BioArray.digest trypsin 0 RBCS 

(*
In reality, proteases don't always completely cut the protein down. Instead, some sites stay intact and should be considered for in silico analysis. 
This can easily be done with the `concernMissCleavages` function. It takes the minimum and maximum amount of misscleavages you want to have and also the digested protein. As a result you get all possible combinations arising from this information.
*)

let digestedRBCS' = Digestion.BioArray.concernMissCleavages 0 2 digestedRBCS

