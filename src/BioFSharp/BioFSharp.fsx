#light

#r "../../bin/FSharpAux.dll"

#load "PhysicalConstants.fs"
#load "Isotopes.fs"
#load "Elements.fs"
#load "Formula.fs"
#load "ModificationInfo.fs"
#load "IBioItem.fs"
#load "AminoAcids.fs"
#load "Nucleotides.fs"
#load "BioSequences.fs"
#load "Digestion.fs"
#load "Mass.fs"

open FSharpAux
open BioFSharp

let t = BioSequences.ofAminoAcidString "ACAS"
t |> Seq.map (fun x -> AminoAcids.symbol x)
t.[1..2]

let aminoMonoMass = 
    AminoAcids.formula >> Formula.monoisoMass

let calcPeptideMass aa =
    let sum  = aa |> Seq.sumBy aminoMonoMass 
    sum + (Formula.Table.H2O |> Formula.monoisoMass)


calcPeptideMass t.[1..2]



let aminoMonoMass' = Memoization.memoize (fun a ->
    aminoMonoMass a)


let calcPeptideMass' =
    let water = Formula.Table.H2O |> Formula.monoisoMass
    (fun (aa:seq<AminoAcids.AminoAcid>) -> 
        aa 
        |> Seq.sumBy aminoMonoMass
        |> (+) water )
               
    
calcPeptideMass'  t



let arr  = Array.create 100000000 AminoAcids.Ala
let barr = BioSequences.BSequence arr

type BioSequence<[<EqualityConditionalOn; ComparisonConditionalOn >]'a when 'a :> IBioItem>  = 'a []

let f (bs:BioSequence<_>) =
    for i=0 to bs.Length-1 do
        bs.[i] |> ignore
    

f arr

#time

for i=0 to arr.Length-1 do
    1 = arr.[i]  |> ignore

for i=0 to arr.Length-1 do
    barr.[i] |> ignore

let barr' = (barr :> BioSequences.IBioSequence<_>).ToArray()
for i=0 to arr.Length-1 do
    barr'.[i] |> ignore


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
