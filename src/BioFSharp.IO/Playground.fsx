#load "../BioFSharp/Playground.fsx"

#load "GAF.fs"
#load "AgilentRaw.fs"
#load "BlastWrapper.fs"
#load "BlastQueries.fs"
#load "Clustal.fs"
#load "DSSP_Stride.fs"
#load "FastA.fs"
#load "FastQ.fs"
#load "GenBank.fs"
#load "GFF3.fs"
#load "Mgf.fs"
#load "Newick.fs"
#load "Obo.fs"
#load "OrthoMCL.fs"
#load "ClustalOWrapper.fs"
#load "SOFT.fs"
#load "WorkflowLanguage.fs"
#load "FSIPrinters.fs"

open FSharpAux
open FSharpAux.IO
open FSharp.Stats
open BioFSharp
open BioFSharp.IO

open ClustalOWrapper

let cw = ClustalOWrapper (@"C:\Users\schne\Downloads\clustal-omega-1.2.2-win64\clustal-omega-1.2.2-win64\clustalo.exe")

module TestSequences =
    let sequences = 
            [
                TaggedSequence.create "seq1" (BioSeq.ofNucleotideString "ATTGTGATGCGAGCTAATTGCTTTTTTTAGCTAGCTAATCTTGAATCGATAGGCCGGCTATATACTTTTTTAAGCGGGCCCTATCGATCGGGATATCGATATTTTTAGCGGCTAATCGTGCTAATAGTAATGCCGAGTAGC")
                TaggedSequence.create "seq2" (BioSeq.ofNucleotideString "ATCGTGATACTATCTAATTGCTTTTTTTAGCTAGGTAATCTAGAATCGTTAGGCCGGCTATATATTTTTTTAAGCGGGGGATATCGATCGGGATATCGATATTTTTAGCGGCTAATCGTGCTAATCGTAAGGCCGAGTAGC")
                TaggedSequence.create "seq3" (BioSeq.ofNucleotideString "ATCGTGATGCTAGCTAATTGCTTTTATTAGCTAGCTAATCTAGAATCGATAGGCCGGCTATATTTTTTTTTAAGCGGGGGCTATCGATCGGGATATCGATATTTTTAGCGGCTAATCGTGCTAATCGTAATGCCGAGTAGC")
                TaggedSequence.create "seq4" (BioSeq.ofNucleotideString "ATCGTGATGCTAGCTAATTGCTTTTTTTAGCTGGCTAATCTAGAATCGATAGGCCGGCTATATATTTGTTTAAGCGGGGGCTATCGATCGGAATATCGATATTTTTAGCGGCTAATCGTGCTAATCGTTATGCCGAGTAGC")
                TaggedSequence.create "seq5" (BioSeq.ofNucleotideString "ATCCTGATGCTAGCTAATTGCTTTTTTTAGCTAGCTAATCTAGAATCGATAGGCCGGCTATATATTTTTTTAAGCGGGGGCTATCGATCGGGATATCGATATTTTTAGCGGCTCATCGTGCTAATCGTAATGCCGAGTAGC")
                TaggedSequence.create "seq6" (BioSeq.ofNucleotideString "ATCGTGATGCTAGCTAATTGCTGTTCTTAGCTAGCTAATCTAGAATCGATAGGCCTGCTATATATTTTTTTAAGCGGGGGCTATCGATCGGGATATCGATATTTTTAGCGGCTAATCGTGCTAATCGTAATGCCGAGTAGC")
                TaggedSequence.create "seq7" (BioSeq.ofNucleotideString "ATCGTGATGCTAGCTAATTGCTTTCCTTAGCTAGCTAATCTAGAATCGATAGGCCGGCTATATATTTTTTTAAGCGGGGGCTATCGATCGGGATATCGATATTTTTAGGGGCTAATCGTGCTAATCGTAATGCCGAGTGGC")
                TaggedSequence.create "seq8" (BioSeq.ofNucleotideString "ATCGTGATCCTAGCTAATTGCTTATTTTAGCTAGCTAATCTAGAATGGATAGGCCGGCTATATATTTTTTTAAGCGGGGGCTATCGATCGGGATATCGATATTTTTAGCGGCTAATCGTGCTAATCGTAATGCCGAGTAGC")
                TaggedSequence.create "seq9" (BioSeq.ofNucleotideString "ATCGTGATGCTAGCTAATTGCTTTTTTTAGCTAGCTAATCTAGAATCGATAGGCCGGCTATATATTTTTTTAAGCGGCGGCTATCGATCGGGATATCGATATTTTTAGCGGCTAATCGTGCTAATCGTAATGCCGAGTAGC")
                TaggedSequence.create "seq10"(BioSeq.ofNucleotideString "ATCGTGATGCTAGCTAATTGCTTGTTTTAGCTAGCTAATCTAGAATCGATAGGCCGGCTATATATTTTCTTAAGCGGGGGCTATCGATCGGGATATCGATATTATTAGCGGCTAATCGTGCTAATCGTAATGCTGAGTAGC")
            ]

    let seqA = 
        "ATTGTGAGGC" 
        |> BioSeq.ofNucleotideString
    let seqB =
        "ATCGTGATAC" 
        |> BioSeq.ofNucleotideString
    
    let p x y : float =
        let l : float = 
            x 
            |> Seq.length 
            |> float        
        let d : float =
            let x_seq = 
                x |> BioSeq.toString
            let y_seq = 
                y |> BioSeq.toString
            let mutable count = 0
            for i in 0 .. x_seq.Length - 1 do
                if x_seq.[i] <> y_seq.[i] then count <- count + 1
            count 
            |> float
        d/l
        
    let p_JC x y: float =
        let p : float =
            let l : float = 
                x 
                |> Seq.length 
                |> float        
            let d : float =
                let x_seq = 
                    x |> BioSeq.toString
                let y_seq = 
                    y |> BioSeq.toString
                let mutable count = 0
                for i in 0 .. x_seq.Length - 1 do
                    if x_seq.[i] <> y_seq.[i] then count <- count + 1
                count
                |> float
            d/l
        System.Math.Log(1.-((4./3.)*p))*(-3./4.)

    let p_K2P n m : float =
        let l : float = 
            n 
            |> Seq.length 
            |> float        
        let P : float =
            let TS : float  = 
                let n_seq = 
                    n |> BioSeq.toString
                let m_seq = 
                    m |> BioSeq.toString
                let mutable countTS = 0
                for i in 0 .. n_seq.Length - 1 do
                    if n_seq.[i] = 'A' && m_seq.[i] = 'G' then countTS <- countTS + 1
                    elif n_seq.[i] = 'G' && m_seq.[i] = 'A' then countTS <- countTS + 1
                    elif n_seq.[i] = 'C' && m_seq.[i] = 'T' then countTS <- countTS + 1
                    elif n_seq.[i] = 'T' && m_seq.[i] = 'C' then countTS <- countTS + 1
                countTS
                |> float
            TS/l
        let Q : float =
            let TV : float =
                let n_seq = 
                    n |> BioSeq.toString
                let m_seq = 
                    m |> BioSeq.toString
                let mutable countTV = 0
                for i in 0 .. n_seq.Length - 1 do
                    if n_seq.[i] = 'A' && m_seq.[i] = 'T' then countTV <- countTV + 1
                    elif n_seq.[i] = 'T' && m_seq.[i] = 'A' then countTV <- countTV + 1
                    elif n_seq.[i] = 'A' && m_seq.[i] = 'C' then countTV <- countTV + 1
                    elif n_seq.[i] = 'C' && m_seq.[i] = 'A' then countTV <- countTV + 1
                    elif n_seq.[i] = 'G' && m_seq.[i] = 'T' then countTV <- countTV + 1
                    elif n_seq.[i] = 'T' && m_seq.[i] = 'G' then countTV <- countTV + 1
                    elif n_seq.[i] = 'G' && m_seq.[i] = 'C' then countTV <- countTV + 1
                    elif n_seq.[i] = 'C' && m_seq.[i] = 'G' then countTV <- countTV + 1
                countTV
                |> float
            TV/l
        System.Math.Log(1.-(2.*P)-Q)*(-1./2.)-System.Math.Log(1.-(2.*Q))*(1./4.)

open TestSequences

let myTree =
    PhylogeneticTree.ofTaggedBioSequences
        p
        sequences

myTree
|> Newick.toFile (fun (s:TaggedSequence<string,Nucleotides.Nucleotide>,d:float) -> s.Tag, string d) @"C:\Users\schne\Desktop\test.newick"