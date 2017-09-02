namespace BioFSharp.Algorithm

open FSharp.Care.Collections

///Contains functions for using the included similarity matrices. These assign a score to every pair of aminoacids/nucleotides and therefore rate the probability of their substitution. The Scoring Matrices are generally used for alignments.
module ScoringMatrix =
    
    open System.Reflection
    open BioFSharp.AminoAcidSymbols
    open BioFSharp

    ///Union case of implemented amino acid scoring matrices with the given reference to its place in the library. Use the "getScoringMatrixAminoAcid" function to obtain a simple mapping function for every amino acid pair
    type ScoringMatrixAminoAcid =
        | BLOSUM45
        | BLOSUM50
        | BLOSUM62
        | BLOSUM80
        | PAM30
        | PAM70
        | PAM250

        static member toFileName = function
            | BLOSUM45  -> "BLOSUM45.txt"
            | BLOSUM50  -> "BLOSUM50.txt"
            | BLOSUM62  -> "BLOSUM62.txt"
            | BLOSUM80  -> "BLOSUM80.txt"
            | PAM30     -> "PAM30.txt"
            | PAM70     -> "PAM70.txt"
            | PAM250    -> "PAM250.txt"
    ///Union case of implemented nucleotide scoring matrices with the given reference to its place in the library. Use the "getScoringMatrixNucleotide" function to obtain a simple mapping function for every nucleotide pair
    type ScoringMatrixNucleotide =
        | EDNA
        | Default

        static member toFileName = function
            | EDNA      -> "EDNA.txt"
            | Default   -> "Default.txt"
            
    
    let private readScoringMatrix resourceName =
        let assembly = Assembly.GetExecutingAssembly()
        //let resourceName = ScoringMatrixType.toFileName scoringMatrixType
        use stream = assembly.GetManifestResourceStream(resourceName)
        use reader = new System.IO.StreamReader(stream)
        
        let scmName = reader.ReadLine()
        let header  =
            (reader.ReadLine()).Split(' ')
            |> Array.filter (fun c -> c <> "")
            |> Array.map (fun c -> (int c.[0]) - 42)

        let scm =
            Array.init 49 (fun _ -> Array.zeroCreate 49)

        seq [
            while not reader.EndOfStream do                              
                yield reader.ReadLine() ]
        |> Seq.iteri (fun i line -> 
            let av = scm.[header.[i]]
            line.Split(' ')
            |> Array.filter (fun c -> c <> "")
            |> Array.iteri (fun i v ->  av.[header.[i]] <-int v)
            scm.[header.[i]] <- av)

        scm


    ///creates a scoring function for amino acids out of a scoring matrix
    let getScoringMatrixAminoAcid (scoringMatrixType:ScoringMatrixAminoAcid) =
        let resourceName = ScoringMatrixAminoAcid.toFileName scoringMatrixType
        let scm = readScoringMatrix resourceName

        (fun  (amino1:AminoAcidSymbol) (amino2:AminoAcidSymbol) -> 
            scm.[int amino1 - 42].[int amino2 - 42])

    ///creates a scoring function for nucleotides out of a scoring matrix
    let getScoringMatrixNucleotide (scoringMatrixType:ScoringMatrixNucleotide) =
        let resourceName = ScoringMatrixNucleotide.toFileName scoringMatrixType
        let scm = readScoringMatrix resourceName

        (fun  (n1:Nucleotides.Nucleotide) (n2:Nucleotides.Nucleotide) -> 
            scm.[int n1 - 42].[int n2 - 42])

