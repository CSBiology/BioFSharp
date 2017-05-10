namespace BioFSharp.Algorithms

open FSharp.Care.Collections

module ScoringMatrix =
    
    open System.Reflection
    open BioFSharp.AminoAcidSymbols
    open BioFSharp


    type ScoringMatrixAminoAcid =
        | BLOSUM62
        

        static member toFileName = function
            | BLOSUM62  -> "BLOSUM62.txt"

    type ScoringMatrixNucleotide =
        | BLOSUM62
        

        static member toFileName = function
            | BLOSUM62  -> "BLOSUM62.txt"
            
    
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



    let getScoringMatrixAminoAcid (scoringMatrixType:ScoringMatrixAminoAcid) =
        let resourceName = ScoringMatrixAminoAcid.toFileName scoringMatrixType
        let scm = readScoringMatrix resourceName

        (fun  (amino1:AminoAcidSymbol) (amino2:AminoAcidSymbol) -> 
            scm.[int amino1 - 42].[int amino2 - 42])


    let getScoringMatrixNucleotide (scoringMatrixType:ScoringMatrixNucleotide) =
        let resourceName = ScoringMatrixNucleotide.toFileName scoringMatrixType
        let scm = readScoringMatrix resourceName

        (fun  (n1:Nucleotides.Nucleotide) (n2:Nucleotides.Nucleotide) -> 
            scm.[int n1 - 42].[int n1 - 42])

