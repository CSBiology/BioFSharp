namespace BioFSharp.Algorithms

open FSharp.Care.Collections

module ScoringMatrix =
    
    open System.Reflection
    open BioFSharp.AminoAcidSymbols


    type ScoringFunction<'a> = 'a -> 'a -> int
        


    type ScoringMatrixType =
        | BLOSUM62

        /// Converts a scoring matrix to the proper typed function (Amino acid or nucleotide)
        static member ConvertToFun = function
            | BLOSUM62  -> 
                ( fun (scm:int[][]) -> 
                    (fun  (amino1:AminoAcidSymbol) (amino2:AminoAcidSymbol) -> 
                        scm.[int amino1 - 42].[int amino2 - 42])
                )


        static member toFileName = function
            | BLOSUM62  -> "BLOSUM62.txt"

    
    let getScoringMatrix (scoringMatrixType:ScoringMatrixType) =
        let assembly = Assembly.GetExecutingAssembly()
        let resourceName = ScoringMatrixType.toFileName scoringMatrixType
        use stream = assembly.GetManifestResourceStream(resourceName)
        use reader = new System.IO.StreamReader(stream)
        
        let scmName = reader.ReadLine()
        let header  =
            (reader.ReadLine()).Split(' ')
            |> Array.map (fun c -> (int c.[0]) - 42)

        let scm =
            let outer = Array.zeroCreate 49
            [|
                while not reader.EndOfStream do                              
                    let av = Array.zeroCreate 49
                    (reader.ReadLine()).Split(' ')
                    |> Array.iteri (fun i v ->  av.[header.[i]] <-int v)
                    yield av                    
            |]
        
        ScoringMatrixType.ConvertToFun scoringMatrixType scm
//        (fun  (amino1:AminoAcidSymbol) (amino2:AminoAcidSymbol) -> 
//            scm.[int amino1 - 65].[int amino2 - 65])
