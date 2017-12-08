#r "System.Configuration.dll"
#I "../../bin"
#r "Alea.dll"
#r "BioFSharp.Parallel.dll"
#r "BioFSharp.dll"

open System
open Microsoft.FSharp.Quotations

// sequential
open BioFSharp
open BioFSharp.Algorithm
open BioFSharp.Algorithm.PairwiseAlignment

// parallel
open Alea
open BioFSharp.Parallel
Alea.Settings.Instance.Resource.AssemblyPath <- __SOURCE_DIRECTORY__ + @"\..\..\packages\Alea\tools\"
Alea.Settings.Instance.Resource.Path <- __SOURCE_DIRECTORY__ + @"..\..\bin"

let executeSequential nucCosts seq1 seq2 =
    let nucConversion (nucs:Nucleotides.Nucleotide list) =
        nucs |> List.map (fun element -> if string element = "Gap" then "-" else string element) |> String.Concat
    let t = System.Diagnostics.Stopwatch.StartNew()
    let sequentialAlignment = PairwiseAlignment.SmithWaterman.runNucleotide nucCosts seq1 seq2
    t.Stop()
    printfn "execution time of sequential: %A" t.Elapsed
    (sequentialAlignment.AlignedSequences.[0] |> nucConversion, sequentialAlignment.AlignedSequences.[1] |> nucConversion)

let executeParallel nucCostsPrimitive op seq1 seq2 =
    let t = System.Diagnostics.Stopwatch.StartNew()
    let parallelAlignment = PairwiseAlignment.SmithWaterman.run nucCostsPrimitive op seq1 seq2
    t.Stop()
    printfn "execution time of parallel: %A" t.Elapsed
    parallelAlignment

let random = System.Random()
let randomSequence length =
    let randomNucleotide () =
        [|'A'; 'C'; 'G'; 'T'|].[random.Next(4)]
    System.String [| for i in 0..length-1 -> randomNucleotide () |]

let seq1 = randomSequence 5 |> BioArray.ofNucleotideString
let seq2 = randomSequence 5 |> BioArray.ofNucleotideString

let nucCosts =
    {
        Open = -5
        Continuation = -1
        Similarity = ScoringMatrix.getScoringMatrixNucleotide  ScoringMatrix.ScoringMatrixNucleotide.EDNA 
    }

let op = <@ fun (n1:int) (n2:int) -> if n1 = n2 then 5 else -4 @>
let op2 = ScoringMatrix.getScoringMatrixNucleotidePrim  ScoringMatrix.ScoringMatrixNucleotide.EDNA
let op3 = <@ fun (n1:int) (n2:int) -> op2.[n1-42].[n2-42]  @>     //(<@ fun (n1:int) (n2:int) -> scm.[n1 - 42].[n2 - 42] @>)
        //(fun  (n1:int) (n2:Nucleotides.Nucleotide) -> 
            //scm.[int n1 - 42].[int n2 - 42])

let nucCostsPrim = new PairwiseAlignment.CostsPrim(-5, -1, op2)

let sequentialAlignment = executeSequential nucCosts seq1 seq2
let parallelAlignment = executeParallel nucCostsPrim op3 seq1 seq2

printfn "sequential: %A" sequentialAlignment
printfn "parallel: %A" parallelAlignment
printfn "alignments equal: %A" (parallelAlignment = sequentialAlignment)