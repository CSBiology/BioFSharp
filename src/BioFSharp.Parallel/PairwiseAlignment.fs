namespace BioFSharp.Parallel
open System
open FSharp.Care.Collections
open BioFSharp.BioArray
open BioFSharp.Algorithm.PairwiseAlignment
open BioFSharp.Algorithm.ScoringMatrix
open Microsoft.FSharp.Quotations
open BioFSharp
open Nucleotides
open Alea
open Alea.FSharp

 module private Conversion =
    /// Character array to integer array
    let charsToInts (chars:char[]) =
        Array.map (fun c -> int c) chars

    /// String to character array
    let explode (string:String) =
        [| for i in 0..string.Length-1 -> char string.[i] |]

    /// String to integer array.
    let stringToInts (string:String) =
        string |> explode |> charsToInts

    /// Integer array to character array. Note how -1s are converted to hyphens.
    let intsToChars (ints:int[]) =
        Array.map (fun x -> if x = -1 then '-' else char x) ints

    /// Packages an alignment into standard form.
    let packageAlignment (alignment) =
        (fst alignment |> List.toArray |> intsToChars |> String, snd alignment |> List.toArray |> intsToChars |> String)

module PairwiseAlignment =
    /// Primitive version of TraceScore.
    /// TraceType Legend:
    /// 0 : NoTrace
    /// 1 : Diagonal
    /// 2 : Horizontal
    /// 3 : Vertical
    [<Struct>]
    type TraceScore =
        val Value : int
        val TraceType : byte

        [<ReflectedDefinition>]
        new (value, traceType) = {Value = value; TraceType = traceType}

    /// Primitive version of Cell.
    [<Struct>]
    type Cell = 
        val M : TraceScore
        val X : TraceScore
        val Y : TraceScore

        [<ReflectedDefinition>]
        new (m, x, y) = {M = m; X = x; Y = y}

    [<Struct>]
    type Costs =
        val Open : int
        val Continuation : int
        val ScoringMatrix : int[][]

        [<ReflectedDefinition>]
        new (openCost, continuationCost, scoringMatrix) = {Open = openCost; Continuation = continuationCost; ScoringMatrix = scoringMatrix} 
    
    /// Primitive version of TraceBackZerovalue. Note that a gap is represented with a "-1"
    let traceBack (fstSeq:int[]) (sndSeq:int[]) (i:int) (j:int) (matrix:Cell[,]) =
        let rec recursiveTraceBack (i:int) (j:int) (acc1:int list) (acc2:int list) =
            match matrix.[i,j].M.TraceType with
            |0uy -> acc1, acc2
            |1uy -> recursiveTraceBack (i-1) (j-1) (fstSeq.[i-1]::acc1) (sndSeq.[j-1]::acc2)
            |2uy -> recursiveTraceBack (i)   (j-1) (-1::acc1)           (sndSeq.[j-1]::acc2)
            |3uy -> recursiveTraceBack (i-1) (j)   (fstSeq.[i-1]::acc1) (-1::acc2)

        recursiveTraceBack i j List.Empty List.Empty

    /// Calculates a new best cell based on the three previous adjacent cells, the current sequence characters, and the costs.
    [<ReflectedDefinition>]
    let private bestTrace (m:Cell) (x:Cell) (y:Cell) (seq1Char:int) (seq2Char:int) (costs:Costs) (negativeScoring:int->int) =
        let diagonalCost (cell:Cell) =
            let sim' = costs.ScoringMatrix.[seq1Char - 42].[seq2Char - 42]
            let mM = cell.M.Value + sim' 
            let mY = cell.Y.Value + sim' 
            let mX = cell.X.Value + sim'
            max mX mY |> max mM |> negativeScoring 

        let horizontalCost (cell:Cell) = 
            let xM = cell.M.Value + costs.Open
            let xX = cell.X.Value + costs.Continuation
            max xX xM |> negativeScoring

        let verticalCost (cell:Cell) =
            let yM = cell.M.Value + costs.Open
            let yY = cell.Y.Value + costs.Continuation
            max yY yM |> negativeScoring

        let mCost = diagonalCost m
        let xCost = horizontalCost x
        let yCost = verticalCost y

        let mutable bestCost = xCost
        let mutable bestTraceType = 2uy
        if yCost > bestCost then
            bestCost <- yCost
            bestTraceType <- 3uy
        if mCost > bestCost then
            bestCost <- mCost
            bestTraceType <- 1uy

        let best = new TraceScore(bestCost, bestTraceType)
        let x' = new TraceScore(xCost, 2uy)
        let y' = new TraceScore(yCost, 3uy)
        new Cell(best, x', y')

    [<ReflectedDefinition>]
    let private maxDiagCell (d:int) (rows:int) (cols:int) =
        let mutable l = 0
        let mutable a = 0
        if rows > cols then
            l <- rows
            a <- cols
        else
            l <- cols
            a <- rows
        let mutable b = rows - cols
        if b < 0 then b <- b * -1
        if d <= a - 1 then
            d
        else if d > a - 1 && d <= a - 1 + b then
            a - 1
        else
            -d + 2 * l - 2 - b

    [<ReflectedDefinition>]
    let private getI d m maxRow = 
        let initialI = if d > maxRow then maxRow else d
        initialI - m

    [<ReflectedDefinition>]
    let private getJ d m maxRow = 
        let initialJ = if d < maxRow then 0 else d - maxRow
        initialJ + m

    let private kernel (negativeScoring:Expr<int -> int>) =
        <@ fun (matrix:Cell[,]) (costs:Costs) (fstSeq:int[]) (sndSeq:int[]) (mMaxs:int[]) ->
            let start = blockIdx.x * blockDim.x + threadIdx.x
            let stride = gridDim.x * blockDim.x

            let rows = matrix.GetLength(0)
            let cols = matrix.GetLength(1)

            for d in 0..(rows + cols) do
                let mMax = mMaxs.[d]
                let mutable m = start
                while m <= mMax do
                    let i = getI d m (rows - 1)
                    let j = getJ d m (rows - 1)

                    // Avoid the first row and first column of the matrix because they are reserved for the sequences in the form of 0s.
                    if i <> 0 && j <> 0 then
                        matrix.[i,j] <- bestTrace matrix.[i-1,j-1] matrix.[i,j-1] matrix.[i-1,j] fstSeq.[i-1] sndSeq.[j-1] costs (%negativeScoring)
                    m <- m + stride
                __syncthreads()
        @>

    let createCellMatrix (initMatrix:Cell[,] -> Cell[,]) (costs:Costs) (fstSeq:int[]) (sndSeq:int[]) (negativeScoring:Expr<int->int>) =
        let rows, cols = fstSeq.Length + 1, sndSeq.Length + 1
        let dMax = rows + cols - 1
        let dMaxs = [|for d in 0..dMax -> d|]
        let mMaxs = dMaxs |> Array.map (fun d -> maxDiagCell d rows cols)

        let emptyTrace = new TraceScore(0, 0uy)
        let emptyCell = new Cell(emptyTrace, emptyTrace, emptyTrace)
        let matrix = (Array2D.create rows cols emptyCell) |> initMatrix

        let gpu = Gpu.Default
        let fstSeqGpu = gpu.Allocate(fstSeq)
        let sndSeqGpu = gpu.Allocate(sndSeq)
        let mMaxsGpu = gpu.Allocate(mMaxs)
        let matrixGpu = gpu.Allocate<Cell>(matrix)

        let lp = LaunchParam(1, 512)
        gpu.Launch (kernel negativeScoring |> Compiler.makeKernel) lp matrixGpu costs fstSeqGpu sndSeqGpu mMaxsGpu

        let matrix = Gpu.CopyToHost(matrixGpu)
        Gpu.Free(fstSeqGpu)
        Gpu.Free(sndSeqGpu)
        Gpu.Free(mMaxsGpu)
        Gpu.Free(matrixGpu)
        Gpu.Free(matrixGpu)
            
        matrix

    module NeedlemanWunsch =
        let run (costs:Costs) (fstSeq:BioArray<Nucleotide>) (sndSeq:BioArray<Nucleotide>) =
            // Primitivze
            let fstSeq = fstSeq |> BioArray.toString |> Conversion.stringToInts
            let sndSeq = sndSeq |> BioArray.toString |> Conversion.stringToInts

            ///In this step the Cells (i=0) and (j=0) of the matrix get filled with the gap-Values
            let initFirstRowCol (matrix: Cell[,]) =
                let len1 = Array2D.length1 matrix
                let len2 = Array2D.length2 matrix
                // init first col (only vertical trace)
                for i=1 to len1-1 do 
                    let currentTrace = new TraceScore(costs.Open + (costs.Continuation * int (i-1)), 3uy)
                    matrix.[i,0] <- new Cell(currentTrace, currentTrace, currentTrace)
                // init first row (only horizontal trace)
                for j=1 to len2-1 do 
                    let currentTrace = new TraceScore(costs.Open + (costs.Continuation * int (j-1)), 2uy)
                    matrix.[0,j] <- new Cell(currentTrace, currentTrace, currentTrace)
                matrix

            // Generate cell matrix
            let matrix = createCellMatrix initFirstRowCol costs fstSeq sndSeq <@id@>

            // Get alignment from cell matrix
            let i, j = Array2D.length1 matrix - 1, Array2D.length2 matrix - 1
            let alignment = traceBack fstSeq sndSeq i j matrix

            // Package alignment            
            alignment |> Conversion.packageAlignment

    module SmithWaterman =
        let run (costs:Costs) (fstSeq:BioArray<Nucleotide>) (sndSeq:BioArray<Nucleotide>) =
            // Primitivze
            let fstSeq = fstSeq |> BioArray.toString |> Conversion.stringToInts
            let sndSeq = sndSeq |> BioArray.toString |> Conversion.stringToInts

            // Generate cell matrix
            let matrix = createCellMatrix id costs fstSeq sndSeq <@ fun x -> max x 0 @>

            // Get alignment from cell matrix
            let i, j = Array2D.indexMaxBy (fun (c:Cell) -> c.M.Value) matrix
            let alignment = traceBack fstSeq sndSeq i j matrix

            // Package alignment            
            alignment |> Conversion.packageAlignment