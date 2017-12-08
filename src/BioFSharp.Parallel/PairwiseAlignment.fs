namespace BioFSharp.Parallel
open System
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

    /// Primitive version of addFloatToTrace
    [<ReflectedDefinition>]
    let private addFloatToTrace (ts:TraceScore) (opc:int) =
        ts.Value + opc

    [<Struct>]
    type CostsPrim =
        val Open : int
        val Continuation : int
        val Similarity : int[][]

        [<ReflectedDefinition>]
        new (o, c, s) = {Open = o; Continuation = c; Similarity = s} 

    /// Primitive version of diagonalCost
    [<ReflectedDefinition>]
    let private diagonalCost (cell:Cell) (item1:int) (item2:int) (costs:CostsPrim) (*(op)*) =
        //let sim' = if item1 = item2 then costs.Same else costs.NotSame
        let sim' = costs.Similarity.[item1-42].[item2-42] (*op item1 item2*)
        let mM = addFloatToTrace cell.M sim' 
        let mY = addFloatToTrace cell.Y sim' 
        let mX = addFloatToTrace cell.X sim'             
        max mM mY |> max mX |> max 0

    /// Primitive version of horizontalCost
    [<ReflectedDefinition>]
    let private horizontalCost (cell:Cell) (costs:CostsPrim) = 
        //printfn "cunt %A" openCost
        let xM = addFloatToTrace cell.M costs.Open
        let xX = addFloatToTrace cell.X costs.Continuation
        max xM xX |> max 0

    /// Primitive version of verticalCost
    [<ReflectedDefinition>]
    let private verticalCost (cell:Cell) (costs:CostsPrim) =
        let yM = addFloatToTrace cell.M costs.Open
        let yY = addFloatToTrace cell.Y costs.Continuation
        max yM yY |> max 0

    /// Primitive function for calculating the best cost.
    [<ReflectedDefinition>]
    let private bestTrace m x y seq1Char seq2Char costs op =
        let mCost = diagonalCost m seq1Char seq2Char costs(*op*)

        let xCost = horizontalCost x costs
        let x' = new TraceScore(xCost, 2uy)

        let yCost = verticalCost y costs
        let y' = new TraceScore(yCost, 3uy)

        let mutable bestCost = xCost
        let mutable bestTraceType = 2uy

        if yCost > bestCost then
            bestCost <- yCost
            bestTraceType <- 3uy

        if mCost > bestCost then
            bestCost <- mCost
            bestTraceType <- 1uy

        let newBestTrace = new TraceScore(bestCost, bestTraceType)

        new Cell(newBestTrace, x', y')
    
    /// Primitive function for calculating the max cell in a matrix.
    let private indexOfMaxInMatrix (matrix:(Cell)[,]) =
        let mutable val_best = 0
        let mutable i_best = 0
        let mutable j_best = 0
        for i in 0..(matrix.GetLength(0)-1) do
            for j in 0..(matrix.GetLength(1)-1) do

                let value = (*prim_get_trace_score_value*) matrix.[i,j].M.Value
                if value >= val_best then
                    val_best <- value
                    i_best <- i
                    j_best <- j

        i_best, j_best

    /// Primitive version of TraceBackZerovalue. Note that a gap is represented with a "-1"
    let rec private traceBackPrimitive (fstSeq:int[]) (sndSeq:int[]) (i:int) (j:int) (acc1:int list) (acc2:int list) (matrix:Cell[,]) =
        match matrix.[i,j].M.TraceType with
        |0uy -> acc1, acc2
        |1uy -> traceBackPrimitive fstSeq sndSeq (i-1) (j-1) (fstSeq.[i-1]::acc1) (sndSeq.[j-1]::acc2) matrix
        |2uy -> traceBackPrimitive fstSeq sndSeq (i)   (j-1) (-1::acc1)           (sndSeq.[j-1]::acc2) matrix
        |3uy -> traceBackPrimitive fstSeq sndSeq (i-1) (j)   (fstSeq.[i-1]::acc1) (-1::acc2)           matrix

    module Matrix =
        [<ReflectedDefinition>]
        let maxDiagCell (d:int) (rows:int) (cols:int) =
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
        let initialI d maxRow = 
            if d > maxRow then maxRow else d

        [<ReflectedDefinition>]
        let initialJ d maxRow =
            if d < maxRow then 0 else d - maxRow

        [<ReflectedDefinition>]
        let getI d m maxRow = 
            (initialI d maxRow) - m

        [<ReflectedDefinition>]
        let getJ d m maxRow = 
            (initialJ d maxRow) + m

        let kernel (op:Expr<int -> int -> int>) =
            <@ fun (matrix:Cell[,]) (fstSeq:int[]) (sndSeq:int[]) (mMaxs:int[]) (costsPrim:CostsPrim) ->

                let start = blockIdx.x * blockDim.x + threadIdx.x
                let stride = gridDim.x * blockDim.x

                let rows = matrix.GetLength(0)
                let cols = matrix.GetLength(1)

                let dMax = rows + cols

                __syncthreads()
                for d in 0..dMax do
                    let mMax = mMaxs.[d]
                    let mutable m = start
                    while m <= mMax do
                        let i = getI d m (rows - 1)
                        let j = getJ d m (rows - 1)

                        // Avoid the first row and first column of the matrix because they are reserved for the sequences in the form of 0s.
                        if i <> 0 && j <> 0 then
                            matrix.[i,j] <- bestTrace matrix.[i-1,j-1] matrix.[i,j-1] matrix.[i-1,j] fstSeq.[i-1] sndSeq.[j-1] costsPrim (%op)
                        m <- m + stride
                    __syncthreads()
                __syncthreads()
            @>

        let createCellMatrix (costs:CostsPrim) op (fstSeq:int[]) (sndSeq:int[]) =
            let kernelAddI32 : KernelDef<Cell[,] -> int[] -> int[] -> int[] -> CostsPrim -> unit> =
                kernel (*costs.Similarity*)op |> Compiler.makeKernel

            let rows, cols = fstSeq.Length + 1, sndSeq.Length + 1
            let dMax = rows + cols - 1
            let dMaxs = [|for d in 0..dMax -> d|]
            let mMaxs = dMaxs |> Array.map (fun d -> maxDiagCell d rows cols)

            let gpu = Gpu.Default
            let fstSeqGpu = gpu.Allocate(fstSeq)
            let sndSeqGpu = gpu.Allocate(sndSeq)
            let mMaxsGpu = gpu.Allocate(mMaxs)
            let matrixGpu = gpu.Allocate<Cell>(rows, cols)

            let lp = LaunchParam(1, 512)
            gpu.Launch kernelAddI32 lp matrixGpu fstSeqGpu sndSeqGpu mMaxsGpu costs
            let matrix = Gpu.CopyToHost(matrixGpu)

            Gpu.Free(fstSeqGpu)
            Gpu.Free(sndSeqGpu)
            Gpu.Free(mMaxsGpu)
            Gpu.Free(matrixGpu)
            Gpu.Free(matrixGpu)
            
            matrix

    module SmithWaterman =
        let run (costs:CostsPrim) op (fstSeq:BioArray<Nucleotide>) (sndSeq:BioArray<Nucleotide>) =
            // Primitivze
            let fstSeqPrim = fstSeq |> BioArray.toString |> Conversion.stringToInts
            let sndSeqPrim = sndSeq |> BioArray.toString |> Conversion.stringToInts

            // Generate cell matrix
            let cellMatrix = Matrix.createCellMatrix costs op fstSeqPrim sndSeqPrim

            // Get alignment from cell matrix
            let i, j = indexOfMaxInMatrix cellMatrix
            let alignment = traceBackPrimitive fstSeqPrim sndSeqPrim i j List.Empty List.Empty cellMatrix

            // Package alignment            
            alignment |> Conversion.packageAlignment