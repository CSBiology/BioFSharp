namespace BioFSharp.Parallel
open System
open Alea
open Alea.FSharp

module PairwiseAlignment =

    // Conversion functions

    /// Character array to integer array
    let private charsToInts (chars:char[]) =
        Array.map (fun c -> int c) chars

    /// Character array to string
    let private implode (chars:char[]) =
        chars |> System.String

    /// String to character array
    let private explode (my_string:string) =
        [| for i in 0..my_string.Length-1 -> char my_string.[i] |]

    /// Integer array to character array. Note how I convert my -1's to hyphens.
    let private intsToChars (ints:int[]) =
        Array.map (fun myInt -> if myInt = -1 then '-' else char myInt) ints

    /// String to integer array.
    let private stringToCharInts (my_string:string) =
        let exploded = explode my_string
        exploded |> charsToInts    
            

    // Alignment functions

    /// Primitive version of TraceScore.
    [<Struct>]
    type TraceScore =
        val Value : int
        val TraceType : byte

        // TraceType LEGEND:
        // 0 : NoTrace
        // 1 : Diagonal
        // 2 : Horizontal
        // 3 : Vertical
        
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

    /// Primitive version of AddFloatToTrace
    [<ReflectedDefinition>]
    let private AddFloatToTrace (ts:TraceScore) (opc:int) =
        ts.Value + opc

    /// Primitive version of DiagonalCost
    [<ReflectedDefinition>]
    let private diagonalCost (cell:Cell) (item1:int) (item2:int) =
        let sim' = if item1 = item2 then 5 else -4
        let mM = AddFloatToTrace cell.M sim' 
        let mY = AddFloatToTrace cell.Y sim' 
        let mX = AddFloatToTrace cell.X sim'             
        max mM mY |> max mX |> max 0

    /// Primitive version of HorizontalCost
    [<ReflectedDefinition>]
    let private horizontalCost (cell:Cell) = 
        let xM = AddFloatToTrace cell.M -5
        let xX = AddFloatToTrace cell.X -1
        max xM xX |> max 0

    /// Primitive version of VerticalCost
    [<ReflectedDefinition>]
    let private verticalCost (cell:Cell) =
        let yM = AddFloatToTrace cell.M -5
        let yY = AddFloatToTrace cell.Y -1
        max yM yY |> max 0

    /// Primitive function for calculating the best cost.
    [<ReflectedDefinition>]
    let private bestTrace m x y seq1Char seq2Char =
        let mCost = diagonalCost m seq1Char seq2Char

        let xCost = horizontalCost x
        let x' = new TraceScore(xCost, 2uy)

        let yCost = verticalCost y
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

    /// Primitive version of TraceBackZerovalue. Note how I represent a gap with a "-1"
    let rec private traceBackPrimitive (fstSeq:int[]) (sndSeq:int[]) (i:int) (j:int) (acc1:int list) (acc2:int list) (matrix:Cell[,]) =
        match matrix.[i,j].M.TraceType with
        |0uy -> acc1, acc2
        |1uy -> traceBackPrimitive fstSeq sndSeq (i-1) (j-1) (fstSeq.[i-1]::acc1) (sndSeq.[j-1]::acc2) matrix
        |2uy -> traceBackPrimitive fstSeq sndSeq (i)   (j-1) (-1::acc1)            (sndSeq.[j-1]::acc2) matrix
        |3uy -> traceBackPrimitive fstSeq sndSeq (i-1) (j)   (fstSeq.[i-1]::acc1) (-1::acc2)            matrix


    module ParallelMatrix =

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

        [<ReflectedDefinition>]
        let kernel (matrix:(Cell)[,]) (fstSeq:int[]) (sndSeq:int[]) (mMaxs:int[]) =

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
                        matrix.[i,j] <- bestTrace matrix.[i-1,j-1] matrix.[i,j-1] matrix.[i-1,j] fstSeq.[i-1] sndSeq.[j-1] 

                    m <- m + stride
                __syncthreads()
            __syncthreads()


        let transformKernel = <@ kernel @> |> Compiler.makeKernel

        let gpuCellMatrix (fst_seq:int[]) (snd_seq:int[]) =
            let rows, cols = fst_seq.Length + 1, snd_seq.Length + 1

            let d_max = rows + cols - 1
            let d_maxs = [|for d in 0..d_max -> d|]
            let m_maxs = d_maxs |> Array.map (fun d -> maxDiagCell d rows cols)

            let gpu = Gpu.Default
            let fstSeqGpu = gpu.Allocate(fst_seq)
            let sndSeqGpu = gpu.Allocate(snd_seq)
            let mMaxsGpu = gpu.Allocate(m_maxs)
            let matrixGpu = gpu.Allocate<Cell>(rows, cols)

            let lp = LaunchParam(1, 512)
            gpu.Launch transformKernel lp matrixGpu fstSeqGpu sndSeqGpu mMaxsGpu

            let matrix = Gpu.CopyToHost(matrixGpu)

            Gpu.Free(fstSeqGpu)
            Gpu.Free(sndSeqGpu)
            Gpu.Free(mMaxsGpu)
            Gpu.Free(matrixGpu)

            matrix

    module SmithWaterman =
        let run (fstSeq:string) (sndSeq:string) =
            let fstSeqPrim = fstSeq |> stringToCharInts
            let sndSeqPrim = sndSeq |> stringToCharInts
            let cell_matrix = ParallelMatrix.gpuCellMatrix fstSeqPrim sndSeqPrim
            let i, j = indexOfMaxInMatrix cell_matrix

            let alignment = traceBackPrimitive fstSeqPrim sndSeqPrim i j List.Empty List.Empty cell_matrix

            let a1 = fst alignment
            let a2 = snd alignment

            let intConversion chars = chars |> List.toArray |> intsToChars |> implode

            (a1 |> intConversion, a2 |> intConversion)