namespace BioFSharp.Parallel
open System
open BioFSharp.BioArray
open BioFSharp.Algorithm.PairwiseAlignment
open BioFSharp.Algorithm.ScoringMatrix
open BioFSharp
open Nucleotides
open Alea
open Alea.FSharp

 module private Conversion =
    /// Character array to integer array
    let charsToInts (chars:char[]) =
        Array.map (fun c -> int c) chars

    /// Character array to string
    let implode (chars:char[]) =
        chars |> System.String

    /// String to character array
    let explode (my_string:string) =
        [| for i in 0..my_string.Length-1 -> char my_string.[i] |]

    /// Integer array to character array. Note how I convert my -1's to hyphens.
    let intsToChars (ints:int[]) =
        Array.map (fun myInt -> if myInt = -1 then '-' else char myInt) ints

    /// String to integer array.
    let stringToCharInts (my_string:string) =
        let exploded = explode my_string
        exploded |> charsToInts    



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

    /////creates a scoring function for nucleotides out of a scoring matrix
    //let getScoringMatrixNucleotide (scoringMatrixType:ScoringMatrixNucleotide) =
    //    let resourceName = ScoringMatrixNucleotide.toFileName scoringMatrixType
    //    let scm = readScoringMatrix resourceName

    //    (fun  (n1:int) (n2:int) -> 
    //        scm.[n1 - 42].[n2 - 42])


    /// Primitive version of diagonalCost
    [<ReflectedDefinition>]
    let private diagonalCost (cell:Cell) (item1:int) (item2:int) =
        let sim' = if item1 = item2 then 5 else -4
        let mM = addFloatToTrace cell.M sim' 
        let mY = addFloatToTrace cell.Y sim' 
        let mX = addFloatToTrace cell.X sim'             
        max mM mY |> max mX |> max 0

    /// Primitive version of horizontalCost
    [<ReflectedDefinition>]
    let private horizontalCost (cell:Cell) (openCost:int) (continueCost:int) = 
        //printfn "cunt %A" openCost
        let xM = addFloatToTrace cell.M openCost
        let xX = addFloatToTrace cell.X continueCost
        max xM xX |> max 0

    /// Primitive version of verticalCost
    [<ReflectedDefinition>]
    let private verticalCost (cell:Cell) (openCost:int) (continueCost:int) =
        let yM = addFloatToTrace cell.M openCost
        let yY = addFloatToTrace cell.Y continueCost
        max yM yY |> max 0


    /// Primitive function for calculating the best cost.
    [<ReflectedDefinition>]
    let private bestTrace m x y seq1Char seq2Char openCost continueCost =
        let mCost = diagonalCost m seq1Char seq2Char

        let xCost = horizontalCost x openCost continueCost
        let x' = new TraceScore(xCost, 2uy)

        let yCost = verticalCost y openCost continueCost
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
        let kernel (matrix:(Cell)[,]) (fstSeq:int[]) (sndSeq:int[]) (mMaxs:int[]) (openCost:int) (continueCost:int) =

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
                        matrix.[i,j] <- bestTrace matrix.[i-1,j-1] matrix.[i,j-1] matrix.[i-1,j] fstSeq.[i-1] sndSeq.[j-1] openCost continueCost
                    m <- m + stride
                __syncthreads()
            __syncthreads()


        let transformKernel = <@ kernel @> |> Compiler.makeKernel


        let gpuCellMatrix (fstSeq:int[]) (sndSeq:int[]) (openCost:int) (continueCost:int) =
            let rows, cols = fstSeq.Length + 1, sndSeq.Length + 1
            let dMax = rows + cols - 1
            let dMaxs = [|for d in 0..dMax -> d|]
            let mMaxs = dMaxs |> Array.map (fun d -> maxDiagCell d rows cols)

            let gpu = Gpu.Default

            //let openGpu = gpu.Allocate<int32>(openCost)
            //let continueGpu = gpu.Allocate<int32>(continueCost)
            let fstSeqGpu = gpu.Allocate(fstSeq)
            let sndSeqGpu = gpu.Allocate(sndSeq)
            let mMaxsGpu = gpu.Allocate(mMaxs)
            let matrixGpu = gpu.Allocate<Cell>(rows, cols)

            let lp = LaunchParam(1, 512)
            gpu.Launch transformKernel lp matrixGpu fstSeqGpu sndSeqGpu mMaxsGpu openCost continueCost
            let matrix = Gpu.CopyToHost(matrixGpu)

            Gpu.Free(fstSeqGpu)
            Gpu.Free(sndSeqGpu)
            Gpu.Free(mMaxsGpu)
            Gpu.Free(matrixGpu)
            //Gpu.Free(openGpu)
            Gpu.Free(matrixGpu)
            
            matrix

    module SmithWaterman =
        let run (costs:Costs<Nucleotide>) (fstSeq:BioArray<Nucleotide>) (sndSeq:BioArray<Nucleotide>) =
            // Primitivze
            let fstSeqPrim = fstSeq |> BioArray.toString |> Conversion.stringToCharInts
            let sndSeqPrim = sndSeq |> BioArray.toString |> Conversion.stringToCharInts
            
            let openCost = costs.Open
            let continueCost = costs.Continuation

            // Generate cell matrix
            let cellMatrix = ParallelMatrix.gpuCellMatrix fstSeqPrim sndSeqPrim openCost continueCost

            // Get alignment from cell matrix
            let i, j = indexOfMaxInMatrix cellMatrix
            let alignment = traceBackPrimitive fstSeqPrim sndSeqPrim i j List.Empty List.Empty cellMatrix

            // Package alignment
            let a1 = fst alignment
            let a2 = snd alignment
            let intConversion chars = chars |> List.toArray |> Conversion.intsToChars |> Conversion.implode
            (a1 |> intConversion, a2 |> intConversion)