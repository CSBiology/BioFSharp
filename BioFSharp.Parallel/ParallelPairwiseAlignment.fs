namespace BioFSharp.Parallel
open System
open Alea
open Alea.FSharp

module ParallelPairwiseAlignment =

    // Conversion functions

    /// Character array to integer array
    let chars_to_ints (chars:char[]) =
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
    let string_to_char_ints (my_string:string) =
        let exploded = explode my_string
        exploded |> chars_to_ints    
            

    // Alignment functions

    /// Primitive version of TraceScore.
    [<Struct>]
    type prim_trace_score =
        val Value : int
        val Trace_type : byte

        // TRACE_TYPE LEGEND:
        // 0 : NoTrace
        // 1 : Diagonal
        // 2 : Horizontal
        // 3 : Vertical
        
        [<ReflectedDefinition>]
        new (value, trace_type) = {Value = value; Trace_type = trace_type}

    /// Primitive version of Cell.
    [<Struct>]
    type prim_cell = 
        val M : prim_trace_score
        val X : prim_trace_score
        val Y : prim_trace_score

        [<ReflectedDefinition>]
        new (m, x, y) = {M = m; X = x; Y = y}

    /// Primitive version of AddFloatToTrace
    [<ReflectedDefinition>]
    let prim_add_float_to_trace (ts:prim_trace_score) (opc:int) =
        ts.Value + opc

    /// Primitive version of DiagonalCost
    [<ReflectedDefinition>]
    let diagonal_cost (cell:prim_cell) (item1:int) (item2:int) =
        let sim' = if item1 = item2 then 5 else -4
        let mM = prim_add_float_to_trace cell.M sim' 
        let mY = prim_add_float_to_trace cell.Y sim' 
        let mX = prim_add_float_to_trace cell.X sim'             
        max mM mY |> max mX |> max 0

    /// Primitive version of HorizontalCost
    [<ReflectedDefinition>]
    let horizontal_cost (cell:prim_cell) = 
        let xM = prim_add_float_to_trace cell.M -5
        let xX = prim_add_float_to_trace cell.X -1
        max xM xX |> max 0

    /// Primitive version of VerticalCost
    [<ReflectedDefinition>]
    let vertical_cost (cell:prim_cell) =
        let yM = prim_add_float_to_trace cell.M -5
        let yY = prim_add_float_to_trace cell.Y -1
        max yM yY |> max 0

    /// Primitive function for calculating the best cost.
    [<ReflectedDefinition>]
    let prim_best_trace m x y seq1_char seq2_char =
        let mCost = diagonal_cost m seq1_char seq2_char

        let xCost = horizontal_cost x
        let x' = new prim_trace_score(xCost, 2uy)

        let yCost = vertical_cost y
        let y' = new prim_trace_score(yCost, 3uy)

        let mutable bestCost = xCost
        let mutable bestTraceType = 2uy

        if yCost > bestCost then
            bestCost <- yCost
            bestTraceType <- 3uy

        if mCost > bestCost then
            bestCost <- mCost
            bestTraceType <- 1uy

        let new_best_trace = new prim_trace_score(bestCost, bestTraceType)

        new prim_cell(new_best_trace, x', y')
    
    /// Primitive function for calculating the max cell in a matrix.
    let index_of_max_in_matrix_TRACE (matrix:(prim_cell)[,]) =
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
    let rec traceBackPrimitive (fst_seq:int[]) (snd_seq:int[]) (i:int) (j:int) (acc1:int list) (acc2:int list) (matrix:prim_cell[,]) =
        match matrix.[i,j].M.Trace_type with
        |0uy -> acc1, acc2
        |1uy -> traceBackPrimitive fst_seq snd_seq (i-1) (j-1) (fst_seq.[i-1]::acc1) (snd_seq.[j-1]::acc2) matrix
        |2uy -> traceBackPrimitive fst_seq snd_seq (i)   (j-1) (-1::acc1)            (snd_seq.[j-1]::acc2) matrix
        |3uy -> traceBackPrimitive fst_seq snd_seq (i-1) (j)   (fst_seq.[i-1]::acc1) (-1::acc2)            matrix


    module ParallelMatrix =

        [<ReflectedDefinition>]
        let max_diag_cell (d:int) (rows:int) (cols:int) =
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
        let initial_i d max_row = 
            if d > max_row then max_row else d

        [<ReflectedDefinition>]
        let initial_j d max_row =
            if d < max_row then 0 else d - max_row

        [<ReflectedDefinition>]
        let get_i d m max_row = 
            (initial_i d max_row) - m

        [<ReflectedDefinition>]
        let get_j d m max_row = 
            (initial_j d max_row) + m

        [<ReflectedDefinition>]
        let kernel (matrix:(prim_cell)[,]) (fst_seq:int[]) (snd_seq:int[]) (m_maxs:int[]) =

            let start = blockIdx.x * blockDim.x + threadIdx.x
            let stride = gridDim.x * blockDim.x

            let rows = matrix.GetLength(0)
            let cols = matrix.GetLength(1)

            let d_max = rows + cols

            __syncthreads()
            for d in 0..d_max do
                let m_max = m_maxs.[d]
                let mutable m = start
                while m <= m_max do
                    let i = get_i d m (rows - 1)
                    let j = get_j d m (rows - 1)

                    // Avoid the first row and first column of the matrix because they are reserved for the sequences in the form of 0s.
                    if i <> 0 && j <> 0 then
                        matrix.[i,j] <- prim_best_trace matrix.[i-1,j-1] matrix.[i,j-1] matrix.[i-1,j] fst_seq.[i-1] snd_seq.[j-1] 

                    m <- m + stride
                __syncthreads()
            __syncthreads()


        let transform_kernel = <@ kernel @> |> Compiler.makeKernel

        let gpu_cell_matrix (fst_seq:int[]) (snd_seq:int[]) =
            let rows, cols = fst_seq.Length + 1, snd_seq.Length + 1

            let d_max = rows + cols - 1
            let d_maxs = [|for d in 0..d_max -> d|]
            let m_maxs = d_maxs |> Array.map (fun d -> max_diag_cell d rows cols)

            let gpu = Gpu.Default
            let fst_seq_gpu = gpu.Allocate(fst_seq)
            let snd_seq_gpu = gpu.Allocate(snd_seq)
            let m_maxs_gpu = gpu.Allocate(m_maxs)
            let matrix_gpu = gpu.Allocate<prim_cell>(rows, cols)

            let lp = LaunchParam(1, 512)
            gpu.Launch transform_kernel lp matrix_gpu fst_seq_gpu snd_seq_gpu m_maxs_gpu

            let matrix = Gpu.CopyToHost(matrix_gpu)

            Gpu.Free(fst_seq_gpu)
            Gpu.Free(snd_seq_gpu)
            Gpu.Free(m_maxs_gpu)
            Gpu.Free(matrix_gpu)

            matrix

    module ParallelSmithWaterman =
        let run (fstSeq:string) (sndSeq:string) =
            let fstSeqPrim = fstSeq |> string_to_char_ints
            let sndSeqPrim = sndSeq |> string_to_char_ints
            let cell_matrix = ParallelMatrix.gpu_cell_matrix fstSeqPrim sndSeqPrim
            let i, j = index_of_max_in_matrix_TRACE cell_matrix

            let alignment = traceBackPrimitive fstSeqPrim sndSeqPrim i j List.Empty List.Empty cell_matrix

            let a1 = fst alignment
            let a2 = snd alignment

            let intConversion chars = chars |> List.toArray |> intsToChars |> implode

            (a1 |> intConversion, a2 |> intConversion)