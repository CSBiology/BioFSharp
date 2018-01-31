namespace BioFSharp.Algorithm

open BioFSharp
open Alignment
open FSharp.Care.Collections
open AminoAcidSymbols
open AminoAcids
open Nucleotides
open BioArray

///Contains functions for evaluating the best possible alignments for 2 Sequences
module PairwiseAlignment =
    
     //Introduction!
     //This page contains functions for evaluating the best possible alignments for 2 Sequences. Both the NeedlemanWunsch(NW)- and the SmithWaterman(SW)-algorithm are implemented by using an affine gapPenalty. For Understaning this Implementation, it is necessary to know about the basic operation of either the NW- or SW-algorithm and have an understanding of the Affine-Gap-penalty (3-submatrix-matrix)

    
    //In this module are the typedefinitons. There is also the runGeneric-Function. It is the skeleton-function for creating the Backtrace-matrix. As parameters it takes the 2 sequences and the operation-cost-functions ( opcs) and basically just creates a loop, enabling the opcs to evaluate the values of the cells. There also is the backtrace-function. It creates the alignment from the matrix and the 2 initial sequences.
    //The NW-Module and the SW-module
    //The two modules contain the opcs. The difference in these two is, that the SW-opcs check if the value is 0 or below. Also the backtrace does not start with the last cell of the matrix but at the one with the biggest value. This leads to the alignment being restrained to a local area, where there are the most matches.



    ///Represents one element of the matrix, contains direction for backtracing and score
    type private TraceScore =     
        | Diagonal   of int
        | Horizontal of int
        | Vertical   of int
        | NoTrace

    ///Get score of matrix element
    let private getTraceScoreValue = function
        | Diagonal   x -> x
        | Horizontal x -> x
        | Vertical   x -> x
        | NoTrace      -> 0

    ///Adds value to score of tracescore element
    let private addFloatToTrace (ts:TraceScore) (opc:int) =
        getTraceScoreValue ts + opc

    ///3 seperate matrices are used for evaluating the affine gap penalty. This is implemented by having every cell of the matrix carry 3 separate tracescore values
    type private Cell = {
        M : TraceScore
        X : TraceScore
        Y : TraceScore
        }
    
    ///Uses 3 TraceScores and creates a Cell from them
    let private createCell m x y =
        {M=m;X=x;Y=y;}

    ///Score of the alignment and the aligned sequences as a tuple
    type Score = int

    ///Merges the alignment score and the aligned sequences
    let private createAlignment score (sequence1,sequence2) : Alignment<'T,Score> =
        {
        AlignedSequences = [sequence1;sequence2]; 
        MetaData = score }
    
    ///Carries the costs for gaps and the scoring matrix (Similarity)
    type Costs<'a> = {
        Open : int
        Continuation : int
        Similarity : 'a -> 'a -> int
        }
    
    ///Carries the functions for evaluating the Tracescores
    type private OperationCosts<'a> = {
        DiagonalCost   : Cell -> 'a -> 'a -> int
        HorizontalCost : Cell -> int
        VerticalCost   : Cell -> int
        }

    /// Creates an jagged array matrix and fills it with Backtracing Information (Cells)
    let private runGeneric (initArray: Cell[,] -> Cell[,]) (fstSeq : 'a[]) (sndSeq : 'a[]) (opc:OperationCosts<'a>) = 
    
        let emptyCell = { M = NoTrace; Y = NoTrace; X = NoTrace}

        let len1,len2 = fstSeq.Length, sndSeq.Length

        let array =
            //The array gets created
            Array2D.create (len1+1) (len2+1) emptyCell
            //In the first step. The first row and column get filled
            |> initArray 


        //In this loop the rest of the Matrix gets filled. The opc-functions are filled in with the NeedlemanWunsch- or the SmithWaterman-subfunctions
        for i in 1..len1 do
            for j in 1..len2 do            
                let m' = opc.DiagonalCost   array.[i-1,j-1] fstSeq.[i-1] sndSeq.[j-1] |> Diagonal
                let x' = opc.HorizontalCost array.[i,j-1] |> Horizontal
                let y' = opc.VerticalCost   array.[i-1,j] |> Vertical
                //TODO: Check performance with maxBy2 as an inline function
                /////Evaluates the bigger of two values (x y) after converting them with a function f
                //let inline maxBy2 f x y =
                //    if f x < f y then y else x
                //let currentTrace = 
                //     createCell (maxBy2 getTraceScoreValue y' m'|> maxBy2 getTraceScoreValue x')
                //            x' y'
                let best = List.maxBy (fun trace -> getTraceScoreValue trace) [m'; x'; y']
                let currentTrace = createCell best x' y'
            
                array.[i,j] <- currentTrace   
        //The finished array/matrix gets returned
        array 

    /// Creates the alignment out of the matrix and the 2 sequences
    let rec private traceBackOption (fstSeq : 'a[]) (sndSeq : 'a[])  i j acc1 acc2 (array:Cell[,]) =         
        match array.[i,j].M with
        |Diagonal _   -> traceBackOption fstSeq sndSeq (i-1) (j-1) ((Some (fstSeq.[i-1]))::acc1) ((Some (sndSeq.[j-1]))::acc2) array
        |Horizontal _ -> traceBackOption fstSeq sndSeq  i    (j-1) (None::acc1) ((Some (sndSeq.[j-1]))::acc2) array
        |Vertical _   -> traceBackOption fstSeq sndSeq (i-1)  j    ((Some (fstSeq.[i-1]))::acc1) (None::acc2) array
        |NoTrace      -> acc1,acc2 //<- NoTrace returns list
    
    /// Creates the alignment out of the matrix and the 2 sequences
    let rec private traceBackZeroValue zeroValue (fstSeq : 'a[]) (sndSeq : 'a[])  i j acc1 acc2 (array:Cell[,]) =
        match array.[i,j].M with
        |Diagonal _   -> traceBackZeroValue zeroValue  fstSeq sndSeq (i-1) (j-1) ((fstSeq.[i-1])::acc1) ((sndSeq.[j-1])::acc2) array
        |Horizontal _ -> traceBackZeroValue zeroValue fstSeq sndSeq  i    (j-1) (zeroValue::acc1) ((sndSeq.[j-1])::acc2) array
        |Vertical _   -> traceBackZeroValue zeroValue fstSeq sndSeq (i-1)  j    ((fstSeq.[i-1])::acc1) (zeroValue::acc2) array
        |NoTrace      -> acc1,acc2 //<- NoTrace returns list

    /// Global pairwise alignment algorithm. (AffineGaps)
    module NeedlemanWunsch = 

        /// Uses the runGeneric function to create a matrix and then aligns the sequences via backtracing
        let private createCellMatrix (costs:Costs<'a>) (fstSeq : 'a [])  (sndSeq : 'a []) =
        
            //Evaluates biggest score for diagonal move
            let diagonalCost (cCell:Cell) (item1:'a) (item2:'a) = 
                    let sim' = costs.Similarity item1 item2
                    let mM = addFloatToTrace cCell.M sim' 
                    let mY = addFloatToTrace cCell.Y sim' 
                    let mX = addFloatToTrace cCell.X sim'             
                    max mX mY |> max mM 
            //Evaluates biggest score for horizontal move
            let horizontalCost (cCell:Cell) : int =
                let xM = addFloatToTrace cCell.M costs.Open
                let xX = addFloatToTrace cCell.X costs.Continuation
                max xX xM
            //Evaluates biggest score for vertical move
            let verticalCost (cCell:Cell) : int =
                let yM = addFloatToTrace cCell.M costs.Open
                let yY = addFloatToTrace cCell.Y costs.Continuation
                max yY yM
            
            ///Contains the 3 Cost-functions which are used by the "runGeneric" function to build the Matrix
            let opcs =  {DiagonalCost=diagonalCost;HorizontalCost=horizontalCost;VerticalCost=verticalCost}

            ///In this step the Cells (i=0) and (j=0) of the matrix get filled with the gap-Values
            let initFirstRowCol (array: Cell[,]) =
                let len1 = Array2D.length1 array
                let len2 = Array2D.length2 array
                // init first col (only vertical trace)
                for i=1 to len1-1 do 
                    let currentTrace = Vertical (costs.Open + (costs.Continuation * int (i-1)))
                    array.[i,0] <- createCell currentTrace currentTrace currentTrace
                // init first row (only horizontal trace)
                for j=1 to len2-1 do 
                    let currentTrace = Horizontal (costs.Open + (costs.Continuation * int (j-1)))
                    array.[0,j] <- createCell currentTrace currentTrace currentTrace    
                array
                        
            // The runGeneric-function is used with the Operation-Cost-Functions and the initiator-function to create a Array2D (matrix)
            runGeneric initFirstRowCol 
                fstSeq  sndSeq 
                    opcs

        ///Returns the optimal global alignment of two arrays of generic values
        let runGeneric (costs:Costs<'a>) (fstSeq : 'a [])  (sndSeq : 'a []) =
            let len1,len2 = fstSeq.Length, sndSeq.Length
            let cellMatrix = createCellMatrix costs fstSeq sndSeq
            //Afterwards, the backtrace creates the allignment out of the array and the 2 sequences and combined with its score 
            createAlignment
                (cellMatrix.[len1,len2].M |> getTraceScoreValue)
                (traceBackOption fstSeq sndSeq len1 len2 list.Empty list.Empty cellMatrix)
        
        ///Returns the optimal global alignment of two AminoAcid BioArrays
        let runAminoAcid (costs:Costs<AminoAcid>) (fstSeq : BioArray<AminoAcid>)  (sndSeq : BioArray<AminoAcid>) =
            let len1,len2 = fstSeq.Length, sndSeq.Length
            let cellMatrix = createCellMatrix costs fstSeq sndSeq
            createAlignment
                (cellMatrix.[len1,len2].M |> getTraceScoreValue)
                (traceBackZeroValue AminoAcid.Gap fstSeq sndSeq len1 len2 list.Empty list.Empty cellMatrix)

        ///Returns the optimal global alignment of two AminoAcidSymbol BioArrays
        let runAminoAcidSymbol (costs:Costs<AminoAcidSymbol>) (fstSeq : BioArray<AminoAcidSymbol>)  (sndSeq : BioArray<AminoAcidSymbol>) =
            let len1,len2 = fstSeq.Length, sndSeq.Length
            let cellMatrix = createCellMatrix costs fstSeq sndSeq
            createAlignment
                (cellMatrix.[len1,len2].M |> getTraceScoreValue)
                (traceBackZeroValue AminoAcidSymbol.Gap fstSeq sndSeq len1 len2 list.Empty list.Empty cellMatrix)
        
        ///Returns the optimal global alignment of two Nucleotide BioArrays
        let runNucleotide (costs:Costs<Nucleotide>) (fstSeq : BioArray<Nucleotide>)  (sndSeq : BioArray<Nucleotide>) =
            let len1,len2 = fstSeq.Length, sndSeq.Length
            let cellMatrix = createCellMatrix costs fstSeq sndSeq
            createAlignment
                (cellMatrix.[len1,len2].M |> getTraceScoreValue)
                (traceBackZeroValue Nucleotide.Gap fstSeq sndSeq len1 len2 list.Empty list.Empty cellMatrix)

    /// Local pairwise alignment algorithm  (AffineGaps)
    module SmithWaterman =

        /// Uses the runGeneric function to create a matrix and then aligns the sequences via backtracing
        let private createCellMatrix (costs:Costs<'a>) (fstSeq : 'a[]) (sndSeq : 'a[])  =

            //Evaluates biggest score for diagonal move           
            let diagonalCost (cCell:Cell) (item1:'a) (item2:'a) = 
                    let sim' = costs.Similarity item1 item2
                    let mM = addFloatToTrace cCell.M sim' 
                    let mY = addFloatToTrace cCell.Y sim' 
                    let mX = addFloatToTrace cCell.X sim'             
                    max mM mY |> max mX |> max 0

            //Evaluates biggest score for horizontal move
            let horizontalCost (cCell:Cell) : int =
                let xM = addFloatToTrace cCell.M costs.Open
                let xX = addFloatToTrace cCell.X costs.Continuation
                max xM xX |> max 0
            //Evaluates biggest score for vertical move
            let verticalCost (cCell:Cell) : int =
                let yM = addFloatToTrace cCell.M costs.Open
                let yY = addFloatToTrace cCell.Y costs.Continuation
                max yM yY |> max 0

            ///Contains the 3 Cost-functions which are used by the "runGeneric" function to build the Matrix
            let opcs =  {DiagonalCost=diagonalCost;HorizontalCost=horizontalCost;VerticalCost=verticalCost}

            // In this step, the runGeneric-function is used with the Operation-Cost-Functions to create a Array2D (matrix)
            runGeneric id            
                (fstSeq : 'a[]) (sndSeq : 'a[]) 
                    opcs                            
        
        ///Returns the optimal local alignment of two arrays of generic values
        let runGeneric (costs:Costs<'a>) (fstSeq : 'a [])  (sndSeq : 'a []) =
            let len1,len2 = fstSeq.Length, sndSeq.Length
            let cellMatrix = createCellMatrix costs fstSeq sndSeq
            let i, j= Array2D.indexMaxBy (fun c -> getTraceScoreValue c.M) cellMatrix
            //Afterwards, the backtrace creates the allignment out of the array and the 2 sequences and combined with its score 
            createAlignment
                (cellMatrix.[len1,len2].M |> getTraceScoreValue)
                (traceBackOption fstSeq sndSeq i j list.Empty list.Empty cellMatrix)

        ///Returns the optimal local alignment of two AminoAcid BioArrays
        let runAminoAcid (costs:Costs<AminoAcid>) (fstSeq : BioArray<AminoAcid>)  (sndSeq : BioArray<AminoAcid>) =
            let len1,len2 = fstSeq.Length, sndSeq.Length
            let cellMatrix = createCellMatrix costs fstSeq sndSeq
            let i, j= Array2D.indexMaxBy (fun c -> getTraceScoreValue c.M) cellMatrix
            createAlignment
                (cellMatrix.[len1,len2].M |> getTraceScoreValue)
                (traceBackZeroValue AminoAcid.Gap fstSeq sndSeq i j list.Empty list.Empty cellMatrix)

        ///Returns the optimal local alignment of two AminoAcidSymbol BioArrays
        let runAminoAcidSymbol (costs:Costs<AminoAcidSymbol>) (fstSeq : BioArray<AminoAcidSymbol>)  (sndSeq : BioArray<AminoAcidSymbol>) =
            let len1,len2 = fstSeq.Length, sndSeq.Length
            let cellMatrix = createCellMatrix costs fstSeq sndSeq
            let i, j= Array2D.indexMaxBy (fun c -> getTraceScoreValue c.M) cellMatrix
            createAlignment
                (cellMatrix.[len1,len2].M |> getTraceScoreValue)
                (traceBackZeroValue AminoAcidSymbol.Gap fstSeq sndSeq i j list.Empty list.Empty cellMatrix)
        
        ///Returns the optimal local alignment of two Nucleotide BioArrays
        let runNucleotide (costs:Costs<Nucleotide>) (fstSeq : BioArray<Nucleotide>)  (sndSeq : BioArray<Nucleotide>) =
            let len1,len2 = fstSeq.Length, sndSeq.Length
            let cellMatrix = createCellMatrix costs fstSeq sndSeq
            let i, j= Array2D.indexMaxBy (fun c -> getTraceScoreValue c.M) cellMatrix
            createAlignment
                (cellMatrix.[len1,len2].M |> getTraceScoreValue)
                (traceBackZeroValue Nucleotide.Gap fstSeq sndSeq i j list.Empty list.Empty cellMatrix)