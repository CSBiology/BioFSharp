namespace BioFSharp.Algorithm


///Contains functions for evaluating the best possible alignments for 2 Sequences
module PairwiseAlignment =

    open FSharp.Care.Collections   
     //Introduction!
     //This page contains functions for evaluating the best possible alignments for 2 Sequences. Both the NeedlemanWunsch(NW)- and the SmithWaterman(SW)-algorithm are implemented by using an affine gapPenalty. For Understaning this Implementation, it is necessary to know about the basic operation of either the NW- or SW-algorithm and have an understanding of the Affine-Gap-penalty (3-submatrix-matrix)

    //The RunGeneric-Module 
    //In this module are the typedefinitons. There is also the runGeneric-Function. It is the skeleton-function for creating the Backtrace-matrix. As parameters it takes the 2 sequences and the operation-cost-functions ( opcs) and basically just creates a loop, enabling the opcs to evaluate the values of the cells. There also is the backtrace-function. It creates the alignment from the matrix and the 2 initial sequences.
    //The NW-Module and the SW-module
    //The two modules contain the opcs. The difference in these two is, that the SW-opcs check if the value is 0 or below. Also the backtrace does not start with the last cell of the matrix but at the one with the biggest value. This leads to the alignment being restrained to a local area, where there are the most matches.

    /// Holds all types and helper-functions. Also includes the function for the matrix creation and the backtrace-function
    module RunGeneric =

        ///Represents one element of the matrix, contains direction for backtracing and score
        type TraceScore =     
            | Diagonal   of int
            | Horizontal of int
            | Vertical   of int
            | NoTrace

        ///Get score of matrix element
        let getTraceScoreValue = function
            | Diagonal   x -> x
            | Horizontal x -> x
            | Vertical   x -> x
            | NoTrace      -> 0

        ///Adds value to score of tracescore element
        let addFloatToTrace (ts:TraceScore) (opc:int) =
            getTraceScoreValue ts + opc

        ///3 seperate matrices are used for evaluating the affine gap penalty. This is implemented by having every cell of the matrix carry 3 separate tracescore values
        type Cell = {
            M : TraceScore
            X : TraceScore
            Y : TraceScore
            }
    
        ///Uses 3 TraceScores and creates a Cell from them
        let createCell m x y =
            {M=m;X=x;Y=y;}

        ///Score of the alignment and the aligned sequences as a tuple
        type Alignment<'a> = {
            Score    : int
            Sequence : list<'a option * 'a option>
            }
    
        ///Merges the alignment score and the aligned sequences
        let createAlignment score sequence =
            {Score=score;Sequence=sequence}
    
        ///Carries the costs for gaps and the scoring matrix (Similarity)
        type Costs<'a> = {
            Open : int
            Continuation : int
            Similarity : 'a -> 'a -> int
            }
    
        ///Carries the functions for evaluating the Tracescores
        type OperationCosts<'a> = {
            DiagonalCost   : Cell -> 'a -> 'a -> int
            HorizontalCost : Cell -> int
            VerticalCost   : Cell -> int
            }
    
        ///Evaluates the bigger of two values (x y) after converting them with a function f
        let maxBy2 f x y =
            if f x < f y then y else x
 
        /// Creates an jagged array matrix and fills it with Backtracing Information (Cells)
        let runGeneric (initArray: Cell[,] -> Cell[,]) (fstSeq : 'a[]) (sndSeq : 'a[]) (opc:OperationCosts<'a>) = 
    
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
            
                    let currentTrace = 
                        createCell (maxBy2 getTraceScoreValue y' m'|> maxBy2 getTraceScoreValue x')
                                x' y'
            
                    array.[i,j] <- currentTrace   
            //The finished array/matrix gets returned
            array 



        /// Creates the alignment out of the matrix and the 2 sequences
        let rec traceBack (fstSeq : 'a[]) (sndSeq : 'a[])  i j acc (array:Cell[,])=         
            match array.[i,j].M with
            |Diagonal _   -> traceBack fstSeq sndSeq (i-1) (j-1) ((Some (fstSeq.[i-1]),Some (sndSeq.[j-1]))::acc) array
            |Horizontal _ -> traceBack fstSeq sndSeq  i    (j-1) ((None               ,Some (sndSeq.[j-1]))::acc) array
            |Vertical _   -> traceBack fstSeq sndSeq (i-1)  j    ((Some (fstSeq.[i-1]),None              )::acc) array
            |NoTrace      -> acc //<- NoTrace returns list


    /// Global alignment algorithm. (AffineGaps)
    module NeedlemanWunsch = 
        open RunGeneric
        /// Uses the runGeneric function to create a matrix and then aligns the sequences via backtracing
        let needlemanWunsch (costs:Costs<'a>) (fstSeq : 'a [])  (sndSeq : 'a [])  =
    
            let len1,len2 = fstSeq.Length, sndSeq.Length
        
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
            let array = 
                runGeneric initFirstRowCol 
                    fstSeq  sndSeq 
                        opcs

            //Afterwards, the backtrace creates the allignment out of the array and the 2 sequences and combined with its score 
            createAlignment
                    (array.[len1,len2].M |> getTraceScoreValue)
                    (traceBack fstSeq sndSeq len1 len2 list.Empty array)

    /// Local alignment algorithm  (AffineGaps)
    module SmithWaterman =
        open RunGeneric
        /// Uses the runGeneric function to create a matrix and then aligns the sequences via backtracing
        let smithWaterman (costs:Costs<'a>) (fstSeq : 'a[]) (sndSeq : 'a[])  =

            let len1,len2 = fstSeq.Length, sndSeq.Length

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
            let array = 
                runGeneric id            
                    (fstSeq : 'a[]) (sndSeq : 'a[]) 
                        opcs                            
        
            let i, j= Array2D.indexMaxBy (fun c -> getTraceScoreValue c.M) array

            //Afterwards, the backtrace creates the allignment out of the array and the 2 sequences and combined with its score
            createAlignment
                (array.[len1,len2].M |> getTraceScoreValue)
                    (traceBack fstSeq sndSeq i j list.Empty array)
