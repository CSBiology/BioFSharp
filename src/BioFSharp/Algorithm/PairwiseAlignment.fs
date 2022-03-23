namespace BioFSharp.Algorithm

open BioFSharp
open Alignment
open FSharpAux
open AminoAcidSymbols
open AminoAcids
open Nucleotides
open BioArray

///Contains functions for evaluating the best possible alignments for 2 Sequences
module PairwiseAlignment =
    
    type GlobalAlignmentInfo = {
        Score: int
        GapPenalty: int
        ExtendGapPenalty: int
        Length: int
        Identity: int
        IdentityFraction: float
    } with
        static member create (
            score: int,
            gapPenalty: int,
            extendGapPenalty: int,
            length: int,
            identity: int,
            identityFraction: float
        ) =
            {
                Score                   = score
                GapPenalty              = gapPenalty
                ExtendGapPenalty        = extendGapPenalty
                Length                  = length
                Identity                = identity
                IdentityFraction        = identityFraction
            }
        
    type LocalAlignmentInfo = {
        Score: int
        GapPenalty: int
        ExtendGapPenalty: int
        Length: int
        Identity: int
        IdentityFraction: float
        Seq1AlignmentStartIndex: int
        Seq2AlignmentStartIndex: int
    } with
        static member create (
            score: int,
            gapPenalty: int,
            extendGapPenalty: int,
            length: int,
            identity: int,
            identityFraction: float,
            seq1AlignmentStartIndex: int,
            seq2AlignmentStartIndex: int
        ) =
            {
                Score                   = score
                GapPenalty              = gapPenalty
                ExtendGapPenalty        = extendGapPenalty
                Length                  = length
                Identity                = identity
                IdentityFraction        = identityFraction
                Seq1AlignmentStartIndex = seq1AlignmentStartIndex
                Seq2AlignmentStartIndex = seq2AlignmentStartIndex
            }
            

    //Introduction
    //This page contains functions for evaluating the best possible alignments for 2 Sequences. Both the NeedlemanWunsch(NW)- and the SmithWaterman(SW)-algorithm are implemented by using an affine gapPenalty. For Understaning this Implementation, it is necessary to know about the basic operation of either the NW- or SW-algorithm and have an understanding of the Affine-Gap-penalty (3-submatrix-matrix)

    ///Represents one element of the matrix, contains direction for backtracing and score
    type internal TraceScore =     
        | Diagonal   of int
        | Horizontal of int
        | Vertical   of int
        | NoTrace

        static member getValue = function
            | Diagonal   x -> x
            | Horizontal x -> x
            | Vertical   x -> x
            | NoTrace      -> 0

        static member addValue (ts:TraceScore) (opc:int) =
            TraceScore.getValue ts + opc

    ///3 seperate matrices are used for evaluating the affine gap penalty. This is implemented by having every cell of the matrix carry 3 separate tracescore values
    type internal Cell = {
        M : TraceScore
        X : TraceScore
        Y : TraceScore
    } with
        ///Uses 3 TraceScores and creates a Cell from them
        static member create m x y =
            {M=m;X=x;Y=y;}

    ///Score of the alignment and the aligned sequences as a tuple
    type Score = int

    ///Merges the alignment score and the aligned sequences
    let private createAlignment score (sequence1,sequence2) : Alignment<'T,Score> =
        {
        Sequences = [sequence1;sequence2]; 
        MetaData = score }
    
    ///Carries the costs for gaps and the scoring matrix (Similarity)
    type Costs<'a> = {
        Open : int
        Continuation : int
        Similarity : 'a -> 'a -> int
    } with
     
        ///Creates a costs element used for alignment.
        static member create opening continuation similarityScoring =
            {
            Open = opening 
            Continuation = continuation
            Similarity  = similarityScoring
            }

    ///Carries the functions for evaluating the Tracescores
    type internal OperationCosts<'a> = {
        DiagonalCost   : Cell -> 'a -> 'a -> int
        HorizontalCost : Cell -> int
        VerticalCost   : Cell -> int
        }

    /// Creates an jagged array matrix and fills it with Backtracing Information (Cells)
    let internal initCellMatrix (initArray: Cell[,] -> Cell[,]) (fstSeq : 'a[]) (sndSeq : 'a[]) (opc:OperationCosts<'a>) = 
    
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

                let inline maxBy2 f x y =
                    if f x < f y then y else x
                let currentTrace = 
                     Cell.create (maxBy2 TraceScore.getValue y' m'|> maxBy2 TraceScore.getValue x')
                            x' y'
            
                array.[i,j] <- currentTrace   
        //The finished array/matrix gets returned
        array 

    /// Creates the alignment out of the matrix and the 2 sequences
    let rec internal traceBackOption (fstSeq : 'a[]) (sndSeq : 'a[])  i j acc1 acc2 (array:Cell[,]) =         
        match array.[i,j].M with
        |Diagonal _   -> traceBackOption fstSeq sndSeq (i-1) (j-1) ((Some (fstSeq.[i-1]))::acc1) ((Some (sndSeq.[j-1]))::acc2) array
        |Horizontal _ -> traceBackOption fstSeq sndSeq  i    (j-1) (None::acc1) ((Some (sndSeq.[j-1]))::acc2) array
        |Vertical _   -> traceBackOption fstSeq sndSeq (i-1)  j    ((Some (fstSeq.[i-1]))::acc1) (None::acc2) array
        |NoTrace      -> acc1,acc2 //<- NoTrace returns list
    
    /// Creates the alignment out of the matrix and the 2 sequences
    let rec internal traceBackZeroValue zeroValue (fstSeq : 'a[]) (sndSeq : 'a[])  i j acc1 acc2 (array:Cell[,]) =
        match array.[i,j].M with
        |Diagonal _   -> traceBackZeroValue zeroValue  fstSeq sndSeq (i-1) (j-1) ((fstSeq.[i-1])::acc1) ((sndSeq.[j-1])::acc2) array
        |Horizontal _ -> traceBackZeroValue zeroValue fstSeq sndSeq  i    (j-1) (zeroValue::acc1) ((sndSeq.[j-1])::acc2) array
        |Vertical _   -> traceBackZeroValue zeroValue fstSeq sndSeq (i-1)  j    ((fstSeq.[i-1])::acc1) (zeroValue::acc2) array
        |NoTrace      -> acc1,acc2 //<- NoTrace returns list

    /// returns absolut amount and fraction of identical entites in the alignment
    let internal getIdentity (seq1: seq<'A>) (seq2: seq<'A>) =
        let len1 = Seq.length seq1
        let len2 = Seq.length seq1
        if len1 <> len2 then
            failwith "alignments must have the same length. This is most likely an internal error."
        else
            let identity = 
                Seq.zip seq1 seq2
                |> Seq.filter (fun (a,b) -> a = b)
                |> Seq.length
            identity, (float identity / float len1)

    /// Global pairwise alignment algorithm. (AffineGaps)
    module Global = 
        
        type NeedlemanWunsch() =
            /// Uses the runGeneric function to create a matrix and then aligns the sequences via backtracing
            static member internal createCellMatrix (costs:Costs<'a>) (fstSeq : 'a [])  (sndSeq : 'a []) =
        
                //Evaluates biggest score for diagonal move
                let diagonalCost (cCell:Cell) (item1:'a) (item2:'a) = 
                        let sim' = costs.Similarity item1 item2
                        let mM = TraceScore.addValue cCell.M sim' 
                        let mY = TraceScore.addValue cCell.Y sim' 
                        let mX = TraceScore.addValue cCell.X sim'             
                        max mX mY |> max mM 
                //Evaluates biggest score for horizontal move
                let horizontalCost (cCell:Cell) : int =
                    let xM = TraceScore.addValue cCell.M costs.Open
                    let xX = TraceScore.addValue cCell.X costs.Continuation
                    max xX xM
                //Evaluates biggest score for vertical move
                let verticalCost (cCell:Cell) : int =
                    let yM = TraceScore.addValue cCell.M costs.Open
                    let yY = TraceScore.addValue cCell.Y costs.Continuation
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
                        array.[i,0] <- Cell.create currentTrace currentTrace currentTrace
                    // init first row (only horizontal trace)
                    for j=1 to len2-1 do 
                        let currentTrace = Horizontal (costs.Open + (costs.Continuation * int (j-1)))
                        array.[0,j] <- Cell.create currentTrace currentTrace currentTrace    
                    array
                        
                // The runGeneric-function is used with the Operation-Cost-Functions and the initiator-function to create a Array2D (matrix)
                initCellMatrix initFirstRowCol fstSeq sndSeq opcs

            ///Returns the optimal global alignment of two arrays of generic values
            static member alignOption<'A when 'A: equality> (
                fstSeq : 'A [],
                sndSeq : 'A [],
                costs:Costs<'A>
            ) =
                let len1,len2 = fstSeq.Length, sndSeq.Length
                let cellMatrix = NeedlemanWunsch.createCellMatrix costs fstSeq sndSeq
                let fstSeqAlignment, sndSeqAlignment = traceBackOption fstSeq sndSeq len1 len2 list.Empty list.Empty cellMatrix
                let score = cellMatrix.[len1,len2].M |> TraceScore.getValue
                let identity,identityFraction = getIdentity fstSeqAlignment sndSeqAlignment
                let metadata = 
                    GlobalAlignmentInfo.create(
                        score,
                        costs.Open,
                        costs.Continuation,
                        fstSeqAlignment.Length,
                        identity,
                        identityFraction
                    )
                Alignment.createAlignment metadata [fstSeqAlignment; sndSeqAlignment]
        
            ///Returns the optimal global alignment of two arrays of generic values
            static member align<'A when 'A: equality> (
                fstSeq : 'A [],
                sndSeq : 'A [],
                costs:Costs<'A>,
                gapValue: 'A
            ) =
                let len1,len2 = fstSeq.Length, sndSeq.Length
                let cellMatrix = NeedlemanWunsch.createCellMatrix costs fstSeq sndSeq
                let fstSeqAlignment, sndSeqAlignment = traceBackZeroValue gapValue fstSeq sndSeq len1 len2 list.Empty list.Empty cellMatrix
                let score = cellMatrix.[len1,len2].M |> TraceScore.getValue
                let identity,identityFraction = getIdentity fstSeqAlignment sndSeqAlignment
                //Afterwards, the backtrace creates the allignment out of the array and the 2 sequences and combined with its score 
                let metadata = 
                    GlobalAlignmentInfo.create(
                        score,
                        costs.Open,
                        costs.Continuation,
                        fstSeqAlignment.Length,
                        identity,
                        identityFraction
                    )
                Alignment.createAlignment metadata [fstSeqAlignment; sndSeqAlignment]

            ///Returns the optimal global alignment of two AminoAcid BioArrays
            static member align (
                fstSeq : BioArray<AminoAcid>,
                sndSeq : BioArray<AminoAcid>,
                costs:Costs<AminoAcid>
            ) = 
                NeedlemanWunsch.align<AminoAcid>(fstSeq,sndSeq,costs,AminoAcids.Gap)


            ///Returns the optimal global alignment of two AminoAcidSymbol BioArrays
            static member align (
                fstSeq : BioArray<AminoAcidSymbol>,
                sndSeq : BioArray<AminoAcidSymbol>,
                costs:Costs<AminoAcidSymbol>
            ) = 
                NeedlemanWunsch.align<AminoAcidSymbol>(fstSeq,sndSeq,costs,AminoAcidSymbol.Gap)
        
            ///Returns the optimal global alignment of two Nucleotide BioArrays
            static member align(
                fstSeq : BioArray<Nucleotide>,
                sndSeq : BioArray<Nucleotide>,
                costs:Costs<Nucleotide>
            ) =
                NeedlemanWunsch.align<Nucleotide>(fstSeq,sndSeq,costs,Nucleotide.Gap)

    module Local =
        
        /// Local pairwise alignment algorithm (AffineGaps)
        type SmithWaterman() =

            /// Uses the runGeneric function to create a matrix and then aligns the sequences via backtracing
            static member internal createCellMatrix (costs:Costs<'a>) (fstSeq : 'a[]) (sndSeq : 'a[])  =

                //Evaluates biggest score for diagonal move           
                let diagonalCost (cCell:Cell) (item1:'a) (item2:'a) = 
                    let sim' = costs.Similarity item1 item2
                    let mM = TraceScore.addValue cCell.M sim' 
                    let mY = TraceScore.addValue cCell.Y sim' 
                    let mX = TraceScore.addValue cCell.X sim'             
                    max mM mY |> max mX |> max 0

                //Evaluates biggest score for horizontal move
                let horizontalCost (cCell:Cell) : int =
                    let xM = TraceScore.addValue cCell.M costs.Open
                    let xX = TraceScore.addValue cCell.X costs.Continuation
                    max xM xX |> max 0

                //Evaluates biggest score for vertical move
                let verticalCost (cCell:Cell) : int =
                    let yM = TraceScore.addValue cCell.M costs.Open
                    let yY = TraceScore.addValue cCell.Y costs.Continuation
                    max yM yY |> max 0

                ///Contains the 3 Cost-functions which are used by the "runGeneric" function to build the Matrix
                let opcs =  {DiagonalCost=diagonalCost;HorizontalCost=horizontalCost;VerticalCost=verticalCost}

                // In this step, the runGeneric-function is used with the Operation-Cost-Functions to create a Array2D (matrix)
                initCellMatrix id fstSeq sndSeq opcs                            
        
            ///Returns the optimal local alignment of two arrays of generic values
            static member alignOption<'A when 'A: equality> (
                fstSeq : 'A [],
                sndSeq : 'A [],
                costs:Costs<'A>
            ) =
                let len1,len2 = fstSeq.Length, sndSeq.Length
                let cellMatrix = SmithWaterman.createCellMatrix costs fstSeq sndSeq
                //i is the start of the alignment in the first sequence, j in the second.
                let i, j= Array2D.indexMaxBy (fun c -> TraceScore.getValue c.M) cellMatrix
                //Afterwards, the backtrace creates the allignment out of the array and the 2 sequences and combined with its score 
                let fstSeqAlignment, sndSeqAlignment = traceBackOption fstSeq sndSeq i j list.Empty list.Empty cellMatrix
                let score = cellMatrix.[len1,len2].M |> TraceScore.getValue
                let identity,identityFraction = getIdentity fstSeqAlignment sndSeqAlignment
                let metadata = 
                    LocalAlignmentInfo.create(
                        score,
                        costs.Open,
                        costs.Continuation,
                        fstSeqAlignment.Length,
                        identity,
                        identityFraction,
                        i,
                        j
                    )

                Alignment.createAlignment metadata [fstSeqAlignment; sndSeqAlignment]

            ///Returns the optimal local alignment of two arrays of generic values
            static member align<'A when 'A: equality> (
                fstSeq : 'A [],
                sndSeq : 'A [],
                costs:Costs<'A>,
                gapValue:'A
            ) =
                let len1,len2 = fstSeq.Length, sndSeq.Length
                let cellMatrix = SmithWaterman.createCellMatrix costs fstSeq sndSeq
                //i is the start of the alignment in the first sequence, j in the second.
                let i, j= Array2D.indexMaxBy (fun c -> TraceScore.getValue c.M) cellMatrix
                //Afterwards, the backtrace creates the allignment out of the array and the 2 sequences and combined with its score 
                let fstSeqAlignment, sndSeqAlignment = traceBackZeroValue gapValue fstSeq sndSeq i j list.Empty list.Empty cellMatrix
                let score = cellMatrix.[len1,len2].M |> TraceScore.getValue
                let identity,identityFraction = getIdentity fstSeqAlignment sndSeqAlignment
                let metadata = 
                    LocalAlignmentInfo.create(
                        score,
                        costs.Open,
                        costs.Continuation,
                        fstSeqAlignment.Length,
                        identity,
                        identityFraction,
                        i,
                        j
                    )
                
                Alignment.createAlignment metadata [fstSeqAlignment; sndSeqAlignment]
                    

            ///Returns the optimal local alignment of two AminoAcid BioArrays
            static member align (
                fstSeq : BioArray<AminoAcid>,
                sndSeq : BioArray<AminoAcid>,
                costs:Costs<AminoAcid>
                
            ) = 
                SmithWaterman.align<AminoAcid>(fstSeq,sndSeq,costs,AminoAcids.Gap)


            ///Returns the optimal local alignment of two AminoAcidSymbol BioArrays
            static member align (
                fstSeq : BioArray<AminoAcidSymbol>,
                sndSeq : BioArray<AminoAcidSymbol>,
                costs:Costs<AminoAcidSymbol>
            ) = 
                SmithWaterman.align<AminoAcidSymbol>(fstSeq,sndSeq,costs,AminoAcidSymbol.Gap)
        
            ///Returns the optimal local alignment of two Nucleotide BioArrays
            static member align(
                fstSeq : BioArray<Nucleotide>,
                sndSeq : BioArray<Nucleotide>,
                costs:Costs<Nucleotide>
            ) =
                SmithWaterman.align<Nucleotide>(fstSeq,sndSeq,costs,Nucleotide.Gap)