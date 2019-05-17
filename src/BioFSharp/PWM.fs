namespace BioFSharp

open System
open FSharpAux
open FSharpAux.IO

module PWM =

    ///Contain helper functions and types used by the rest.
    module HelperFunctionsAndTypes =

        /// One dimensional array with fixed positions for each element.
        type BaseArray<'a, 'value when 'a :> IBioItem>internal (array:'value []) =
    
            let getIndex (key:'a) =
                (BioItem.symbol key |> int) - 42

            new () =
                let arr:'value [] = Array.zeroCreate 49
                new BaseArray<_, 'value>(arr)

            member this.Array = 
                if (obj.ReferenceEquals(array, null)) then
                    raise (ArgumentNullException("array"))
                array
            
            member this.Item
                with get i       = array.[getIndex i]
                and  set i value = array.[getIndex i] <- value

        /// Matrix with fixed positions for nucleotides and amino acids.
        type BaseMatrix<'a, 'value when 'a :> IBioItem>internal(matrix:'value [,]) =

            let getRowArray2DIndex (key:'a) =
                (BioItem.symbol key |> int) - 42

            new (rowLength:int) =
                let arr:'value [,] = Array2D.zeroCreate 49 rowLength 
                new BaseMatrix<_, 'value>(arr)

            member this.Matrix = 
                if (obj.ReferenceEquals(matrix, null)) then
                    raise (ArgumentNullException("array"))
                matrix
            
            member this.Item
                with get (column, row)       = matrix.[getRowArray2DIndex column, row]
                and  set (column, row) value = matrix.[getRowArray2DIndex column, row] <- value

        // Compare all elements which are part of a list and group equals together.
        let groupEquals (il:'a list when 'a:equality) = 
            let rec innerLoop (cL:('b*int)list) (hL:('b*int)list) (item:'b) =
                match cL with
                |[] -> 
                    ((item, 1)::hL)
                |(it, count)::tail -> 
                    if item = it then
                        ((it, count+1)::(hL@tail))
                    else 
                        innerLoop tail ((it, count)::hL) item
            let rec outerLoop l acc =
                match l with
                |[] -> acc
                |head::tail ->
                    outerLoop tail (innerLoop acc [] head)
            outerLoop il []

        ///Checks whether all elements in the list have a wider distance than width or not.
        let ceckForDistance (width:int) (items:int list) =
            if items.Length <= 0 || items.Length = 1 then true
            else
                let rec loop n i =
                    if n = items.Length-1 then true
                    else
                        if i >= items.Length then loop (n+1) (n+2)
                        else
                            if Operators.abs(items.[n]-items.[i]) > width then
                                loop n (i+1)
                            else false
                loop 0 1

        /// Get an integer which is between 0 and the length of the sequence - segmentLength
        let getRandomNumberInSequence (segmentLength:int) (source:'a[]) =
            let rnd = System.Random()
            Array.init 1 (fun _ -> rnd.Next(0, source.Length-segmentLength+1))
            |> Array.head

        /// Create a specific sub sequence of the source sequence based on the given length and starting position. Do not forget, sequences start counting at 0!
        let getDefinedSegment (subsequenceLength:int) (source:'a[]) (startPoint:int) =
            source 
            |> Array.skip startPoint 
            |> Array.take subsequenceLength 
            |> Array.ofSeq


        ///Finds the best information content in an array of arrays of PWMSs and Positions.
        let getBestInformationContent (item:((float*int)[])[]) =
            let rec loop (n:int) (bestPWMS:(float*int)[]) =
                if n = item.Length then bestPWMS
                else
                    let informationContentItem =
                        item.[n]
                        |> Array.fold (fun baseValue (pwms, _) -> pwms + baseValue) 0.
                    let informationContentBestPWMS =
                        bestPWMS
                        |> Array.fold (fun baseValue (pwms, _) -> pwms + baseValue) 0.
                    if informationContentItem > informationContentBestPWMS then
                        loop (n + 1) item.[n]
                    else
                        loop (n + 1) bestPWMS
            loop 0 [||]

    open HelperFunctionsAndTypes

    /// Contain tools to create and manipulate FrequencyVectors.
    module FrequencyVector =

        /// One dimensional array with fixed positions for each element.
        /// Use to track frequency of elements independent of position in source.
        type FrequencyVector internal (array:int []) =

            inherit BaseArray<IBioItem, int>(array)

            new() = 
                let arr:'value [] = Array.zeroCreate 49
                new FrequencyVector(arr)

        /// Increase counter of position by 1.
        let addInPlaceAtPos (bioItem:'a) (backGroundFrequencyVector:FrequencyVector) = 
            backGroundFrequencyVector.[bioItem] <- backGroundFrequencyVector.[bioItem] + 1
            backGroundFrequencyVector

        /// Increase counter of position by n.
        let addInPlaceSpecifiedAmountAtPos (bioItem:'a) (backGroundFrequencyVector:FrequencyVector) n = 
            backGroundFrequencyVector.[bioItem] <- backGroundFrequencyVector.[bioItem] + n
            backGroundFrequencyVector

        /// Create a FrequencyVector based on BioArrays and exclude the specified segments.
        let getFrequencyVectorOfSources (motifLength:int) (resSources:BioArray.BioArray<#IBioItem>[]) (motifPositions:int[]) =
            let backGroundCounts = new FrequencyVector()    
            Array.map2 (fun (resSource:BioArray.BioArray<#IBioItem>) (position:int) -> Array.append resSource.[0..(position-1)] resSource.[(position+motifLength)..]) resSources motifPositions
            |> Array.concat
            |> Array.fold (fun bc bioItem -> (addInPlaceAtPos bioItem bc)) backGroundCounts

        /// Create a FrequencyVector based on BioArrays and exclude the specified segments.
        let getFrequencyVectorOfSource (resSources:BioArray.BioArray<#IBioItem>) =
            let backGroundCounts = new FrequencyVector()   
            Array.fold (fun bc bioItem -> (addInPlaceAtPos bioItem bc)) backGroundCounts resSources

        /// Create new FrequencyVector based on the sum of the positions of an array of FrequencyVectors.
        let fuseFrequencyVectors (usedBioSet:#IBioItem[]) (bfVectors:FrequencyVector[]) =
            let backgroundFrequencyVector = new FrequencyVector()
            for bfVector in bfVectors do
                for bioItem in usedBioSet do
                    backgroundFrequencyVector.[bioItem] <- backgroundFrequencyVector.[bioItem] + (bfVector.[bioItem])
            backgroundFrequencyVector

        /// Create FrequencyVector based on BioArray and excludes the specified segment.
        let getFrequencyVectorWithoutSegment (motifLength:int) (position:int) (resSource:BioArray.BioArray<#IBioItem>) =
            let backGroundCounts = new FrequencyVector()
            Array.append resSource.[0..(position-1)] resSource.[(position+motifLength)..]
            |> Array.fold (fun bc bioItem -> (addInPlaceAtPos bioItem bc)) backGroundCounts

        /// Create FrequencyVector based on BioArray.
        let addInPlaceCountsOfSource (resSources:BioArray.BioArray<#IBioItem>) (backGroundCounts:FrequencyVector) =   
            resSources
            |> Array.fold (fun bc bioItem -> (addInPlaceAtPos bioItem bc)) backGroundCounts

        /// Subtracts the amount of elements in the given source from the FrequencyVector.
        let substractCountsOfBFVector (source:BioArray.BioArray<#IBioItem>) (bfVector:FrequencyVector) =
            let bfVec = new FrequencyVector(bfVector.Array)
            for bioItem in source do
                bfVec.[bioItem] <- (if bfVector.[bioItem] - 1 > 0 then bfVector.[bioItem] - 1 else 0)
            bfVec

    /// Contain tools to create and manipulate FrequencyVectors.
    module ProbabilityVector = 
    
        type ProbabilityVector internal (array:float []) =
        
            inherit BaseArray<IBioItem,float>(array)

            new() = 
                let arr:'value [] = Array.zeroCreate 49
                new ProbabilityVector(arr)

        /// Increase counter of position by 1.
        let addInPlaceAtPos (bioItem:'a) (backGroundProbabilityVector:ProbabilityVector) = 
            backGroundProbabilityVector.[bioItem] <- backGroundProbabilityVector.[bioItem] + 1.
            backGroundProbabilityVector

        /// Increase counter of position by n.
        let addInPlaceSpecifiedAmountAtPos (bioItem:'a) (backGroundProbabilityVector:ProbabilityVector) n = 
            backGroundProbabilityVector.[bioItem] <- backGroundProbabilityVector.[bioItem] + n
            backGroundProbabilityVector

        /// Create a ProbabilityVector based on a FrequencyVector by replacing the integers with floats.
        let getBPVofBFV (caArray:FrequencyVector.FrequencyVector) =
            caArray.Array
            |> Array.map (fun item -> float item)
            |> fun item -> new ProbabilityVector(item)

        /// Create normalized ProbabilityVector based on FrequencyVector.
        let getNormalizedProbabilityVector (usedBioSet:#IBioItem[]) (pseudoCount:float) (backGroundFrequencyVector:FrequencyVector.FrequencyVector) =
            let backGroundProbabilityVector = getBPVofBFV backGroundFrequencyVector
            let sum = (float (Array.sum backGroundFrequencyVector.Array)) + ((float usedBioSet.Length) * pseudoCount)
            for item in usedBioSet do
                backGroundProbabilityVector.[item] <- (backGroundProbabilityVector.[item] + pseudoCount)/sum
            backGroundProbabilityVector

        /// Calculate the score of the given sequence based on the Probabilities of the ProbabilityVector.
        let calculateBackGroundSegmentScore (bpVector:ProbabilityVector) (bioItems:BioArray.BioArray<#IBioItem>) =
            Array.fold (fun (value:float) (bios:#IBioItem) -> value * (bpVector.[bios])) 1. bioItems

    /// Contain tools to create and manipulate PositionFrequencyMatrix.
    module PositionFrequencyMatrix =

        /// Matrix with fixed positions for nucleotides and amino acids with default value of 0.
        type PositionFrequencyMatrix internal(matrix:int [,]) =

            inherit BaseMatrix<IBioItem, int>(matrix)

            new (rowLength:int) = 
                let arr:'value [,] = Array2D.zeroCreate 49 rowLength 
                new PositionFrequencyMatrix(arr)

        /// Increase counter of PositionFrequencyMatrix at fixed position by 1.
        let addInPlaceAtPos (pos:int) (bioItem:'a when 'a :> IBioItem) (positionFrequencyMatrix:PositionFrequencyMatrix) = 
            positionFrequencyMatrix.[bioItem, pos] <- positionFrequencyMatrix.[bioItem, pos] + 1
            positionFrequencyMatrix

        /// Increase counter of PositionFrequencyMatrix at fixed position by n.
        let addInPlaceSpecifiedAmountAtPos (pos:int) (bioItem:'a when 'a :> IBioItem) (positionFrequencyMatrix:PositionFrequencyMatrix) n = 
            positionFrequencyMatrix.[bioItem, pos] <- positionFrequencyMatrix.[bioItem, pos] + n
            positionFrequencyMatrix

        /// Create PositionFrequencyMatrix based on BioArray.
        let getPositionFrequencyMatrix (source:BioArray.BioArray<#IBioItem>) =
            let positionFrequencyMatrix = new PositionFrequencyMatrix(source.Length)
            source
            |> Array.fold (fun (row, cm) column -> row + 1, addInPlaceAtPos row column cm) (0, positionFrequencyMatrix) |> ignore
            positionFrequencyMatrix

        /// Create new PositionFrequencyMatrix based on the sum of the positions of an array of Countmatrices.
        let fusePositionFrequencyMatrices (motifLength:int) (countMatrices:PositionFrequencyMatrix[]) =
            let positionFrequencyMatrix = new PositionFrequencyMatrix(motifLength)
            if countMatrices.Length > 0 then
                for cMatrix in countMatrices do
                    for column = 0 to (Array2D.length1 cMatrix.Matrix)-1 do
                            for row = 0 to (Array2D.length2 cMatrix.Matrix)-1 do
                                positionFrequencyMatrix.Matrix.[column, row] <- positionFrequencyMatrix.Matrix.[column, row] + (cMatrix.Matrix.[column, row])
                positionFrequencyMatrix
            else positionFrequencyMatrix

    /// Contain tools to create and manipulate PositionProbabilityMatrix.
    module PositionProbabilityMatrix = 
    
        /// Matrix with fixed positions for nucleotides and amino acids with default value of 0. .
        type PositionProbabilityMatrix internal(matrix:float [,]) =

            inherit BaseMatrix<IBioItem, float>(matrix)

            new(rowLength:int) = 
                let arr:'value [,] = Array2D.zeroCreate 49 rowLength 
                new PositionProbabilityMatrix(arr)

        /// Increase counter of PositionProbabilityMatrix at fixed position by 1.
        let addInPlaceAtPos (pos:int) (bioItem:'a when 'a :> IBioItem) (positionProbabilityMatrix:PositionProbabilityMatrix) = 
            positionProbabilityMatrix.[bioItem, pos] <- positionProbabilityMatrix.[bioItem, pos] + 1.
            positionProbabilityMatrix

        /// Increase counter of PositionProbabilityMatrix at fixed position by n.
        let addInPlaceSpecifiedAmountAtPos (pos:int) (bioItem:'a when 'a :> IBioItem) (positionProbabilityMatrix:PositionProbabilityMatrix) n = 
            positionProbabilityMatrix.[bioItem, pos] <- positionProbabilityMatrix.[bioItem, pos] + n
            positionProbabilityMatrix

        /// Create new PositionWeightMatrix based on existing PositionFrequencyMatrix. The counts of each position of each element are transformed to floats.
        let transformCountMatrixToPPMatrix (positionFrequencyMatrix:PositionFrequencyMatrix.PositionFrequencyMatrix) =
            positionFrequencyMatrix.Matrix |> Array2D.map (fun item -> float item)
            |> fun item -> new PositionProbabilityMatrix(item)

        /// Create PositionProbabilityMatrix based on PositionFrequencyMatrix.
        let getPositionProbabilityMatrix (sourceCount:int) (usedBioSet:#IBioItem[]) (pseudoCount:float) (positionFrequencyMatrix:PositionFrequencyMatrix.PositionFrequencyMatrix) =
            let positionProbabilityMatrix = transformCountMatrixToPPMatrix positionFrequencyMatrix
            let sum = (float sourceCount) + ((float usedBioSet.Length) * pseudoCount)
            for item in usedBioSet do
                for position = 0 to (Array2D.length2 positionProbabilityMatrix.Matrix) - 1 do
                positionProbabilityMatrix.[item, position] <- (positionProbabilityMatrix.[item, position] + pseudoCount)/sum
            positionProbabilityMatrix
  