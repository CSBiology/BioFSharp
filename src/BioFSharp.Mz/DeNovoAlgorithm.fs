namespace BioFSharp.Mz

open System
open BioFSharp

open FunctionalGraph
open FSharp.FGL
open FSharp.FGL.Directed


module DeNovoAlgorithm =
    
    (*
        Path Algorithm 1. 
        ==================
        The path algorithm uses a depth first search to find all longest paths of connected edges from a defined startnode. 
        Those paths are stored together with a subGraph in a path accumulator. By decomposing the original graph at each visited node
        the already visited nodes and edges are removed. Thereby a subGraph is created that contains all but the already considered nodes and edges.
        If the calculated mass x of a thereby found path matches (x > precursorMass - minAAmass) it is saved as a result. For other paths
        fitting subPaths are searched in their related subGraphs through performing a second depth first search from the next node 
        with a higher mass than the last visited node of the current path. If the mass y of the thereby found second path
        matches ( y >= precursorMass - (mass of the previous path)) a gap is detected and a new combined path is saved in the
        path accumulator as well as a result. This process is repeated until a predefined threshold of decomposition steps is reached or all possible
        paths have been visited. Afterwards all paths saved as results are returned. 
    
    *)

    /// Smallest mass of the considered Amino Acids.
    let minAAmass = 
        references
        |> Array.minBy snd
        |> snd

    /// Finds all longest connected paths of edges resulting from a startnode in a functional graph. Returns the found paths mass, amino acid sequence and gap mass list.
    let collectAllLongestPaths (precursorMass : float) (completeGraph : Graph<SpectrumGraphNode, int, AminoAcids.AminoAcid>) 
        (startLabel : int) (gapLimit : int) = 
    
        /// Finds the startnode through its label in the graph. Fails if there is no valid node found. 
        let rec findStartNodeByLabel (counter : int) (nodeLabel : int) 
                (graph : Graph<SpectrumGraphNode, int, AminoAcids.AminoAcid>) = 
            if counter > 1000 then failwith "Input Graph empty"
            else 
                match (graph
                       |> Map.toArray
                       |> Array.tryFind (fun i -> (NodeContext.labels i) = (nodeLabel + counter))) with
                | Some x -> fst x
                | None -> findStartNodeByLabel (counter + 1) nodeLabel graph
        
        /// Finds the node with the next bigger mass than the input node by its label in a graph.
        let rec findNodeByLabel (nodeLabel : int) (subGraph : Graph<SpectrumGraphNode, int, AminoAcids.AminoAcid>) = 
            let rec inner (nodeLabel : int) (subGraph : Graph<SpectrumGraphNode, int, AminoAcids.AminoAcid>) (counter) = 
                if counter > 10 then None
                else 
                    match (subGraph
                           |> Map.toArray
                           |> Array.tryFind (fun i -> (NodeContext.labels i) = (nodeLabel + counter))) with
                    | Some x -> Some(fst x)
                    | None -> inner nodeLabel subGraph (counter + 1)
            inner nodeLabel subGraph 0
                
        /// Performs a depth first search in the graph. Returns all longest paths of connected edges resulting from the successor list of a startnode as well as the distance to the last visited node of the travelled path and it's remaining subGraph.
        let rec loopThroughGraph (successor : Adj<SpectrumGraphNode, AminoAcids.AminoAcid>) 
                    (graph : Graph<SpectrumGraphNode, int, AminoAcids.AminoAcid>) distanceToNode pathAcc resultAcc 
                    (finalResult : ((int * (SpectrumGraphNode * int)) * AminoAcids.AminoAcid list * Graph<SpectrumGraphNode, int, AminoAcids.AminoAcid>) list) =  
            match successor with
            | h :: t -> 
                let node,aa = h
                let subContext, subGraph = Graph.tryDecompose node graph
                
                let newSuccessor, nodeLabel = 
                    match subContext with
                    | Some(x, y, z, suc) -> suc, z
                    | None -> [], 0
                
                let newDistanceToNode = (fst distanceToNode + 1), (node, nodeLabel)
                if t.IsEmpty then 
                    loopThroughGraph newSuccessor subGraph newDistanceToNode pathAcc (aa :: resultAcc) finalResult
                else 
                    loopThroughGraph newSuccessor subGraph newDistanceToNode 
                        ((t, graph, distanceToNode, resultAcc) :: pathAcc) (aa :: resultAcc) finalResult
            | [] -> 
                match pathAcc with
                | (sL, g, dist, rL) :: tail -> 
                    if finalResult.IsEmpty then 
                        loopThroughGraph sL g dist tail rL ((distanceToNode, (List.rev resultAcc), graph) :: finalResult)
                    else 
                        let (currentDistance, amino, g) = finalResult.Head
                        if (fst currentDistance) > (fst distanceToNode) then loopThroughGraph sL g dist tail rL finalResult
                        elif (fst currentDistance) = (fst distanceToNode) then 
                            loopThroughGraph sL g dist tail rL 
                                ((distanceToNode, (List.rev resultAcc), graph) :: finalResult)
                        else loopThroughGraph sL g dist tail rL [ distanceToNode, (List.rev resultAcc), graph ]
                | [] -> 
                    if finalResult.IsEmpty then ([ distanceToNode, (List.rev resultAcc), graph ])
                    else 
                        let (currentDistance, amino, g) = finalResult.Head
                        if (fst currentDistance) > (fst distanceToNode) then finalResult
                        elif (fst currentDistance) = (fst distanceToNode) then 
                            ((distanceToNode, (List.rev resultAcc), graph) :: finalResult)
                        else [ distanceToNode, (List.rev resultAcc), graph ]
        
        /// Detects gaps between a path and a related list of subPaths. Combines them if a gap is found and returns a result accumulator containing combined paths with gaps as well as a path accumulator containing potentially unfinished paths.
        let rec combinator path 
                (alist : ((int * (SpectrumGraphNode * int)) * float * AminoAcids.AminoAcid list * Graph<SpectrumGraphNode, int, AminoAcids.AminoAcid> * float list) list) 
                (nsNode : SpectrumGraphNode) acc (gapacc : float list) acc2 = 
            match alist with
            | (dist, mass, aa, g, gapList) :: t -> 
                let (pdist, (sucNode, sucLabel)), pMass, pAAList, pG, pGapList = path
                let ndist, (nNode, nLabel) = dist
                let gap = 
                    (convertToMass (valueOfSpectrumGraphNode (nsNode))) - (convertToMass (valueOfSpectrumGraphNode sucNode))
                if (mass) <= (precursorMass - pMass) && gap >= minAAmass then 
                    let newPathwithGap = 
                        (pMass + (mass) + gap), 
                        (List.concat [ pAAList
                                       [ AminoAcids.Gap ]
                                       aa ])
                    combinator path t nsNode 
                        (((pdist, (sucNode, sucLabel)), pMass, pAAList, g, gapacc) 
                         :: ((dist, (fst newPathwithGap), (snd newPathwithGap), g, (gap :: gapacc)) :: acc)) gapacc 
                        (((newPathwithGap, (gap :: gapacc))) :: acc2)
                else 
                    combinator path t nsNode 
                        (((pdist, (sucNode, sucLabel)), pMass, pAAList, g, gapacc) :: ((dist, mass, aa, g, gapList) :: acc)) 
                        gapacc acc2
            | [] -> acc, acc2
        
        /// The first startNode of the search, found by its label.    
        let startNode = findStartNodeByLabel 0 startLabel completeGraph
        
        /// Defines the context and subGraph of the startNode by decomposing the native graph.
        let startContext, startGraph = Graph.tryDecompose startNode completeGraph
        
        /// Defines the list of successors of the startNode.
        let startSuc = 
            match startContext with
            | Some(x, y, z, suc) -> suc
            | None -> []
        
        /// The result of the first depth first for the startNode.
        let primaryResult = 
            (loopThroughGraph startSuc startGraph (0, (startNode, startLabel)) [] [] []) |> List.map (fun (x, y, z) -> 
                                                                                                x, 
                                                                                                (y
                                                                                                 |> List.map 
                                                                                                        (fun y -> 
                                                                                                        AminoAcids.monoisoMass 
                                                                                                            y)
                                                                                                 |> List.sum), y, z, [])
        
        /// Inner loop that decomposes the subGraph of each path, passes the resulting newStartSuccessor to the depth first search and passes the thereby found subPath list as well as the path to the combinator function.
        let rec inner (temp : ((int * (SpectrumGraphNode * int)) * float * AminoAcids.AminoAcid list * Graph<SpectrumGraphNode, int, AminoAcids.AminoAcid> * float list) list) 
                (counter : int) 
                (acc : ((int * (SpectrumGraphNode * int)) * float * AminoAcids.AminoAcid list * Graph<SpectrumGraphNode, int, AminoAcids.AminoAcid> * float list) list) 
                final = 
            if counter > gapLimit then 
                final
                |> List.map (fun x -> (x |> List.map (fun (x, y) -> x, (y |> List.rev))))
                |> List.distinct
            else 
                match temp with
                | (i, x, y, z, gap) :: t -> 
                    if x > (precursorMass - minAAmass) then inner t counter acc ([ (x, y), gap ] :: final)
                    else 
                        match (findNodeByLabel (snd (snd i)) z) with
                        | Some adjacentNode -> 
                            let newStartContext, newGraph = Graph.tryDecompose adjacentNode z
                            
                            let newStartSuc, newSNode, newLabel = 
                                match newStartContext with
                                | Some(x, y, z, s) -> s, y, z
                                | None -> [], (C(uint64 0.)), 0
                            
                            let subPathList = loopThroughGraph newStartSuc newGraph ((fst i), (newSNode, newLabel)) [] [] []
                            
                            let subSeqWMass = 
                                subPathList
                                |> List.map (fun (x, y, z) -> (x, y, z))
                                |> List.map (fun (x, y, z) -> 
                                       (x, 
                                        (y
                                         |> List.map (fun y -> AminoAcids.monoisoMass y)
                                         |> List.sum), y, z, []))
                            
                            let pSeqWMass = (i, x, y, z, gap)
                            let pathWithGaps = combinator pSeqWMass subSeqWMass newSNode [] gap []
                            if (snd pathWithGaps).IsEmpty then 
                                if acc.IsEmpty then inner t counter (fst pathWithGaps) final
                                else inner t counter (List.append (fst pathWithGaps) acc) final
                            else if acc.IsEmpty then inner t counter (fst pathWithGaps) ((snd pathWithGaps) :: final)
                            else inner t counter (List.append (fst pathWithGaps) acc) ((snd pathWithGaps) :: final)
                        | None -> inner t counter acc final
                | [] -> 
                    if acc.IsEmpty then 
                        final
                        |> List.map (fun x -> (x |> List.map (fun (x, y) -> x, (y |> List.rev))))
                        |> List.distinct
                    else inner acc (counter + 1) [] final
        
        inner primaryResult 0 [] []
    
    /// Returns the i paths with the highest overall mass from a collection of paths.
    let chooseResultsBasedOnMass i ( inList:((float*AminoAcids.AminoAcid list) *float list) list list) = 
        (inList |> List.concat |> List.sortByDescending (fun ((a,b),c) -> a)).GetSlice (Some 0, Some (i - 1))
    

    (*  
        2.
        Similar to the first approach the updated path algorithm is based on the decomposition of a graph. Instead of saving all paths with gaps and searching for the most fitting 
        one in the collection of paths, this algorithm continues a path after finding a fitting gap and only stores one longest or multiple paths with an equal distance.     
    
    *)

    /// Finds the longest path of connected edges in a functional graph resulting from a startNode. The maximum gap size defines how many adjacent nodes are considered if a gap is encountered.
    let findLongestPath (precursorMass:float) (completeGraph:Graph<SpectrumGraphNode,int,AminoAcids.AminoAcid>) (startLabel:int) (maxGapSize:int)=
    
        let temp = completeGraph |> Map.toArray

        /// Finds the startnode through its label in the graph. Fails if there is no valid node found. 
        let startNode = match (temp |> Array.tryFind (fun (i) -> (NodeContext.labels i) = startLabel)) with
                        | Some x -> fst x
                        | None -> failwith "Empty graph or invalid start node"

        /// Defines the context and subGraph of the startNode by decomposing the native graph.
        let startContext,startGraph = Graph.tryDecompose startNode completeGraph

        /// Defines the list of successors of the startNode.
        let startSuc = match startContext with 
                       | Some (x,y,z,suc) -> suc
                       | None -> []

        /// Performs a depth first search in the graph by looping through lists of successors nodes. Returns the longest path found in the graph.
        let rec loopThroughGraph (successor:Adj<SpectrumGraphNode,AminoAcids.AminoAcid>) (graph:Graph<SpectrumGraphNode,int,AminoAcids.AminoAcid>) gapMassAcc distanceToNode pathAcc resultAcc (finalResult:((int*(SpectrumGraphNode*int)) * (AminoAcids.AminoAcid list )*(float list)*Graph<SpectrumGraphNode,int,AminoAcids.AminoAcid>) list) =
    
            match successor with
            |h::t -> let node,aa = h
                     let subContext,subGraph = Graph.tryDecompose node graph
                     let newSuccessor,nodeLabel = match subContext with 
                                                  | Some (x,y,z,suc) -> suc,z
                                                  | None -> [],0
    
                     let newDistanceToNode = if aa = AminoAcids.AminoAcid.Gap then (fst distanceToNode),(node, nodeLabel)
                                             else (fst distanceToNode + 1),(node,nodeLabel)
    
                     if t.IsEmpty then loopThroughGraph newSuccessor subGraph gapMassAcc newDistanceToNode pathAcc (aa::resultAcc) finalResult
                     else loopThroughGraph newSuccessor subGraph gapMassAcc newDistanceToNode ((t,graph,gapMassAcc,distanceToNode,resultAcc)::pathAcc) (aa::resultAcc) finalResult//((distance+1)::pathAcc)
            
            |[] ->  let currentMass = (resultAcc |> List.map (fun y -> AminoAcids.monoisoMass y) |> List.sum) + (gapMassAcc |> List.sum)
                    if currentMass <= (precursorMass - minAAmass) then
                       let gapNodes = [for i in 1..maxGapSize do
                                          let potentialGapNode = (temp |> Array.tryFind (fun x -> (NodeContext.labels x) = ((snd(snd distanceToNode)) + i )))                          
                                          match potentialGapNode with
                                          | Some x -> let gap = ((convertToMass (valueOfSpectrumGraphNode (fst x) )) - (convertToMass (valueOfSpectrumGraphNode (fst (snd distanceToNode) ) ))) 
                                                      if gap >= minAAmass then
                                                         yield ([(fst x),AminoAcids.AminoAcid.Gap], graph,(gap::gapMassAcc), distanceToNode,resultAcc )
                                                         else ()  
                                          | None -> () ]
                        
                       match gapNodes with
                       | [] ->  match pathAcc with
                                | (sL,g,gap,dist,rL)::tail -> if finalResult.IsEmpty then loopThroughGraph sL g gap dist tail rL ((distanceToNode,resultAcc,gapMassAcc,graph)::finalResult) //graph
                                                              else let (currentDistance,amino,gapL,g) = finalResult.Head
                                                                   if (fst currentDistance) > ( fst distanceToNode) then loopThroughGraph sL g gap dist tail rL finalResult
                                                                   elif (fst currentDistance) = ( fst distanceToNode) then loopThroughGraph sL g gap dist tail rL ((distanceToNode,resultAcc,gapMassAcc,graph)::finalResult)
                                                                   else loopThroughGraph sL g gap dist tail rL [distanceToNode,resultAcc,gapMassAcc,graph]
                                                          
                                | [] -> if finalResult.IsEmpty then ([distanceToNode,resultAcc,gapMassAcc,graph])
                                        else let (currentDistance,amino,gapL,g) = finalResult.Head
                                             if (fst currentDistance) > (fst distanceToNode) then finalResult
                                             elif (fst currentDistance) = (fst distanceToNode) then ((distanceToNode,resultAcc,gapMassAcc,graph)::finalResult)
                                             else [distanceToNode,resultAcc,gapMassAcc,graph]
                       
                       | (newSucc,b,c,d,e)::t -> loopThroughGraph newSucc graph c distanceToNode (t@pathAcc) resultAcc finalResult   
    
                    else
                      match pathAcc with
                      | (sL,g,gap,dist,rL)::tail -> if finalResult.IsEmpty then loopThroughGraph sL g gap dist tail rL ((distanceToNode,resultAcc,gapMassAcc,graph)::finalResult) //graph
                                                    else let (currentDistance,amino,gapL,g) = finalResult.Head
                                                         if (fst currentDistance) > ( fst distanceToNode) then loopThroughGraph sL g gap dist tail rL finalResult
                                                         elif (fst currentDistance) = ( fst distanceToNode) then loopThroughGraph sL g gap dist tail rL ((distanceToNode,resultAcc,gapMassAcc,graph)::finalResult)
                                                         else loopThroughGraph sL g gap dist tail rL [distanceToNode,resultAcc,gapMassAcc,graph]
                                                
                      | [] -> if finalResult.IsEmpty then ([distanceToNode,resultAcc,gapMassAcc,graph])
                              else let (currentDistance,amino,gapL,g) = finalResult.Head
                                   if (fst currentDistance) > (fst distanceToNode) then finalResult
                                   elif (fst currentDistance) = (fst distanceToNode) then ((distanceToNode,resultAcc,gapMassAcc,graph)::finalResult)
                                   else [distanceToNode,resultAcc,gapMassAcc,graph]    
    
        let primaryResult = (loopThroughGraph startSuc startGraph [] (0,(startNode,startLabel)) [] [] [])
                            |> List.map (fun (w,x,y,z) -> (x|> List.rev),(y|> List.rev))
                            
        primaryResult

    