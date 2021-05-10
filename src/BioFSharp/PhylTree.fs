namespace BioFSharp
open FSharp.Stats.ML.Unsupervised

/// Recursive representation of a phylogenetic tree
type PhylogeneticTree<'T> =
    ///Can be internal node or leaf node, depending on wether the list is empty or not. Match accordingly
    | Branch of 'T * List<PhylogeneticTree<'T>>

    /// converts the input hierarchical clustering to a phylogenetig tree and conserves the distance insformation.
    /// In contrasr to the clustering result, the distance value of a Branch represents the distance to its Parent, 
    /// not the distance that all children have to this Branch.
    static member ofHierarchicalCluster (branchTag:'T) (distanceConverter: float -> 'Distance) (hCluster:HierarchicalClustering.Cluster<'T>) : PhylogeneticTree<'T * 'Distance>=
        let rec loop distance (c: HierarchicalClustering.Cluster<'T>) =
            match hCluster with
            | HierarchicalClustering.Cluster.Node (cIndex, distance, lCount, left, right) ->    
                PhylogeneticTree.Branch ((branchTag, distanceConverter distance), [loop distance left; loop distance right])
            | HierarchicalClustering.Cluster.Leaf (id, lCount, tag) -> PhylogeneticTree.Branch((tag, distanceConverter distance),[])
        loop 0. hCluster 

    /// <summary>Performs hierarchical clustering of the input TaggedSequences using the provided distance function and linker. Returns the result as a Phylogenetic tree.</summary>
    /// <parameter name="branchTag">a tag to give the infered common ancestor branches (these are not tagged in contrast to the input sequence.)</parameter>
    /// <parameter name="distanceConverter">a converter function for the distance between nodes of the tree. Usually, a conversion to a string makes sense for downstream conversion to Newick format</parameter>
    /// <parameter name="distanceFunction">a function that determines the distance between two sequences e.g. evolutionary distance based on a substitution model</parameter>
    /// <parameter name="linker">the linker function to join clusters with</parameter>
    /// <parameter name="sequences">the input TaggedSequences</parameter>
    static member ofTaggedSequencesWithLinker (branchTag:'T) (distanceConverter: float -> 'Distance) (distanceFunction: seq<'S> -> seq<'S> -> float) linker (sequences: seq<TaggedSequence<'T,'S>>) =
        sequences
        |> HierarchicalClustering.generate
            (fun a b  -> distanceFunction a.Sequence b.Sequence)
            linker
        |> PhylogeneticTree.ofHierarchicalCluster (TaggedSequence.create branchTag Seq.empty) distanceConverter


    /// <summary>Performs hierarchical clustering of the input TaggedSequences using the provided distance function. Returns the result as a Phylogenetic tree.</summary>
    /// <parameter name="distanceFunction">a function that determines the distance between two sequences e.g. evolutionary distance based on a substitution model</parameter>
    /// <parameter name="sequences">the input TaggedSequences</parameter>
    static member ofTaggedBioSequences (distanceFunction: seq<#IBioItem> -> seq<#IBioItem> -> float) (sequences: seq<TaggedSequence<string,#IBioItem>>) =
        sequences
        |> PhylogeneticTree.ofTaggedSequencesWithLinker
            "Ancestor"
            string
            distanceFunction
            HierarchicalClustering.Linker.upgmaLwLinker

    ///Iterates trough a tree and transforms all nodes by applying a mapping function on them
    static member map (mapping: PhylogeneticTree<'T> -> 't) (tree:PhylogeneticTree<'T>) = 
        let rec loop (mapping: PhylogeneticTree<'T> -> 't) (tree:PhylogeneticTree<'T>) =
            let treeMapper tree = loop mapping tree
            match tree with     
            | Branch (_,nl) -> Branch (mapping tree, List.map treeMapper nl)
        loop mapping tree

    ///Iterates trough a tree and performs a action on every node
    static member iter (action: PhylogeneticTree<'T> -> unit) (tree:PhylogeneticTree<'T>) =     
        let rec loop (action: PhylogeneticTree<'T> -> unit) (tree:PhylogeneticTree<'T>) =
            let treeIterer tree = loop action tree
            match tree with
            | Branch (_,nl) ->  
                action tree
                List.iter treeIterer nl   
        loop action tree

    ///Iterates through a tree and accumulates a value by applying the folder to it and every node of the tree
    static member fold (acc: 'State) (folder: 'State -> PhylogeneticTree<'T> -> 'State) (tree:PhylogeneticTree<'T>) =
        let rec loop (acc: 'State) (folder: 'State -> PhylogeneticTree<'T> -> 'State) (tree:PhylogeneticTree<'T>) =
            match tree with
            | Branch (_,nl) -> 
                folder 
                    (List.fold (fun acc n -> loop acc folder n) acc nl)
                    tree
        loop acc folder tree

    ///Iterates through a tree and accumulates a value by applying the folder to it and every mapped node of the tree
    static member mapFold (acc: 'State) (mapping: PhylogeneticTree<'T> -> 't) (folder: 'State -> 't -> 'State) (tree:PhylogeneticTree<'T>) =     
        let rec loop (acc: 'State) (mapping: PhylogeneticTree<'T> -> 't) (folder: 'State -> 't -> 'State) (tree:PhylogeneticTree<'T>) =
            match tree with
            | Branch (_,nl) -> 
                folder 
                    (List.fold 
                        (fun acc n -> PhylogeneticTree.mapFold acc mapping folder n)
                        acc
                        nl)
                    (mapping tree)
        loop acc mapping folder tree

    /// Returns the count of nodes containing no subtrees
    static member countLeafs (tree:PhylogeneticTree<'T>) =     
        PhylogeneticTree.fold 0 (fun x y -> x + (match y with | Branch (n,[]) -> 1 | _ -> 0)) tree
    
    ///Returns the most top level element for which the condition returns true
    static member tryGetNodeBy (condition: PhylogeneticTree<'T> -> bool) (tree:PhylogeneticTree<'T>) =
        let rec loop (condition: PhylogeneticTree<'T> -> bool) (tree:PhylogeneticTree<'T>) =
            let rec loopList nl =
                match nl with
                | n :: tail -> 
                    match loop condition n with
                    | Some x -> Some x
                    | None -> loopList tail
                | [] -> None
            match tree with
            | Branch _ when condition tree ->
                Some tree
            | Branch (_,nl) -> loopList nl
        loop condition tree

    ///Adds a child Node to the nodes for which the condition returns true
    static member addChildToNodes (condition: PhylogeneticTree<'T> -> bool) (child: PhylogeneticTree<'T>) (tree:PhylogeneticTree<'T>) : PhylogeneticTree<'T>=
        let rec loop (condition: PhylogeneticTree<'T> -> bool) (child: PhylogeneticTree<'T>) (tree:PhylogeneticTree<'T>) : PhylogeneticTree<'T>=
            let mapper = loop (condition: PhylogeneticTree<'T> -> bool) (child: PhylogeneticTree<'T>)
            let rec loopInner tree= 
                match tree with             
                | Branch (n,nl) when condition tree ->
                    loopInner (Branch(n,(child :: (List.map mapper nl))))
                | Branch (_,[]) ->
                    tree
                | Branch (n,nl) ->
                    loopInner (Branch(n,List.map mapper nl))
            loopInner tree
        loop condition child tree 