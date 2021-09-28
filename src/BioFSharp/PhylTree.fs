namespace BioFSharp
open FSharp.Stats.ML.Unsupervised

/// Recursive representation of a phylogenetic tree
type PhylogeneticTree<'T> =
    ///Can be internal node or leaf node, depending on wether the list is empty or not. Match accordingly
    | Branch of 'T * List<PhylogeneticTree<'T>>
    | Leaf of 'T

module PhylogeneticTree =
    /// converts the input hierarchical clustering to a phylogenetig tree and conserves the distance insformation.
    /// In contrasr to the clustering result, the distance value of a Branch represents the distance to its Parent, 
    /// not the distance that all children have to this Branch.
    let ofHierarchicalCluster (branchTag:'T) (distanceConverter: float -> 'Distance) (hCluster:HierarchicalClustering.Cluster<'T>) : PhylogeneticTree<'T * 'Distance>=
        let rec loop distance (c: HierarchicalClustering.Cluster<'T>) =
            match c with
            | HierarchicalClustering.Cluster.Node (cIndex, distance, lCount, left, right) ->    
                PhylogeneticTree.Branch ((branchTag, distanceConverter distance), [loop distance left; loop distance right])
            | HierarchicalClustering.Cluster.Leaf (id, lCount, tag) -> PhylogeneticTree.Leaf((tag, distanceConverter distance))
        loop 0. hCluster 

    /// <summary>Performs hierarchical clustering of the input TaggedSequences using the provided distance function and linker. Returns the result as a Phylogenetic tree.</summary>
    /// <parameter name="branchTag">a tag to give the infered common ancestor branches (these are not tagged in contrast to the input sequence.)</parameter>
    /// <parameter name="distanceConverter">a converter function for the distance between nodes of the tree. Usually, a conversion to a string makes sense for downstream conversion to Newick format</parameter>
    /// <parameter name="distanceFunction">a function that determines the distance between two sequences e.g. evolutionary distance based on a substitution model</parameter>
    /// <parameter name="linker">the linker function to join clusters with</parameter>
    /// <parameter name="sequences">the input TaggedSequences</parameter>
    let ofTaggedSequencesWithLinker (branchTag:'T) (distanceConverter: float -> 'Distance) (distanceFunction: seq<'S> -> seq<'S> -> float) linker (sequences: seq<TaggedSequence<'T,'S>>) =
        sequences
        |> HierarchicalClustering.generate
            (fun a b  -> distanceFunction a.Sequence b.Sequence)
            linker
        |> ofHierarchicalCluster (TaggedSequence.create branchTag Seq.empty) distanceConverter


    /// <summary>Performs hierarchical clustering of the input TaggedSequences using the provided distance function. Returns the result as a Phylogenetic tree.</summary>
    /// <parameter name="distanceFunction">a function that determines the distance between two sequences e.g. evolutionary distance based on a substitution model</parameter>
    /// <parameter name="sequences">the input TaggedSequences</parameter>
    let ofTaggedBioSequences (distanceFunction: seq<#IBioItem> -> seq<#IBioItem> -> float) (sequences: seq<TaggedSequence<string,#IBioItem>>) : PhylogeneticTree<TaggedSequence<string,#IBioItem>*float>=
        sequences
        |> ofTaggedSequencesWithLinker
            "Ancestor"
            id
            distanceFunction
            HierarchicalClustering.Linker.upgmaLwLinker

    ///Iterates trough a tree and transforms all branch and leaf values by applying a mapping function on them
    let map (branchTagMapping: 'T -> 'U) (leafTagMapping: 'T -> 'U) (tree:PhylogeneticTree<'T>) = 
        let rec loop (tree:PhylogeneticTree<'T>) =
            match tree with     
            | Branch (b,children) -> Branch (branchTagMapping b, List.map loop children)
            | Leaf l -> Leaf (l |> leafTagMapping)
        loop tree

    ///Iterates trough a tree and performs an action on every branch and leaf
    let iter (branchAction: 'T -> unit) (leafAction: 'T -> unit) (tree:PhylogeneticTree<'T>) =     
        let rec loop (tree:PhylogeneticTree<'T>) =
            match tree with
            | Branch (b,nl) ->  
                branchAction b
                List.iter loop nl   
            | Leaf l -> leafAction l
        loop tree

    ///Iterates through a tree and accumulates a value by applying the folder to it and every branch and leaf. 
    let fold (folder: 'State -> 'T -> 'State) (acc: 'State) (tree:PhylogeneticTree<'T>) =
        let rec loop (tree:PhylogeneticTree<'T>) (acc:'State) =
            match tree with
            | Branch (b,nl) -> nl |> List.fold (fun acc elem -> loop elem acc) (folder acc b)
            | Leaf l -> folder acc l
        loop tree acc

    /// Returns the count of leaves
    let countLeafs (tree:PhylogeneticTree<'T>) =     
        let mutable leafCount = 0
        tree |> iter (fun _ -> ()) (fun _ -> leafCount <- leafCount+1)
        leafCount

    /// Returns the count of branches
    let countBranches (tree:PhylogeneticTree<'T>) =     
        let mutable branchCount = 0
        tree |> iter (fun _ -> branchCount <- branchCount+1) (fun _ -> ()) 
        branchCount