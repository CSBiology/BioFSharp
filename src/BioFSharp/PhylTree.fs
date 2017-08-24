namespace BioFSharp

///Phyologenetic Tree
module PhylTree =
    
    type Node<'n> = | Branch of 'n * List<Node<'n>>

    ///Iterates trough a tree and transforms all nodes by applying a mapping function on them
    let rec map (mapping: Node<'n> -> 't) (tree:Node<'n>) = 
        let treeMapper tree = map mapping tree
        match tree with     
        | Branch (_,nl) -> Branch (mapping tree, List.map treeMapper nl)

    ///Iterates trough a tree and performs a action on every node
    let rec iter (action: Node<'n> -> unit) (tree:Node<'n>) =     
        let treeIterer tree = iter action tree
        match tree with
        | Branch (_,nl) ->  
            action tree
            List.iter treeIterer nl   

    ///Iterates through a tree and accumulates a value by applying the folder to it and every node of the tree
    let rec fold (acc: 'State) (folder: 'State -> Node<'n> -> 'State) (tree:Node<'n>) =     
        match tree with
        | Branch (_,nl) -> 
            folder 
                (List.fold 
                    (fun acc n -> fold acc folder n)
                    acc
                    nl)
               tree

    ///Iterates through a tree and accumulates a value by applying the folder to it and every mapped node of the tree
    let rec mapFold (acc: 'State) (mapping: Node<'n> -> 't) (folder: 'State -> 't -> 'State) (tree:Node<'n>) =     
        match tree with
        | Branch (_,nl) -> 
            folder 
                (List.fold 
                    (fun acc n -> mapFold acc mapping folder n)
                    acc
                    nl)
                (mapping tree)

    /// Returns the count of nodes containing no subtrees
    let countLeafs (tree:Node<'n>) =     
        fold 0 (fun x y -> x + (match y with | Branch (n,[]) -> 1 | _ -> 0)) tree
    
    ///Returns the most top level element for which the condition returns true
    let rec tryGetNodeBy (condition: Node<'n> -> bool) (tree:Node<'n>) =
        let rec loopList nl =
            match nl with
            | n :: tail -> 
                match tryGetNodeBy condition n with
                | Some x -> Some x
                | None -> loopList tail
            | [] -> None
        match tree with
        | Branch _ when condition tree ->
            Some tree
        | Branch (_,nl) -> loopList nl        

    ///Adds a child Node to the nodes for which the condition returns true
    let rec addChildToNodes (condition: Node<'n> -> bool) (child: Node<'n>) (tree:Node<'n>) : Node<'n>=
        let mapper = addChildToNodes (condition: Node<'n> -> bool) (child: Node<'n>)
        let rec loop tree= 
            match tree with             
            | Branch (n,nl) when condition tree ->
                loop (Branch(n,(child :: (List.map mapper nl))))
            | Branch (_,[]) ->
                tree
            | Branch (n,nl) ->
                loop (Branch(n,List.map mapper nl))
        loop tree