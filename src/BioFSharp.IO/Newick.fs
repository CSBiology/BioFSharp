namespace BioFSharp.IO

open System.Text.RegularExpressions
open FSharp.Care.IO.SeqIO

module Newick = 
    
    ///Phylogenetic Tree which is used for parsing the Newick format
    type NewickTree =
            //------- distance * leafCount * NewickTree left * NewickTree right
            | Node of float * int * NewickTree list
            //------- dist * leafID
            | Leaf of float * string
            //Denotes illogical values in the input data
            | Corrupted

    //---Treebuilder functions---//

    let private getNewickTreeMemberCount (c:NewickTree) =
        match c with
        | Node(_,mc,_) -> mc
        | Leaf(_,_)     -> 1
        | Corrupted -> 0

    let private createNode<'T> (dist:float) (branchSet:NewickTree list) =        
        let leaveCount = List.sumBy getNewickTreeMemberCount branchSet
        Node(dist, leaveCount, branchSet)

    let private createLeaf (id:string) (dist: float) =
        Leaf(dist,id)

    let private createCorrupted =
        Corrupted

    //---Lexer---//

    ///Active Pattern for a leaf
    let private (|L|_|) input =
        let m = Regex.Match(input,".+:[\d]+\.[\d]+")
        if m.Success then 
            let a = m.Value.Split(':')
            Some (a.[0], System.Double.Parse(a.[1]))
        else None

    ///Active Pattern for nodedistance
    let private (|D|_|) input =
        let m = Regex.Match(input,"(?<=:)[\d]+\.[\d]+")
        if m.Success then
            Some (System.Double.Parse(m.Value))
        else None
    
    ///Union case for lexer tags
    type private Tags =
        | Emptyline
        | OpenNode
        | Leaf of string * float
        | NodeDistance of float
        | CorruptedValue
    
    ///Lexer
    let private lexer input =
        match input with
        | ""        -> Emptyline
        | "("       -> OpenNode
        | L input   -> Leaf input
        | D input   -> NodeDistance input
        | _         -> CorruptedValue
   
    //---Reader---//

    ///Creates a phylogenetic tree of type newick tree of file
    let fromFile (path:string) : NewickTree =
        let sequence = Seq.fromFile path
        let en = sequence.GetEnumerator()
        let rec loop (l:NewickTree list) = 
            match en.MoveNext() with
            | true -> 
                match lexer en.Current with
                | Emptyline ->
                    loop l
                | OpenNode ->
                    loop ((loop[])::l)
                | Leaf (s,d) -> 
                    loop ((createLeaf s d)::l)
                | NodeDistance d -> 
                    createNode d (l |> List.rev)
                | CorruptedValue -> Corrupted
            | false -> createNode 0.0 (l |> List.rev)
        // The starter function decides what happens at the beginning
        // it skips empty lines until it reaches either an opennode or a leaf
        // If the value it reaches is illogical, the tree consist only of the union case Corrupted
        let rec starter() = 
            match en.MoveNext() with
            | true -> 
                match lexer en.Current with
                    |OpenNode ->
                        loop []
                    |Leaf (s,d) -> 
                        if en.MoveNext() = false then
                            createLeaf s d
                        else Corrupted
                    | Emptyline -> 
                        starter()
                    |_ -> Corrupted
            | false -> Corrupted
        starter()

    //---Writer---//