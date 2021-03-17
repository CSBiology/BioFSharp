module TestingUtils

open Expecto
    
    ///
let floatsClose accuracy (seq1:seq<float>) (seq2:seq<float>) = 
    Seq.map2 (fun x1 x2 -> Accuracy.areClose accuracy x1 x2) seq1 seq2
    |> Seq.contains false
    |> not