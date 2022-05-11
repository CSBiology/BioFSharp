module TestingUtils

open Expecto
open System.Reflection
    
let floatsClose accuracy (seq1:seq<float>) (seq2:seq<float>) = 
    Seq.map2 (fun x1 x2 -> Accuracy.areClose accuracy x1 x2) seq1 seq2
    |> Seq.contains false
    |> not

let readEmbeddedDocument resourceName= 
    let assembly = Assembly.GetExecutingAssembly()
    use stream = assembly.GetManifestResourceStream("BioFSharp.Tests.data." + resourceName)
    use reader = new System.IO.StreamReader(stream)
    seq [
        while not reader.EndOfStream do                              
        yield reader.ReadLine() 
    ]
    |> Array.ofSeq