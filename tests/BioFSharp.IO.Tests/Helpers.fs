module Helpers

open System
open System.Reflection

let readEmbeddedDocument resourceName= 
    let assembly = Assembly.GetExecutingAssembly()
    use stream = assembly.GetManifestResourceStream("BioFSharp.IO.Tests.data." + resourceName)
    use reader = new System.IO.StreamReader(stream)
    seq [
        while not reader.EndOfStream do                              
        yield reader.ReadLine() 
    ]
    |> Array.ofSeq