namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("BioFSharp.IO")>]
[<assembly: AssemblyProductAttribute("BioFSharp")>]
[<assembly: AssemblyDescriptionAttribute("BioFSharp aims to be a user-friendly library for Bioinformatics written in F# as the successor official of FSharpBio.")>]
[<assembly: AssemblyVersionAttribute("0.0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.1"
    let [<Literal>] InformationalVersion = "0.0.1"
