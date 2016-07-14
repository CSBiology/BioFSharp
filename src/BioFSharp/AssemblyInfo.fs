namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("BioFSharp")>]
[<assembly: AssemblyProductAttribute("BioFSharp")>]
[<assembly: AssemblyDescriptionAttribute("BioFSharp aims to be a user-friendly library for Bioinformatics written in F# as the successor official of FSharpBio.")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
    let [<Literal>] InformationalVersion = "1.0"
