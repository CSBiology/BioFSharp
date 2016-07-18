namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("BioFSharp.IO")>]
[<assembly: AssemblyProductAttribute("BioFSharp")>]
[<assembly: AssemblyDescriptionAttribute("Official git repository for BioFSharp")>]
[<assembly: AssemblyVersionAttribute("0.0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.1"
