// --------------------------------------------------------------------------------------
// Builds the documentation from `.fsx` and `.md` files in the 'docs/content' directory
// (the generated documentation is stored in the 'docs/output' directory)
// --------------------------------------------------------------------------------------

// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------

#I __SOURCE_DIRECTORY__
#I "../../packages/formatting/FSharp.Formatting/lib/netstandard2.0"
#r "FSharp.CodeFormat.dll"
#r "FSharp.Literate.dll"
#r "FSharp.Markdown.dll"
#r "FSharp.MetadataFormat.dll"
#r "FSharp.Formatting.Common.dll"
#r "Microsoft.AspNetCore.Razor.dll"
#r "Microsoft.AspNetCore.Razor.Runtime.dll"
#r "Microsoft.AspNetCore.Razor.Language.dll"
#r "RazorEngine.NetCore.dll"
#r "FSharp.Formatting.Razor.dll"

let website = "https://CSBiology.github.io/BioFSharp/"

open System.IO
open FSharp.Formatting.Razor
open System.Collections.Generic

let subDirectories (dir: string) = Directory.EnumerateDirectories dir 

let rec copyRecursive dir1 dir2 =
    Directory.CreateDirectory dir2 |> ignore
    for subdir1 in Directory.EnumerateDirectories dir1 do
         let subdir2 = Path.Combine(dir2, Path.GetFileName subdir1)
         copyRecursive subdir1 subdir2
    for file in Directory.EnumerateFiles dir1 do
         File.Copy(file, file.Replace(dir1, dir2), true)
// Web site location for the generated documentation

// Specify more information about your project
let info =
  [ "project-name", "BioFSharp"
    "project-author", "Timo Mühlhaus, Kevin Schneider, Heinrich Lukas Weil, David Zimmer, F# open source contributors"
    "project-summary", "An open source bioinformatics toolbox written in F#. <https://csbiology.github.io/BioFSharp/>"
    "project-github", "https://github.com/CSBiology/BioFSharp"
    "project-nuget", "http://nuget.org/packages/BioFSharp" ]

    
#load "formatters.fsx"

// When called from 'build.fsx', use the public project URL as <root>
// otherwise, use the current 'output' directory.
#if RELEASE
let root = website
#else
let root = "file://" + (__SOURCE_DIRECTORY__ + "/../output")
#endif
    
// Paths with template/source/output locations
let bin        = __SOURCE_DIRECTORY__ + "/../../bin"
let content    = __SOURCE_DIRECTORY__ + "/../content"
let output     = __SOURCE_DIRECTORY__ + "/../../docs/output"
let files      = __SOURCE_DIRECTORY__ + "/../files"
let templates  = __SOURCE_DIRECTORY__ + "/templates"
let formatting = __SOURCE_DIRECTORY__ + "/../../packages/formatting/FSharp.Formatting/"
let docTemplate = formatting + "/templates/docpage.cshtml"
let referenceOut = (output + "/reference")

#I "/../content"

// Where to look for *.csproj templates (in this order)
let layoutRoots =
    [ templates; formatting + "templates";
    formatting + "/templates/reference" ]
let layoutRootsAll = Dictionary<string, string list>()
layoutRootsAll.Add("en",layoutRoots)
    
let copyFiles () = copyRecursive files output

// Build documentation from `fsx` and `md` files in `docs/content`
let buildDocumentation (file : string) =
    
    let filehtml = 
        Path.GetFileName(file)
            .Replace(".fsx",".html")

    printfn "building docPage for %s ..." file
    let fsiEvaluator = Formatters.createFsiEvaluator root output
    let langSpecificPath(lang, path:string) =
        path.Split([|'/'; '\\'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.exists(fun i -> i = lang)
    let layoutRoots =
        let key = layoutRootsAll.Keys |> Seq.tryFind (fun i -> langSpecificPath(i, file))
        match key with
        | Some lang -> layoutRootsAll.[lang]
        | None -> layoutRootsAll.["en"] // "en" is the default language
    RazorLiterate.ProcessScriptFile
        (file, docTemplate, output + "/" + filehtml, replacements = ("root", root)::info,
        layoutRoots = layoutRoots,
        fsiEvaluator = fsiEvaluator
        )
// Generate
copyFiles()
buildDocumentation fsi.CommandLineArgs.[0]
