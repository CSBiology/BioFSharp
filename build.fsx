// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r "paket:
nuget BlackFox.Fake.BuildTask
nuget Fake.Core.Target
nuget Fake.Core.Process
nuget Fake.Core.ReleaseNotes
nuget Fake.IO.FileSystem
nuget Fake.DotNet.Cli
nuget Fake.DotNet.MSBuild
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.DotNet.Paket
nuget Fake.DotNet.FSFormatting
nuget Fake.DotNet.Fsi
nuget Fake.DotNet.NuGet
nuget Fake.Api.Github
nuget Fake.DotNet.Testing.Expecto //"

#load ".fake/build.fsx/intellisense.fsx"

open BlackFox.Fake
open System.IO
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing
open Fake.IO.Globbing.Operators
open Fake.DotNet.Testing
open Fake.Tools
open Fake.Api
open Fake.Tools.Git

Target.initEnvironment ()

[<AutoOpen>]
module TemporaryDocumentationHelpers =

    type LiterateArguments =
        { ToolPath : string
          Source : string
          OutputDirectory : string 
          Template : string
          ProjectParameters : (string * string) list
          LayoutRoots : string list 
          FsiEval : bool }


    let private run toolPath arguments = 
        Command.RawCommand
            (
                toolPath,
                arguments
            )
        |> CreateProcess.fromCommand
        |> CreateProcess.withFramework
        |> CreateProcess.ensureExitCode
        |> Proc.run
        |> ignore
        //if 0 <> Process.execSimple ((fun info ->
        //        { info with
        //            FileName = toolPath
        //            Arguments = command }) >> Process.withFramework) System.TimeSpan.MaxValue

        //then failwithf "FSharp.Formatting %s failed." command

    let createDocs p =
        let toolPath = 
            match ProcessUtils.tryFindLocalTool "" "fsformatting.exe"  [(Directory.GetCurrentDirectory() @@ "/lib")] with
            |Some tool -> tool
            | _ -> failwith "FSFormatting executable not found"
        //let toolPath = Tools.findToolInSubPath "fsformatting.exe" (Directory.GetCurrentDirectory() @@ "lib/Formatting")

        let defaultLiterateArguments =
            { ToolPath = toolPath
              Source = ""
              OutputDirectory = ""
              Template = ""
              ProjectParameters = []
              LayoutRoots = [] 
              FsiEval = false }

        let arguments = (p:LiterateArguments->LiterateArguments) defaultLiterateArguments
        let layoutroots =
            if arguments.LayoutRoots.IsEmpty then []
            else [ "--layoutRoots" ] @ arguments.LayoutRoots
        let source = arguments.Source
        let template = arguments.Template
        let outputDir = arguments.OutputDirectory
        let fsiEval = if arguments.FsiEval then [ "--fsieval" ] else []

        let command = 
            arguments.ProjectParameters
            |> Seq.map (fun (k, v) -> [ k; v ])
            |> Seq.concat
            |> Seq.append 
                   (["literate"; "--processdirectory" ] @ layoutroots @ [ "--inputdirectory"; source; "--templatefile"; template; 
                      "--outputDirectory"; outputDir] @ fsiEval @ [ "--replacements" ])
            |> Arguments.OfArgs
        run arguments.ToolPath command
        printfn "Successfully generated docs for %s" source

[<AutoOpen>]
module MessagePrompts =

    let prompt (msg:string) =
        System.Console.Write(msg)
        System.Console.ReadLine().Trim()
        |> function | "" -> None | s -> Some s
        |> Option.map (fun s -> s.Replace ("\"","\\\""))

    let rec promptYesNo msg =
        match prompt (sprintf "%s [Yn]: " msg) with
        | Some "Y" | Some "y" -> true
        | Some "N" | Some "n" -> false
        | _ -> System.Console.WriteLine("Sorry, invalid answer"); promptYesNo msg

    let releaseMsg = """This will stage all uncommitted changes, push them to the origin and bump the release version to the latest number in the RELEASE_NOTES.md file. 
        Do you want to continue?"""

    let releaseDocsMsg = """This will push the docs to gh-pages. Remember building the docs prior to this. Do you want to continue?"""

// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------
let project         = "BioFSharp"
let summary         = "An open source bioinformatics toolbox written in F#. <https://csbiology.github.io/BioFSharp/>"
let solutionFile    = "BioFSharp.sln"
let configuration   = "Release"

let testAssemblies = "tests/**/bin" </> configuration </> "**" </> "*Tests.exe"

let gitOwner = "CSBiology"
let gitHome = sprintf "%s/%s" "https://github.com" gitOwner
let gitName = "BioFSharp"

let website = "/BioFSharp"
let pkgDir = "pkg"

//Build configurations
let monoConfiguration = DotNet.Custom "Mono"
let dotnetCoreConfiguration = DotNet.Custom "DotnetCore"
let buildConfiguration = DotNet.Custom <| Environment.environVarOrDefault "configuration" configuration

let buildServerSuffix = 
    match BuildServer.buildServer with
    |BuildServer.AppVeyor   -> sprintf "appveyor.%s" BuildServer.appVeyorBuildVersion
    |BuildServer.Travis     -> sprintf "travis.%s" BuildServer.travisBuildNumber
    | _                     -> ""

// Read additional information from the release notes document
let release = ReleaseNotes.load "RELEASE_NOTES.md"

//---------------------------------------------------------------------------------------------------------------------------------
// Projects for different buildConfigs

// when building everything on windows (netstandard 2.0 + netfx)
let allProjectPaths =
    !! "src/**/*.fsproj"
    |>  Seq.map 
        (fun f -> (Path.getDirectory f))

// projects built with the mono configuration (ik cant get bioDB to work here)
let monoProjectPaths =
    !! "src/**/*.fsproj"
    -- "src/BioFSharp.BioDB/BioFSharp.BioDB.fsproj"
    |>  Seq.map 
        (fun f -> (Path.getDirectory f))

// netstandard2.0 projects only
let dotnetProjectPaths =
    !! "src/**/*.fsproj"
    -- "src/BioFSharp.BioDB/BioFSharp.BioDB.fsproj"
    -- "src/BioFSharp.Parallel/BioFSharp.Parallel.fsproj" 
    -- "src/BioFSharp.ImgP/BioFSharp.ImgP.fsproj" 
    -- "src/BioFSharp.Vis/BioFSharp.Vis.fsproj"
        |>  Seq.map 
        (fun f -> (Path.getDirectory f))
//---------------------------------------------------------------------------------------------------------------------------------
//======================================================= Build Tasks =============================================================
//---------------------------------------------------------------------------------------------------------------------------------

// --------------------------------------------------------------------------------------------------------------------------------
// Clean build results

let clean = 
    BuildTask.create "Clean" [] {
        Shell.cleanDirs ["bin"; "temp"; "pkg"]
    }

let cleanDocs = 
    BuildTask.create "CleanDocs" [] {
        Shell.cleanDirs ["docs"]
    }

// --------------------------------------------------------------------------------------------------------------------------------
// Generate assembly info files with the right version & up-to-date information. Clean first.

let assemblyInfo = 
    BuildTask.create "AssemblyInfo" [clean.IfNeeded] {
        let getAssemblyInfoAttributes projectName =
            [ AssemblyInfo.Title (projectName)
              AssemblyInfo.Product project
              AssemblyInfo.Description summary
              AssemblyInfo.Version release.AssemblyVersion
              AssemblyInfo.FileVersion release.AssemblyVersion
              AssemblyInfo.Configuration configuration ]

        let getProjectDetails projectPath =
            let projectName = Path.GetFileNameWithoutExtension(projectPath)
            ( projectPath,
              projectName,
              Path.GetDirectoryName(projectPath),
              (getAssemblyInfoAttributes projectName)
            )

        !! "src/**/*.fsproj"
        |> Seq.map getProjectDetails
        |> Seq.iter 
            (fun (projFileName, _, folderName, attributes) ->
                AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") attributes
            )
    }


// --------------------------------------------------------------------------------------------------------------------------------
// Build library & test project. build assembly info first

let buildAll = 
    BuildTask.create "BuildAll" [clean.IfNeeded; assemblyInfo] {
        let setParams (defaults:MSBuildParams) =
            { defaults with
                Verbosity = Some(Quiet)
                Targets = ["Build"]
                Properties =
                    [
                        "Optimize", "True"
                        "DebugSymbols", "True"
                        "Configuration", configuration
                    ]
             }
        MSBuild.build setParams solutionFile
    }

let buildMono = 
    BuildTask.create "BuildMono" [clean.IfNeeded; assemblyInfo] {
        let setParams (defaults:MSBuildParams) =
            { defaults with
                Verbosity = Some(Quiet)
                Targets = ["Build"]
                Properties =
                    [
                        "Optimize", "True"
                        "DebugSymbols", "True"
                        "Configuration", "Mono"
                    ]
             }
        MSBuild.build setParams solutionFile
    }

let buildDotnet = 
    BuildTask.create "BuildDotnet" [clean.IfNeeded; assemblyInfo] {
        solutionFile 
        |> DotNet.build (fun p -> 
            { p with
                Configuration = dotnetCoreConfiguration }
            )
    }

// --------------------------------------------------------------------------------------------------------------------------------
// Copies binaries from default VS location to expected bin folder
// But keeps a subdirectory structure for each project in the
// src folder to support multiple project outputs
// Build first.

let copyBinaries = 
        BuildTask.create "CopyBinaries" [clean.IfNeeded; assemblyInfo.IfNeeded; buildAll] {
        !! "src/**/*.fsproj"
        |>  Seq.map 
            (fun f -> (Path.getDirectory f) </> "bin" </> configuration, "bin" </> (Path.GetFileNameWithoutExtension f))
        |>  Seq.iter 
            (fun (fromDir, toDir) -> 
                printfn "copy from %s to %s" fromDir toDir
                Shell.copyDir toDir fromDir (fun _ -> true)
            )
    }

let copyBinariesMono = 
    BuildTask.create "CopyBinariesMono" [clean.IfNeeded; assemblyInfo.IfNeeded; buildMono] {
        //only select projects with the mono configuration
        !! "src/**/*.fsproj"
        -- "src/BioFSharp.BioDB/BioFSharp.BioDB.fsproj"
        |>  Seq.map (fun f -> (Path.getDirectory f) </> "bin" </> "Mono", "bin" </> (Path.GetFileNameWithoutExtension f))
        |>  Seq.iter 
            (fun (fromDir, toDir) ->   
                printfn "copy from %s to %s" fromDir toDir
                Shell.copyDir toDir fromDir (fun _ -> true)
            )
    }

let copyBinariesDotnet = 
    BuildTask.create "CopyBinariesDotnet" [clean.IfNeeded; assemblyInfo.IfNeeded; buildDotnet] {
        //only select projects with the dotnet configuration
        !! "src/**/*.fsproj"
        -- "src/BioFSharp.BioDB/BioFSharp.BioDB.fsproj"
        -- "src/BioFSharp.Parallel/BioFSharp.Parallel.fsproj" 
        -- "src/BioFSharp.ImgP/BioFSharp.ImgP.fsproj" 
        -- "src/BioFSharp.Vis/BioFSharp.Vis.fsproj"
        |>  Seq.map (fun f -> ((Path.getDirectory f) </> "bin" </> "DotnetCore", "bin" </> (Path.GetFileNameWithoutExtension f)))
        |>  Seq.iter 
            (fun (fromDir, toDir) ->   
                printfn "copy from %s to %s" fromDir toDir
                Shell.copyDir toDir fromDir (fun _ -> true)
            )
    }

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner. Of course build the assemblies to test first

let runTestsAll = 
    BuildTask.create "RunTests" [clean.IfNeeded; assemblyInfo.IfNeeded; copyBinaries.IfNeeded; buildAll] {
        let assemblies = !! testAssemblies

        assemblies
        |> Seq.iter (fun f ->
            Command.RawCommand (
                f,
                Arguments.OfArgs []
            )
            |> CreateProcess.fromCommand
            |> CreateProcess.withFramework
            |> CreateProcess.ensureExitCode
            |> Proc.run
            |> ignore
        )
    }

let runTestsMono = 
    BuildTask.create "RunTestsMono" [clean.IfNeeded; assemblyInfo.IfNeeded; copyBinariesMono.IfNeeded; buildMono] {
        let assemblies = !! testAssemblies

        assemblies
        |> Seq.iter (fun f ->
            Command.RawCommand (
                f,
                Arguments.OfArgs []
            )
            |> CreateProcess.fromCommand
            |> CreateProcess.withFramework
            |> CreateProcess.ensureExitCode
            |> Proc.run
            |> ignore
        )
    }

// --------------------------------------------------------------------------------------
// Build a NuGet package. Build and test packages first

let buildPrereleasePackages = 
    BuildTask.create "BuildPreReleasePackages" [buildAll; runTestsAll] {
        printfn "Please enter pre-release package suffix"
        let suffix = System.Console.ReadLine()
        Paket.pack(fun p -> 
            { p with
                
                ToolType = ToolType.CreateLocalTool()
                OutputPath = pkgDir
                Version = sprintf "%s-%s" release.NugetVersion suffix
                ReleaseNotes = release.Notes |> String.toLines })
    }

let BuildReleasePackages = 
    BuildTask.create "BuildReleasePackages" [buildAll; runTestsAll] {
        Paket.pack(fun p ->
            { p with
                ToolType = ToolType.CreateLocalTool()
                OutputPath = pkgDir
                Version = release.NugetVersion
                ReleaseNotes = release.Notes |> String.toLines })
    }

//dependencies for cI will be resolved in the CI build task.
let buildCIPackages name config projectPaths = 
    BuildTask.create (sprintf "BuildCIPackages%s" name) [buildAll.IfNeeded; runTestsAll.IfNeeded; buildMono.IfNeeded; runTestsMono.IfNeeded; buildDotnet.IfNeeded] {
        projectPaths
        |> Seq.iter 
            (fun proj -> 
                Paket.pack (fun p ->
                    { p with
                        BuildConfig = config
                        TemplateFile = proj </> "paket.template"
                        ToolType = ToolType.CreateLocalTool()
                        OutputPath = pkgDir
                        Version = sprintf "%s-%s" release.NugetVersion buildServerSuffix
                        ReleaseNotes = release.Notes |> String.toLines 
                    }
                )
            )
    }

let publishNugetPackages = 
    BuildTask.create "PublishNugetPackages" [buildAll; runTestsAll; BuildReleasePackages] {
        Paket.push(fun p ->
            { p with
                WorkingDir = pkgDir
                ToolType = ToolType.CreateLocalTool()
                ApiKey = Environment.environVarOrDefault "NuGet-key" "" })
    }

// --------------------------------------------------------------------------------------
// Generate the documentation

// Paths with template/source/output locations
let bin        = __SOURCE_DIRECTORY__ @@ "bin"
let content    = __SOURCE_DIRECTORY__ @@ "docsrc/content"
let output     = __SOURCE_DIRECTORY__ @@ "docs"
let files      = __SOURCE_DIRECTORY__ @@ "docsrc/files"
let templates  = __SOURCE_DIRECTORY__ @@ "docsrc/tools/templates"
let formatting = __SOURCE_DIRECTORY__ @@ "packages/formatting/FSharp.Formatting"
let docTemplate = "docpage.cshtml"

let github_release_user = Environment.environVarOrDefault "github_release_user" gitOwner
let githubLink = sprintf "https://github.com/%s/%s" github_release_user gitName

// Specify more information about your project
let info =
  [ "project-name", "BioFSharp"
    "project-author", "Timo Mühlhaus"
    "project-summary", "An open source bioinformatics toolbox written in F#. <https://csbiology.github.io/BioFSharp/>"
    "project-github", githubLink
    "project-nuget", "http://nuget.org/packages/BioFSharp" ]

let root = website

let referenceBinaries = []

let layoutRootsAll = new System.Collections.Generic.Dictionary<string, string list>()
layoutRootsAll.Add("en",[   templates;
                            formatting @@ "templates"
                            formatting @@ "templates/reference" ])

let buildReferenceDocs = 
    BuildTask.create "ReferenceDocs" [cleanDocs; buildAll.IfNeeded; runTestsAll.IfNeeded; buildMono.IfNeeded; runTestsMono.IfNeeded; buildDotnet.IfNeeded] {
        Directory.ensure (output @@ "reference")

        let binaries () =
            let manuallyAdded =
                referenceBinaries
                |> List.map (fun b -> bin @@ b)

            let conventionBased =
                DirectoryInfo.getSubDirectories <| DirectoryInfo bin
                |> Array.collect (fun d ->
                    let name, dInfo =
                        let net45Bin =
                            DirectoryInfo.getSubDirectories d |> Array.filter(fun x -> x.FullName.ToLower().Contains("net45"))
                        let net47Bin =
                            DirectoryInfo.getSubDirectories d |> Array.filter(fun x -> x.FullName.ToLower().Contains("net47"))
                        let netstandardBin =
                            DirectoryInfo.getSubDirectories d |> Array.filter(fun x -> x.FullName.ToLower().Contains("netstandard"))
                        if net45Bin.Length > 0 then
                            d.Name, net45Bin.[0]
                        elif net47Bin.Length > 0 then
                            d.Name, net47Bin.[0]
                        else
                            d.Name, netstandardBin.[0]

                    dInfo.GetFiles()
                    |> Array.filter (fun x ->
                        x.Name.ToLower() = (sprintf "%s.dll" name).ToLower())
                    |> Array.map (fun x -> x.FullName)
                    )
                |> List.ofArray

            conventionBased @ manuallyAdded

        binaries()
        |> FSFormatting.createDocsForDlls (fun args ->
            { args with
                OutputDirectory = output @@ "reference"
                LayoutRoots =  layoutRootsAll.["en"]
                ProjectParameters =  ("root", root)::info
                SourceRepository = githubLink @@ "tree/master" }
               )
    }

let copyFiles () =
    Shell.copyRecursive files output true
    |> Trace.logItems "Copying file: "
    Directory.ensure (output @@ "content")
    Shell.copyRecursive (formatting @@ "styles") (output @@ "content") true
    |> Trace.logItems "Copying styles and scripts: "

let buildDocs =  
    BuildTask.create "Docs" [cleanDocs; buildAll.IfNeeded; runTestsAll.IfNeeded; buildMono.IfNeeded; runTestsMono.IfNeeded; buildDotnet.IfNeeded] {
        File.delete "docsrc/content/release-notes.md"
        Shell.copyFile "docsrc/content/" "RELEASE_NOTES.md"
        Shell.rename "docsrc/content/release-notes.md" "docsrc/content/RELEASE_NOTES.md"

        File.delete "docsrc/content/license.md"
        Shell.copyFile "docsrc/content/" "LICENSE"
        Shell.rename "docsrc/content/license.md" "docsrc/content/LICENSE"


        DirectoryInfo.getSubDirectories (DirectoryInfo.ofPath templates)
        |> Seq.iter (fun d ->
                        let name = d.Name
                        if name.Length = 2 || name.Length = 3 then
                            layoutRootsAll.Add(
                                    name, [templates @@ name
                                           formatting @@ "templates"
                                           formatting @@ "templates/reference" ]))
        copyFiles ()

        for dir in  [ content; ] do
            let langSpecificPath(lang, path:string) =
                path.Split([|'/'; '\\'|], System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.exists(fun i -> i = lang)
            let layoutRoots =
                let key = layoutRootsAll.Keys |> Seq.tryFind (fun i -> langSpecificPath(i, dir))
                match key with
                | Some lang -> layoutRootsAll.[lang]
                | None -> layoutRootsAll.["en"] // "en" is the default language

            createDocs (fun args ->
                { args with
                    Source = content
                    OutputDirectory = output
                    LayoutRoots = layoutRoots
                    ProjectParameters  = ("root", root)::info
                    Template = docTemplate 
                    FsiEval = true
                    } )
    }

let generateDocumentation = BuildTask.createEmpty "GenerateDocs" [buildAll; buildReferenceDocs; buildDocs]

// --------------------------------------------------------------------------------------
// Release Scripts

//#load "paket-files/fsharp/FAKE/modules/Octokit/Octokit.fsx"
//open Octokit
let askForDocReleaseConfirmation =
    BuildTask.create "ReleaseDocsConfirmation" [] {
        match promptYesNo releaseDocsMsg with | true -> () |_ -> failwith "Release canceled"
    }
let releaseDocsToGhPages = 
    BuildTask.create "ReleaseDocs" [askForDocReleaseConfirmation;generateDocumentation] {
        let tempDocsDir = "temp/gh-pages"
        Shell.cleanDir tempDocsDir |> ignore
        Git.Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir
        Shell.copyRecursive "docs" tempDocsDir true |> printfn "%A"
        Git.Staging.stageAll tempDocsDir
        Git.Commit.exec tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
        Git.Branches.push tempDocsDir
    }

let buildLocalDocs = 
    BuildTask.create "BuildLocalDocs" [generateDocumentation] {
        let tempDocsDir = "temp/localDocs"
        Shell.cleanDir tempDocsDir |> ignore
        Shell.copyRecursive "docs" tempDocsDir true  |> printfn "%A"
        Shell.replaceInFiles 
            (seq {
                yield "href=\"/" + project + "/","href=\""
                yield "src=\"/" + project + "/","src=\""}) 
            (Directory.EnumerateFiles tempDocsDir |> Seq.filter (fun x -> x.EndsWith(".html")))
    }


let askForReleaseConfirmation = 
    BuildTask.create "ReleaseConfirmation" [] {
        match promptYesNo releaseMsg with | true -> () |_ -> failwith "Release canceled"
    }

let releaseOnGithub = 
    BuildTask.create "ReleaseonGithub" [askForReleaseConfirmation; buildAll; runTestsAll;] {
        Git.Staging.stageAll ""
        Git.Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
        Git.Branches.push ""

        Git.Branches.tag "" release.NugetVersion
        Git.Branches.pushTag "" "upstream" release.NugetVersion
    }

let askForGitReleaseNugetConfirmation = 
    BuildTask.create "GitReleaseNugetConfirmation" [] {
        match promptYesNo releaseMsg with | true -> () |_ -> failwith "Release canceled"
    }

let releaseNugetPackageOnGithub =
    BuildTask.create "GitReleaseNuget" [askForGitReleaseNugetConfirmation; buildAll; runTestsAll;] {
        let tempNugetDir = "temp/nuget"
        Shell.cleanDir tempNugetDir |> ignore
        Git.Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "nuget" tempNugetDir
        let files = Directory.EnumerateFiles pkgDir 
        Shell.copy tempNugetDir files
        Git.Staging.stageAll tempNugetDir
        Git.Commit.exec tempNugetDir (sprintf "Update git nuget packages for version %s" release.NugetVersion)
        Git.Branches.push tempNugetDir
        Shell.copy tempNugetDir files
   }

//Buildchains


//Token Targets for local builds
let fullBuildChainLocal = 
    BuildTask.createEmpty "FullLocal" [
        clean
        assemblyInfo
        buildAll
        copyBinaries
        runTestsAll
        cleanDocs
        generateDocumentation
        BuildReleasePackages
    ]

let monoBuildChainLocal = 
    BuildTask.createEmpty "MonoLocal"   [
        clean
        assemblyInfo
        buildMono
        copyBinariesMono
        runTestsMono
    ]

let dotnetBuildChainLocal = 
    BuildTask.createEmpty "DotnetLocal" [
        clean
        assemblyInfo
        buildDotnet
        copyBinariesDotnet
    ]

//Token Targets for CI builds
let fullBuildChainCI    = 
    BuildTask.createEmpty "FullCI" [
        clean
        assemblyInfo
        buildAll
        copyBinaries
        runTestsAll
        (buildCIPackages "AppveyorWindows" configuration allProjectPaths)
    ]

let monoBuildChainCI  = 
    BuildTask.createEmpty "CIBuildMono" [
        clean
        assemblyInfo
        buildMono
        copyBinariesMono
        runTestsMono
        (buildCIPackages "TravisMono" "Mono" monoProjectPaths)
    ]

let dotnetBuildChainCI    = 
    BuildTask.createEmpty "CIBuildDotnet" [
        clean
        assemblyInfo
        buildDotnet
        copyBinariesDotnet
        (buildCIPackages "AppveyorDotnet" "DotnetCore" dotnetProjectPaths)
    ]

Target.create "PrintGraph" (fun _ ->
    printfn "What target?"
    let t = System.Console.ReadLine()
    Target.printDependencyGraph true t
)

BuildTask.runOrDefaultWithArguments fullBuildChainLocal
