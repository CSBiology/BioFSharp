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

//This is ugly i bet you can find a better way ;)
let mutable prereleaseTag = ""
let mutable isPrerelease = false

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
    BuildTask.create "clean" [] {
        Shell.cleanDirs [
            "bin"; "temp"; "pkg" 
            yield! allProjectPaths |> Seq.map (fun x -> x </> "bin")
            ]
    }

let cleanDocs = 
    BuildTask.create "cleanDocs" [] {
        Shell.cleanDirs ["docs"]
    }

// --------------------------------------------------------------------------------------------------------------------------------
// Generate assembly info files with the right version & up-to-date information. Clean first.

let assemblyInfo = 
    BuildTask.create "assemblyInfo" [clean.IfNeeded] {
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
    BuildTask.create "buildAll" [clean.IfNeeded; assemblyInfo] {
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
    BuildTask.create "buildMono" [clean.IfNeeded; assemblyInfo] {
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
    BuildTask.create "buildDotnet" [clean.IfNeeded; assemblyInfo] {
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
        BuildTask.create "copyBinaries" [clean.IfNeeded; assemblyInfo.IfNeeded; buildAll] {
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
    BuildTask.create "copyBinariesMono" [clean.IfNeeded; assemblyInfo.IfNeeded; buildMono] {
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
    BuildTask.create "copyBinariesDotnet" [clean.IfNeeded; assemblyInfo.IfNeeded; buildDotnet] {
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
    BuildTask.create "runTestsAll" [clean.IfNeeded; assemblyInfo.IfNeeded; copyBinaries.IfNeeded; buildAll] {
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
    BuildTask.create "runTestsMono" [clean.IfNeeded; assemblyInfo.IfNeeded; copyBinariesMono.IfNeeded; buildMono] {
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
    BuildTask.create "buildPrereleasePackages" [buildAll; runTestsAll] {
        printfn "Please enter pre-release package suffix"
        let suffix = System.Console.ReadLine()
        isPrerelease <- true
        prereleaseTag <- (sprintf "%s-%s" release.NugetVersion suffix)
        if promptYesNo (sprintf "package tag will be %s OK?" prereleaseTag )
            then 
                Paket.pack(fun p -> 
                    { p with
                
                        ToolType = ToolType.CreateLocalTool()
                        OutputPath = pkgDir
                        Version = prereleaseTag
                        ReleaseNotes = release.Notes |> String.toLines })
            else 
                failwith "aborted"
    }

let BuildReleasePackages = 
    BuildTask.create "BuildReleasePackages" [buildAll; runTestsAll] {
        isPrerelease <- false
        Paket.pack(fun p ->
            { p with
                ToolType = ToolType.CreateLocalTool()
                OutputPath = pkgDir
                Version = release.NugetVersion
                ReleaseNotes = release.Notes |> String.toLines })
    }

//dependencies for cI will be resolved in the CI build task.
let buildCIPackages name config projectPaths = 
    BuildTask.create (sprintf "buildCIPackages%s" name) [
        buildAll.IfNeeded; runTestsAll.IfNeeded; copyBinaries.IfNeeded
        buildMono.IfNeeded; runTestsMono.IfNeeded; copyBinariesMono.IfNeeded
        buildDotnet.IfNeeded; copyBinariesDotnet.IfNeeded
        ] 
        {
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
    BuildTask.create "publishNugetPackages" [clean; buildAll; runTestsAll; BuildReleasePackages] {
        Paket.push(fun p ->
            { p with
                WorkingDir = pkgDir
                ToolType = ToolType.CreateLocalTool()
                ApiKey = Environment.environVarOrDefault "NuGet-key" "" })
    }

let publishPrereleaseNugetPackages = 
    BuildTask.create "publishPrereleaseNugetPackages" [clean; buildAll; runTestsAll; buildPrereleasePackages] {
        Paket.push(fun p ->
            { p with
                WorkingDir = pkgDir
                ToolType = ToolType.CreateLocalTool()
                ApiKey = Environment.environVarOrDefault "NuGet-key" "" })
    }

// --------------------------------------------------------------------------------------
// Generate the documentation

let generateDocumentation = 
    BuildTask.create "generateDocumentation" [cleanDocs; buildAll.IfNeeded] {
        let result =
            DotNet.exec
                (fun p -> { p with WorkingDirectory = __SOURCE_DIRECTORY__ @@ "docsrc" @@ "tools" })
                "fsi"
                "--define:RELEASE --define:REFERENCE --define:HELP --exec generate.fsx"

        if not result.OK then 
            failwith "error generating docs" 
    }

let generateSingleDocumentation =

    BuildTask.createFn "generateSingleDocumentation" [cleanDocs; buildAll.IfNeeded] ( fun p ->

        let docsPage = __SOURCE_DIRECTORY__ @@ "docsrc/content/" @@ p.Context.Arguments.[0]

        let result =
            DotNet.exec
                (fun p -> { p with WorkingDirectory = __SOURCE_DIRECTORY__ @@ "docsrc" @@ "tools" })
                "fsi"
                (sprintf "--define:RELEASE --define:REFERENCE --define:HELP --exec --use:generateSingle.fsx %s" docsPage)

        if not result.OK then 
            failwith "error generating docs" 
    )

// --------------------------------------------------------------------------------------
// Release Scripts

//#load "paket-files/fsharp/FAKE/modules/Octokit/Octokit.fsx"
//open Octokit
let askForDocReleaseConfirmation =
    BuildTask.create "askForDocReleaseConfirmation" [] {
        match promptYesNo releaseDocsMsg with | true -> () |_ -> failwith "Release canceled"
    }
let releaseDocsToGhPages = 
    BuildTask.create "releaseDocsToGhPages" [askForDocReleaseConfirmation;generateDocumentation] {
        let tempDocsDir = "temp/gh-pages"
        Shell.cleanDir tempDocsDir |> ignore
        Git.Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir
        Shell.copyRecursive "docs/output" tempDocsDir true |> printfn "%A"
        Git.Staging.stageAll tempDocsDir
        Git.Commit.exec tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
        Git.Branches.push tempDocsDir
    }
let askForReleaseConfirmation = 
    BuildTask.create "askForReleaseConfirmation" [] {
        match promptYesNo releaseMsg with | true -> () |_ -> failwith "Release canceled"
    }

let releaseOnGithub = 
    BuildTask.create "releaseOnGithub" [askForReleaseConfirmation; buildAll; runTestsAll;] {
        Git.Staging.stageAll ""
        Git.Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
        Git.Branches.push ""

        Git.Branches.tag "" release.NugetVersion
        Git.Branches.pushTag "" "upstream" release.NugetVersion
    }

let askForGitReleaseNugetConfirmation = 
    BuildTask.create "askForGitReleaseNugetConfirmation" [] {
        match promptYesNo releaseMsg with | true -> () |_ -> failwith "Release canceled"
    }

let releaseNugetPackageOnGithub =
    BuildTask.create "releaseNugetPackageOnGithub" [askForGitReleaseNugetConfirmation; buildAll; runTestsAll; buildPrereleasePackages.IfNeeded; BuildReleasePackages.IfNeeded] {
        let tempNugetDir = "temp/nuget"
        Shell.cleanDir tempNugetDir |> ignore
        Git.Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "nuget" tempNugetDir
        let files = Directory.EnumerateFiles pkgDir 
        Shell.copy tempNugetDir files
        Git.Staging.stageAll tempNugetDir
        let version = if isPrerelease then prereleaseTag else release.NugetVersion
        Git.Commit.exec tempNugetDir (sprintf "Update git nuget packages for version %s" version)
        Git.Branches.push tempNugetDir
        Shell.copy tempNugetDir files
    }

// --------------------------------------------------------------------------------------
// Local Docs
let buildLocalDocs = 
    BuildTask.create "buildLocalDocs" [generateDocumentation] {
        let tempDocsDir = "temp/localDocs"
        Shell.cleanDir tempDocsDir |> ignore
        Shell.copyRecursive "docs/output" tempDocsDir true  |> printfn "%A"
        Shell.replaceInFiles 
            (seq {
                yield "href=\"/" + project + "/","href=\""
                yield "src=\"/" + project + "/","src=\""}) 
            (Directory.EnumerateFiles tempDocsDir |> Seq.filter (fun x -> x.EndsWith(".html")))
    }

let testSingleDocumentationPage =

    BuildTask.createFn "testSingleDocumentationPage" [generateSingleDocumentation] ( fun p ->
        let docsPage = 
            p.Context.Arguments.[0]
                .Replace(".fsx",".html")
        let docsPageName = Path.GetFileName docsPage
        let tempDocsDir = "temp/localDocs"
        Shell.cleanDir tempDocsDir |> ignore
        Shell.copyFile ("temp/localDocs" </> docsPageName) ("docs/output" </> docsPageName)  |> printfn "%A"
        Shell.replaceInFiles 
            (seq {
                yield "href=\"/" + project + "/","href=\""
                yield "src=\"/" + project + "/","src=\""}) 
            (Directory.EnumerateFiles tempDocsDir |> Seq.filter (fun x -> x.EndsWith(".html")))
        let psi = new System.Diagnostics.ProcessStartInfo(FileName = (__SOURCE_DIRECTORY__ </> "temp/localDocs" </> docsPageName), UseShellExecute = true)
        System.Diagnostics.Process.Start(psi) |> ignore
    )

let inspectLocalDocs =
    BuildTask.create "inspectLocalDocs" [buildLocalDocs] {
        let psi = new System.Diagnostics.ProcessStartInfo(FileName = (__SOURCE_DIRECTORY__ </> "temp/localDocs/index.html"), UseShellExecute = true)
        System.Diagnostics.Process.Start(psi) |> ignore
    }


//Buildchains


//Token Targets for local builds
let fullBuildChainLocal = 
    BuildTask.createEmpty "fullBuildChainLocal" [
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
    BuildTask.createEmpty "monoBuildChainLocal"   [
        clean
        assemblyInfo
        buildMono
        copyBinariesMono
        runTestsMono
    ]

let dotnetBuildChainLocal = 
    BuildTask.createEmpty "dotnetBuildChainLocal" [
        clean
        assemblyInfo
        buildDotnet
        copyBinariesDotnet
    ]

//Push prerelease Package to github
let prereleasePackagesToGithub =
     BuildTask.createEmpty "prereleasePackagesToGithub" [
        clean
        assemblyInfo
        buildAll
        copyBinaries
        runTestsAll
        buildPrereleasePackages
        releaseNugetPackageOnGithub
     ]


//Token Targets for CI builds
let fullBuildChainCI    = 
    BuildTask.createEmpty "fullBuildChainCI" [
        clean
        assemblyInfo
        buildAll
        copyBinaries
        runTestsAll
        (buildCIPackages "AppveyorWindows" configuration allProjectPaths)
    ]

let monoBuildChainCI  = 
    BuildTask.createEmpty "monoBuildChainCI" [
        clean
        assemblyInfo
        buildMono
        copyBinariesMono
        runTestsMono
        //(buildCIPackages "TravisMono" "Mono" monoProjectPaths)
    ]

let dotnetBuildChainCI    = 
    BuildTask.createEmpty "dotnetBuildChainCI" [
        clean
        assemblyInfo
        buildDotnet
        copyBinariesDotnet
        //(buildCIPackages "AppveyorDotnet" "DotnetCore" dotnetProjectPaths)
    ]

let buildDocsChain = 
    BuildTask.createEmpty "buildDocsChain" [
        clean
        assemblyInfo
        buildAll
        copyBinaries
        runTestsAll
        cleanDocs
        generateDocumentation
    ]


Target.create "PrintGraph" (fun _ ->
    printfn "What target?"
    let t = System.Console.ReadLine()
    Target.printDependencyGraph true t
)

BuildTask.runOrDefaultWithArguments fullBuildChainLocal
