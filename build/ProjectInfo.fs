module ProjectInfo

open Fake.Core

let project = "BioFSharp"

let testProjects = [
    "tests/BioFSharp.Tests/BioFSharp.Tests.fsproj"
    "tests/BioFSharp.IO.Tests/BioFSharp.IO.Tests.fsproj"
]

let summary = "Open source bioinformatics and computational biology toolbox written in F#."

let solutionFile  = "BioFSharp.sln"

let configuration = "Release"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "CSBiology"

let gitName = "BioFSharp"

let gitHome = sprintf "%s/%s" "https://github.com" gitOwner

let projectRepo = sprintf "%s/%s/%s" "https://github.com" gitOwner gitName

let website = "/BioFSharp"

let pkgDir = "pkg"

let release = ReleaseNotes.load "RELEASE_NOTES.md"

let stableVersion = SemVer.parse release.NugetVersion

let stableVersionTag = (sprintf "%i.%i.%i" stableVersion.Major stableVersion.Minor stableVersion.Patch )

let mutable prereleaseSuffix = ""

let mutable prereleaseTag = ""

let mutable isPrerelease = false