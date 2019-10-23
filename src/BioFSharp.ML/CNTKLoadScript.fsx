open System
open System.IO

  
let dependencies = 
    [
        @"../../packages/CNTK.CPUOnly/lib/netstandard2.0"
        @"../../packages/CNTK.CPUOnly/support/x64/Release"
        @"../../packages/CNTK.CPUOnly/support/x64/Release"
        @"../../packages/CNTK.Deps.MKL/support/x64/Dependency"
        @"../../packages/CNTK.Deps.OpenCV.Zip/support/x64/Dependency"
    ]

let resolveCNTKDependencies () =
    Environment.SetEnvironmentVariable("Path",
        Environment.GetEnvironmentVariable("Path") + ";" + __SOURCE_DIRECTORY__ )
    dependencies 
    |> Seq.iter (fun dep -> 
        let path = Path.Combine(__SOURCE_DIRECTORY__,dep)
        Environment.SetEnvironmentVariable("Path",
            Environment.GetEnvironmentVariable("Path") + ";" + path)
        )    


#I @"../../packages/CNTK.CPUOnly/lib/netstandard2.0"
#I @"../../packages/CNTK.CPUOnly/support/x64/Release"
#I @"../../packages/CNTK.CPUOnly/support/x64/Release"
#I @"../../packages/CNTK.Deps.MKL/support/x64/Dependency"
#I @"../../packages/CNTK.Deps.OpenCV.Zip/support/x64/Dependency"
#I @"../../bin"

 
#r @"../../packages/CNTK.CPUOnly/lib/netstandard2.0/Cntk.Core.Managed-2.7.dll"
#r "netstandard"