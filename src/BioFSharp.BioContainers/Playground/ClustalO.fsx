#r "netstandard"
#r "../../../packages/Newtonsoft.Json/lib/netstandard2.0/Newtonsoft.Json.dll"
#r "../../../packages/System.Buffers/lib/netstandard2.0/System.Buffers.dll"
#r "../../../packages/Docker.DotNet/lib/netstandard2.0/Docker.DotNet.dll"
#r "../../../packages/SharpZipLib/lib/netstandard2.0/ICSharpCode.SharpZipLib.dll"
#r "../../../packages/SharpZipLib/lib/netstandard2.0/ICSharpCode.SharpZipLib.dll"
#r "../../../packages/FSharpAux/lib/netstandard2.0/FSharpAux.dll"
#r "../../../packages/FSharpAux.IO/lib/netstandard2.0/FSharpAux.IO.dll"

#load "../Docker.fs"
#load "../BioContainerIO.fs"
#load "../BioContainer.fs"
#load "../ClustalO.fs"

open BioFSharp.BioContainers.ClustalO