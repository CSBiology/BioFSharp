namespace BioFSharp.BioContainers

open System
open System.Threading
open Docker.DotNet
open Docker.DotNet.Models
open System.IO

/// BioContainer helper
module BioContainerIO =

    open ICSharpCode.SharpZipLib.GZip
    open ICSharpCode.SharpZipLib.Tar
    open System.IO
 
    let toUnixDirectorySeparator (filename:string) = 
        let dirSep = "/"
        filename.Replace("\\", dirSep)
            .TrimEnd(Path.DirectorySeparatorChar)

    let directoryName (filename:string) = 
        let dirSep = Path.DirectorySeparatorChar
        let tmp    = filename.Split([|dirSep;'/'|])
        tmp
        |> Seq.take (tmp.Length-1)
        |> String.concat ("/")
        |> fun s -> s + "/"

    let fileName (filename:string) = 
        let dirSep = Path.DirectorySeparatorChar
        let tmp    = filename.Split([|dirSep;'/'|])
        let last = tmp.Length-1
        if last > 0 then tmp.[last] else ""

    let readFrom (stream:System.IO.Stream) =
        let length = (stream.Length) |> int
        let tmp : array<byte> = Array.zeroCreate length
        stream.Read(tmp,0,length) |> ignore

        System.Text.Encoding.UTF8.GetString(tmp,0,length)

    // Returns the first file as stream
    let tarToStream inStream =
        let tarIn = new TarInputStream(inStream)
        if tarIn.GetNextEntry().IsDirectory then 
            tarIn.GetNextEntry() |> ignore
            tarIn :> Stream
        else
            tarIn :> Stream
   

    /// Returns a tar-archive MemoryStream (only one entry supported) 
    let tarOfStream (tarEntryName:string) (inputStream:Stream) =
        let outStream = new MemoryStream()
        let tarOutputStream = new TarOutputStream(outStream) 

        let fileSize = inputStream.Length
        let entry = TarEntry.CreateTarEntry(tarEntryName)
        // Must set size, otherwise TarOutputStream will fail when output exceeds.
        entry.Size <- fileSize
        // Add the entry to the tar stream, before writing the data.
        tarOutputStream.PutNextEntry(entry)
        // this is copied from TarArchive.WriteEntryCore
        let localBuffer : byte [] = Array.zeroCreate (32 * 1024)
        let rec loop () =
            let numRead = inputStream.Read(localBuffer, 0, localBuffer.Length)
            if (numRead <= 0) then
                tarOutputStream.CloseEntry()
            else
                tarOutputStream.Write(localBuffer, 0, numRead)
                loop ()
    
        loop ()
        tarOutputStream.IsStreamOwner <- false
        tarOutputStream.Close()
        outStream.Position <- 0L
        outStream

