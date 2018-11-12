namespace BioFSharp.BioDB

module GEO =

    open System
    open System.IO
    open System.Net
    open System.Net.Security

    [<Literal>]
    let rootGEOAdress = @"ftp://ftp.ncbi.nlm.nih.gov/geo"

    module private FTPHelpers = 

        let constructFTPAdress (additionalPath: string) =
            sprintf "%s/%s" rootGEOAdress additionalPath

        let getFileSize (filePath:string) = 
            let req  = WebRequest.Create(filePath) //:?> FtpWebRequest
            req.Method <- WebRequestMethods.Ftp.GetFileSize
            let res = req.GetResponse() //:?> FtpWebResponse
            let x = res
            res.Close()
            x.ContentLength

        let getDateModified (filePath:string) = 
            let req = FtpWebRequest.Create(filePath) :?> FtpWebRequest
            req.Method <- WebRequestMethods.Ftp.GetDateTimestamp
            let res = req.GetResponse()  :?> FtpWebResponse
            let x = res.LastModified
            res.Close()
            x

        let downloadFile (filePath:string) (destination:string) =
            let req = FtpWebRequest.Create(filePath) :?> FtpWebRequest
            req.Method <- WebRequestMethods.Ftp.DownloadFile
            let res = req.GetResponse()  :?> FtpWebResponse
            let stream = res.GetResponseStream()
            let reader = new StreamReader(stream)
            let data = reader.ReadToEnd()
            res.Close()
            reader.Close()
            use writer = new StreamWriter(destination)
            data
            |> Seq.iter writer.WriteLine

        let downloadBinaryFile (filePath:string) (destination:string) =
            let req = FtpWebRequest.Create(filePath) :?> FtpWebRequest
            req.Method <- WebRequestMethods.Ftp.DownloadFile
            let res = req.GetResponse()  :?> FtpWebResponse
            let stream = res.GetResponseStream()
            let buffer : byte [] = Array.zeroCreate 1024
            let fileStream = new FileStream(destination,FileMode.Create,FileAccess.Write)
            let rec loop amountRead =
                if amountRead > 0 then
                    //printfn "%i" amountRead
                    fileStream.Write (buffer, 0, amountRead)
                    loop (stream.Read(buffer,0,buffer.Length))
                else
                    fileStream.Dispose()
                    stream.Close()
                    res.Close()
            loop (stream.Read(buffer,0,buffer.Length))

    open FTPHelpers

    type GEODataFile = {
        FileName: string
        FullPath: string
        Size: int64
        DateModified : System.DateTime
    }

    let private createGEODataFile fn fp s dm = {FileName=fn;FullPath=fp;Size=s;DateModified=dm}

    let getFTPDirectoryContents (path:string) =

        let fileNameRequest = FtpWebRequest.Create(path) :?> FtpWebRequest
        fileNameRequest.Method <- WebRequestMethods.Ftp.ListDirectory
        let fileNameResponse = fileNameRequest.GetResponse() :?> FtpWebResponse
        let fileNameResponseStream = fileNameResponse.GetResponseStream()
        let fileNameReader = new StreamReader(fileNameResponseStream)
    
        let fileNames = 
            [|while (not fileNameReader.EndOfStream) do yield fileNameReader.ReadLine()|]
            |> Array.map (fun x -> x.Split('/') |> Array.last)
        fileNameResponse.Close()
        fileNameReader.Close()
    
        let fileSizes =
            fileNames
            |> Array.map (fun fileName ->   let fullPath = (sprintf "%s/%s" path fileName)
                                            getFileSize fullPath)

        let datesModified =
            fileNames
            |> Array.map (fun fileName ->   let fullPath = (sprintf "%s/%s" path fileName)
                                            getDateModified fullPath)

        fileNames
        |> Array.mapi (fun i fileName -> createGEODataFile fileName (sprintf "%s%s" path fileName) fileSizes.[i] datesModified.[i])

    let downloadGEOFile (file:GEODataFile) (savePath:string) =
        let binaryExtensions = [|".zip";".gz";".rar"|]
        if Array.contains (Path.GetExtension file.FullPath) binaryExtensions then 
            downloadBinaryFile file.FullPath savePath
        else
            downloadFile file.FullPath savePath
        

    let listDirectories (path:string) =
        let req = FtpWebRequest.Create(path) :?> FtpWebRequest
        req.Method <- WebRequestMethods.Ftp.ListDirectoryDetails
        use Response = req.GetResponse() :?> FtpWebResponse
        let ResponseStream = Response.GetResponseStream()
        use Reader = new StreamReader(ResponseStream)
        let dirNames = 
            [|while (not Reader.EndOfStream) do yield Reader.ReadLine()|]
            |> Array.map (fun x -> x.Split(' ') |> Array.last)
        dirNames

    //let tryGetContents x =
    //    try getFTPDirectoryContents x  with | e as exn -> [|failedReq (sprintf "%s : %s" x e.Message)|]