namespace BioFSharp.Interactive

open System
open System.Threading.Tasks
open Microsoft.DotNet.Interactive
open Microsoft.DotNet.Interactive.Formatting
open BioFSharp
open BioFSharp.IO

type FormatterKernelExtension() =

    interface IKernelExtension with
        member _.OnLoadAsync _ =
            Formatter.Register<AminoAcids.AminoAcid>(
                Action<_, _>
                    (fun item (writer: IO.TextWriter) ->
                        let pretty = FSIPrinters.prettyPrintBioItem item
                        writer.Write(pretty)),
                "text/html"
            )
            Formatter.Register<seq<IBioItem>>(
                Action<_, _>
                    (fun bioCollection (writer: IO.TextWriter) ->
                        let pretty = $"<pre>{FSIPrinters.prettyPrintBioCollection bioCollection}</pre>"
                        writer.Write(pretty)),
                "text/html"
            )
            Task.CompletedTask
