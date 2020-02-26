module Formatters
#I "../../packages/formatting/FSharp.Formatting"
#load "FSharp.Formatting.fsx"
#r "../../packages/formatting/FSharp.Plotly/lib/netstandard2.0/FSharp.Plotly.dll"
#r "../../packages/formatting/FSharp.Compiler.Service/lib/netstandard2.0/FSharp.Compiler.Service.dll"
#r "netstandard"

open System.IO
open FSharp.Literate
open FSharp.Markdown
open FSharp.Plotly

let ensureDirectory dir =
    let di = new DirectoryInfo(dir)
    if not di.Exists then di.Create()

/// Combine two paths
let (@@) a b = Path.Combine(a, b)

let createFsiEvaluator root output (floatFormat:string) =

  /// Counter for saving files
    let imageCounter = 
        let count = ref 0
        (fun () -> incr count; !count)

    let transformation (value:obj, typ:System.Type) =
        match value with 
        | :? System.Drawing.Image as img ->
            // Pretty print image - save the image to the "images" directory 
            // and return a DirectImage reference to the appropriate location
            let id = imageCounter().ToString()
            let file = "chart" + id + ".png"
            ensureDirectory (output @@ "images")
            img.Save(output @@ "images" @@ file, System.Drawing.Imaging.ImageFormat.Png) 
            Some [ Paragraph([DirectImage ("", root + "/images/" + file, None, None)], None) ]
        | :? GenericChart.GenericChart as ch ->
            // printfn "Chart detected"
            // Just return the inline HTML for a Plotly chart        
            let html = GenericChart.toChartHtmlWithSize 700 500 ch
            let result = Some [InlineBlock <| (html,None)]
            // printfn "%A" result
            result
        | _ -> None 

    let fsiEvaluator = FsiEvaluator() 
    fsiEvaluator.RegisterTransformation(transformation)
    fsiEvaluator