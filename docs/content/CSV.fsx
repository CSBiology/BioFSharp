(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "../../packages/build/FSharp.Plotly/lib/net45/Fsharp.Plotly.dll"
open FSharp.Plotly

#I "../../bin"
#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"
#r "FSharp.Care.dll"
#r "FSharp.Care.IO.dll"


(**
Seqperated format I/O
=====================

Reading .csv files
-------------------
*)

open FSharp.Care.IO
open FSharp.Care.IO.SchemaReader
open FSharp.Care.IO.SchemaReader.Csv
open FSharp.Care.IO.SchemaReader.Attribute

// ##################################################################
// Examples: Csv-reader reads iris data set
type irisItem = 
    {   [<FieldAttribute("Sepal length")>] 
        SepalLength : float
        [<FieldAttribute("Sepal width")>] 
        SepalWidth : float
        [<FieldAttribute("Petal length")>] 
        PetalLength : float
        [<FieldAttribute("Petal width")>] 
        PetalWidth : float
        [<FieldAttribute("Species")>] 
        Species : string 
          }




let path = __SOURCE_DIRECTORY__ + "/data/irisData.csv" //....
let hasHeader = true
let separator = ','
let data = 
    Seq.fromFileWithCsvSchema<irisItem>(path, separator, hasHeader)
    |> Seq.map (fun ii -> ( ii.SepalLength, ii.SepalWidth) )
    |> Seq.toList
    

(*** define-output:pie1 ***)
Chart.Point(data,Name="Sepal: length vs. width")
(*** include-it:pie1 ***)
