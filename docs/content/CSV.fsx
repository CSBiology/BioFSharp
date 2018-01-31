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
<table class="HeadAPI">
<td class="Head"><h1>CSV Format</h1></td>
<td class="API">
    <a id="APILink" href="http://csbiology.github.io/FSharp.Care/reference/fsharp-care-io-schemareader.html" >&#128194;View module documentation</a>
</td>
</table>
The CSV reader is a straightforward, modular tool for reading tabular data.  
As a first step the input type has to be modelled. This type should represent the information of one line of the given file. The fields have to be marked as `FieldAttribute`s with the according column header, or if with the according line numbers. 

*)
open FSharp.Care.IO
open FSharp.Care.IO.SchemaReader
open FSharp.Care.IO.SchemaReader.Csv
open FSharp.Care.IO.SchemaReader.Attribute

type irisItem = 
    {
        [<FieldAttribute("Sepal length")>]
        SepalLength : float;
        [<FieldAttribute("Sepal width")>]
        SepalWidth : float;
        [<FieldAttribute("Petal length")>]
        PetalLength : float;
        [<FieldAttribute("Petal width")>]
        PetalWidth : float;
        [<FieldAttribute("Species")>]
        Species : string;
        }

(**
<br>
The according file can be seen here:  
<button type="button" class="btn" data-toggle="collapse" data-target="#csvExample">Show/Hide example</button>
<div id="csvExample" class="collapse csvExample ">
<pre>
Sepal length,Sepal width,Petal length,Petal width,Species
 5.1,3.5,1.4,0.2,Iris-setosa
 4.9,3.0,1.4,0.2,Iris-setosa
 4.7,3.2,1.3,0.2,Iris-setosa
 4.6,3.1,1.5,0.2,Iris-setosa
 5.0,3.6,1.4,0.2,Iris-setosa
 5.4,3.9,1.7,0.4,Iris-setosa
 4.6,3.4,1.4,0.3,Iris-setosa
 5.0,3.4,1.5,0.2,Iris-setosa
 4.4,2.9,1.4,0.2,Iris-setosa
 4.9,3.1,1.5,0.1,Iris-setosa
 5.4,3.7,1.5,0.2,Iris-setosa
 4.8,3.4,1.6,0.2,Iris-setosa
 4.8,3.0,1.4,0.1,Iris-setosa
 4.3,3.0,1.1,0.1,Iris-setosa
 5.8,4.0,1.2,0.2,Iris-setosa
 5.7,4.4,1.5,0.4,Iris-setosa
 5.4,3.9,1.3,0.4,Iris-setosa
 5.1,3.5,1.4,0.3,Iris-setosa
 5.7,3.8,1.7,0.3,Iris-setosa
 5.1,3.8,1.5,0.3,Iris-setosa
 5.4,3.4,1.7,0.2,Iris-setosa
 5.1,3.7,1.5,0.4,Iris-setosa
 4.6,3.6,1.0,0.2,Iris-setosa
 5.1,3.3,1.7,0.5,Iris-setosa
 4.8,3.4,1.9,0.2,Iris-setosa
 5.0,3.0,1.6,0.2,Iris-setosa
 5.0,3.4,1.6,0.4,Iris-setosa
 5.2,3.5,1.5,0.2,Iris-setosa
 5.2,3.4,1.4,0.2,Iris-setosa
 4.7,3.2,1.6,0.2,Iris-setosa
 4.8,3.1,1.6,0.2,Iris-setosa
 5.4,3.4,1.5,0.4,Iris-setosa
 5.2,4.1,1.5,0.1,Iris-setosa
 5.5,4.2,1.4,0.2,Iris-setosa
 4.9,3.1,1.5,0.1,Iris-setosa
 5.0,3.2,1.2,0.2,Iris-setosa
 5.5,3.5,1.3,0.2,Iris-setosa
 4.9,3.1,1.5,0.1,Iris-setosa
 4.4,3.0,1.3,0.2,Iris-setosa
 5.1,3.4,1.5,0.2,Iris-setosa
 5.0,3.5,1.3,0.3,Iris-setosa
 4.5,2.3,1.3,0.3,Iris-setosa
 4.4,3.2,1.3,0.2,Iris-setosa
 5.0,3.5,1.6,0.6,Iris-setosa
 5.1,3.8,1.9,0.4,Iris-setosa
 4.8,3.0,1.4,0.3,Iris-setosa
 5.1,3.8,1.6,0.2,Iris-setosa
 4.6,3.2,1.4,0.2,Iris-setosa
 5.3,3.7,1.5,0.2,Iris-setosa
 5.0,3.3,1.4,0.2,Iris-setosa
 7.0,3.2,4.7,1.4,Iris-versicolor
 6.4,3.2,4.5,1.5,Iris-versicolor
 6.9,3.1,4.9,1.5,Iris-versicolor
 5.5,2.3,4.0,1.3,Iris-versicolor
 6.5,2.8,4.6,1.5,Iris-versicolor
 5.7,2.8,4.5,1.3,Iris-versicolor
 6.3,3.3,4.7,1.6,Iris-versicolor
 4.9,2.4,3.3,1.0,Iris-versicolor
 6.6,2.9,4.6,1.3,Iris-versicolor
 5.2,2.7,3.9,1.4,Iris-versicolor
 5.0,2.0,3.5,1.0,Iris-versicolor
 5.9,3.0,4.2,1.5,Iris-versicolor
 6.0,2.2,4.0,1.0,Iris-versicolor
 6.1,2.9,4.7,1.4,Iris-versicolor
 5.6,2.9,3.6,1.3,Iris-versicolor
 6.7,3.1,4.4,1.4,Iris-versicolor
 5.6,3.0,4.5,1.5,Iris-versicolor
 5.8,2.7,4.1,1.0,Iris-versicolor
 6.2,2.2,4.5,1.5,Iris-versicolor
 5.6,2.5,3.9,1.1,Iris-versicolor
 5.9,3.2,4.8,1.8,Iris-versicolor
 6.1,2.8,4.0,1.3,Iris-versicolor
 6.3,2.5,4.9,1.5,Iris-versicolor
 6.1,2.8,4.7,1.2,Iris-versicolor
 6.4,2.9,4.3,1.3,Iris-versicolor
 6.6,3.0,4.4,1.4,Iris-versicolor
 6.8,2.8,4.8,1.4,Iris-versicolor
 6.7,3.0,5.0,1.7,Iris-versicolor
 6.0,2.9,4.5,1.5,Iris-versicolor
 5.7,2.6,3.5,1.0,Iris-versicolor
 5.5,2.4,3.8,1.1,Iris-versicolor
 5.5,2.4,3.7,1.0,Iris-versicolor
 5.8,2.7,3.9,1.2,Iris-versicolor
 6.0,2.7,5.1,1.6,Iris-versicolor
 5.4,3.0,4.5,1.5,Iris-versicolor
 6.0,3.4,4.5,1.6,Iris-versicolor
 6.7,3.1,4.7,1.5,Iris-versicolor
 6.3,2.3,4.4,1.3,Iris-versicolor
 5.6,3.0,4.1,1.3,Iris-versicolor
 5.5,2.5,4.0,1.3,Iris-versicolor
 5.5,2.6,4.4,1.2,Iris-versicolor
 6.1,3.0,4.6,1.4,Iris-versicolor
 5.8,2.6,4.0,1.2,Iris-versicolor
 5.0,2.3,3.3,1.0,Iris-versicolor
 5.6,2.7,4.2,1.3,Iris-versicolor
 5.7,3.0,4.2,1.2,Iris-versicolor
 5.7,2.9,4.2,1.3,Iris-versicolor
 6.2,2.9,4.3,1.3,Iris-versicolor
 5.1,2.5,3.0,1.1,Iris-versicolor
 5.7,2.8,4.1,1.3,Iris-versicolor
 6.3,3.3,6.0,2.5,Iris-virginica
 5.8,2.7,5.1,1.9,Iris-virginica
 7.1,3.0,5.9,2.1,Iris-virginica
 6.3,2.9,5.6,1.8,Iris-virginica
 6.5,3.0,5.8,2.2,Iris-virginica
 7.6,3.0,6.6,2.1,Iris-virginica
 4.9,2.5,4.5,1.7,Iris-virginica
 7.3,2.9,6.3,1.8,Iris-virginica
 6.7,2.5,5.8,1.8,Iris-virginica
 7.2,3.6,6.1,2.5,Iris-virginica
 6.5,3.2,5.1,2.0,Iris-virginica
 6.4,2.7,5.3,1.9,Iris-virginica
 6.8,3.0,5.5,2.1,Iris-virginica
 5.7,2.5,5.0,2.0,Iris-virginica
 5.8,2.8,5.1,2.4,Iris-virginica
 6.4,3.2,5.3,2.3,Iris-virginica
 6.5,3.0,5.5,1.8,Iris-virginica
 7.7,3.8,6.7,2.2,Iris-virginica
 7.7,2.6,6.9,2.3,Iris-virginica
 6.0,2.2,5.0,1.5,Iris-virginica
 6.9,3.2,5.7,2.3,Iris-virginica
 5.6,2.8,4.9,2.0,Iris-virginica
 7.7,2.8,6.7,2.0,Iris-virginica
 6.3,2.7,4.9,1.8,Iris-virginica
 6.7,3.3,5.7,2.1,Iris-virginica
 7.2,3.2,6.0,1.8,Iris-virginica
 6.2,2.8,4.8,1.8,Iris-virginica
 6.1,3.0,4.9,1.8,Iris-virginica
 6.4,2.8,5.6,2.1,Iris-virginica
 7.2,3.0,5.8,1.6,Iris-virginica
 7.4,2.8,6.1,1.9,Iris-virginica
 7.9,3.8,6.4,2.0,Iris-virginica
 6.4,2.8,5.6,2.2,Iris-virginica
 6.3,2.8,5.1,1.5,Iris-virginica
 6.1,2.6,5.6,1.4,Iris-virginica
 7.7,3.0,6.1,2.3,Iris-virginica
 6.3,3.4,5.6,2.4,Iris-virginica
 6.4,3.1,5.5,1.8,Iris-virginica
 6.0,3.0,4.8,1.8,Iris-virginica
 6.9,3.1,5.4,2.1,Iris-virginica
 6.7,3.1,5.6,2.4,Iris-virginica
 6.9,3.1,5.1,2.3,Iris-virginica
 5.8,2.7,5.1,1.9,Iris-virginica
 6.8,3.2,5.9,2.3,Iris-virginica
 6.7,3.3,5.7,2.5,Iris-virginica
 6.7,3.0,5.2,2.3,Iris-virginica
 6.3,2.5,5.0,1.9,Iris-virginica
 6.5,3.0,5.2,2.0,Iris-virginica
 6.2,3.4,5.4,2.3,Iris-virginica
 5.9,3.0,5.1,1.8,Iris-virginica
 </pre>

<button type="button" class="btn" data-toggle="collapse" data-target="#csvExample">Hide again</button>  
</div>
<br>
To read out the file the `fromFileWithCsvSchema`-function can be used. It takes the path, the information about the field separator and the information about wether the first line of the file includes headers or not.
*)

let path = __SOURCE_DIRECTORY__ + "/data/irisData.csv" //....
let hasHeader = true
let separator = ','
let myIrisData = 
    Seq.fromFileWithCsvSchema<irisItem>(path, separator, hasHeader)
    |> Seq.toList

(**
<br>
The result can be seen here:  
<button type="button" class="btn" data-toggle="collapse" data-target="#csv2Example">Show/Hide result</button>
<div id="csv2Example" class="collapse csv2Example ">
<pre>
val data : irisItem list =
  [{SepalLength = 5.1;
    SepalWidth = 3.5;
    PetalLength = 1.4;
    PetalWidth = 0.2;
    Species = "Iris-setosa";}; {SepalLength = 4.9;
                                SepalWidth = 3.0;
                                PetalLength = 1.4;
                                PetalWidth = 0.2;
                                Species = "Iris-setosa";};
   {SepalLength = 4.7;
    SepalWidth = 3.2;
    PetalLength = 1.3;
    PetalWidth = 0.2;
    Species = "Iris-setosa";}; {SepalLength = 4.6;
                                SepalWidth = 3.1;
                                PetalLength = 1.5;
                                PetalWidth = 0.2;
                                Species = "Iris-setosa";};
   {SepalLength = 5.0;
    SepalWidth = 3.6;
    PetalLength = 1.4;
    PetalWidth = 0.2;
    Species = "Iris-setosa";}; {SepalLength = 5.4;
                                SepalWidth = 3.9;
                                PetalLength = 1.7;
                                PetalWidth = 0.4;
                                Species = "Iris-setosa";};
   {SepalLength = 4.6;
    SepalWidth = 3.4;
    PetalLength = 1.4;
    PetalWidth = 0.3;
    Species = "Iris-setosa";}; {SepalLength = 5.0;
                                SepalWidth = 3.4;
                                PetalLength = 1.5;
                                PetalWidth = 0.2;
                                Species = "Iris-setosa";}; ...]
 </pre>

<button type="button" class="btn" data-toggle="collapse" data-target="#csv2Example">Hide again</button>  
</div>
<br>
*)


(**
###Reading lines as collections:
By using a `ConverterAttribute`, values in a line can also be read as a collection. In this example we want to parse the same file as above, but group all values of one line as an array:
*)
//The double array converter defined here is used to map a seq of string to a boxed float array.
type DoubleArrayConverter() = 
    inherit ConverterAttribute()
    override this.convertToObj = 
        Converter.Collection(fun (strs : seq<string>) -> 
            (strs
             |> Seq.map 
                    (fun s -> FSharp.Care.String.tryParseFloatDefault nan s)
             |> Seq.toArray)
            |> box)

//This record type states that the first 4 fields of every line in the file should be converted by the DoubleArrayConverter. It fills the field Features. 
type irisItemWithMulti = 
    {   [<FieldAttribute([| 0; 1; 2; 3 |])>][<DoubleArrayConverter>]
        Features : float []
        [<FieldAttribute(4)>] 
        Species : string }

//If you use numbers to mark your fields of the file, make sure you use `false` for the header parameter.
let hasHeader' = false

let myMultiIrisData = 
    Seq.fromFileWithCsvSchema<irisItemWithMulti>(path, separator, hasHeader')
    |> Seq.skip 1 //We skip the first line, which will only contain nan because of the headers
    |> Seq.toList
(**

<br>
The result can be seen here:  
<button type="button" class="btn" data-toggle="collapse" data-target="#csv3Example">Show/Hide result</button>
<div id="csv3Example" class="collapse csv3Example ">
<pre>
val myMultiIrisData : irisItemWithMulti list =
  [{Features = [|5.1; 3.5; 1.4; 0.2|];
    Species = "Iris-setosa";}; {Features = [|4.9; 3.0; 1.4; 0.2|];
                                Species = "Iris-setosa";};
   {Features = [|4.7; 3.2; 1.3; 0.2|];
    Species = "Iris-setosa";}; {Features = [|4.6; 3.1; 1.5; 0.2|];
                                Species = "Iris-setosa";};
   {Features = [|5.0; 3.6; 1.4; 0.2|];
    Species = "Iris-setosa";}; {Features = [|5.4; 3.9; 1.7; 0.4|];
                                Species = "Iris-setosa";};
   {Features = [|4.6; 3.4; 1.4; 0.3|];
    Species = "Iris-setosa";}; {Features = [|5.0; 3.4; 1.5; 0.2|];
                                Species = "Iris-setosa";};
   {Features = [|4.4; 2.9; 1.4; 0.2|];
    Species = "Iris-setosa";}; {Features = [|4.9; 3.1; 1.5; 0.1|];
                                Species = "Iris-setosa";};
   {Features = [|5.4; 3.7; 1.5; 0.2|];
    Species = "Iris-setosa";}; {Features = [|4.8; 3.4; 1.6; 0.2|];
                                Species = "Iris-setosa";}; ...]
 </pre>

<button type="button" class="btn" data-toggle="collapse" data-target="#csv3Example">Hide again</button>  
</div>
<br>

*)