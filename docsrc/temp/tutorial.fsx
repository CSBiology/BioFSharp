(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "../../packages/build/FSharp.Plotly/lib/net45/Fsharp.Plotly.dll"
open FSharp.Plotly
#I "../../bin"

(**
Introducing your project
========================

Say more

*)
#r "BioFSharp.dll"
open BioFSharp

(**
Some more info
*)


(*** define-output:pie1 ***)
Chart.Point([1;2;3],[1;2;3],Name="scattern")
(*** include-it:pie1 ***)
|> Chart.Show
