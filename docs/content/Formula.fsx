(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Formula
=======

work in progress... 

*)
#r "BioFSharp.dll"
open BioFSharp


let CO2 = Formula.parseFormulaString "CO2"
Formula.toString CO2

let c = Formula.add CO2 CO2
Formula.toString c

