namespace BioFSharp.Tests

open Expecto
open FsCheck
open GeneratorsCode

module Core =

    open BioFSharp.Formula

    [<Tests>]
    let testFormula =

        testList "BioFSharp.Formula" [

            testCase "test_toString" <| fun () ->
                let res = "H2O"  |> parseFormulaString |> toString
                Expect.isTrue (res = "H2.00 O1.00") "Expected True"

            testCase "test_add" <| fun () ->
                let f1  = "H2"  |> parseFormulaString 
                let f2  = "O"   |> parseFormulaString 
                let res = add f1 f2 |> toString 
                Expect.isTrue (res = "H2.00 O1.00") "Expected True"
            
            testCase "test_substract" <| fun () ->
                let f1  = "H2O"  |> parseFormulaString 
                let f2  = "H4O2" |> parseFormulaString 
                let res = substract f1 f2 |> toString 
                Expect.isTrue (res = "H2.00 O1.00") "Expected True"

            testCase "test_substract_neg" <| fun () ->
                let f1  = "H2O"   |> parseFormulaString 
                let f2  = emptyFormula 
                let res = substract f1 f2 |> toString 
                Expect.isTrue (res = "H-2.00 O-1.00") "Expected True"

            testCase "test_substract_Zero" <| fun () ->
                let f1  = "H2O" |> parseFormulaString 
                let f2  = "H2O" |> parseFormulaString 
                let res = substract f1 f2 |> toString 
                Expect.isTrue (res = "H0.00 O0.00") "Expected True"

        ]

        