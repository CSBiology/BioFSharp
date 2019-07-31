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
    
    open BioFSharp.Mass
    
    [<Tests>]
    let testMass =

        testList "BioFSharp.Mass" [

            testCase "test_toMz" <| fun () ->
                let mass = 1000.  
                let res  = toMZ mass 3.
                let exp  = 334.3406098
                Expect.floatClose Accuracy.high res exp (sprintf "Expected %f got %f" exp res)

            testCase "test_toMass" <| fun () ->
                let mz   = 334.3406098
                let res  = ofMZ mz 3.
                let exp  = 1000.
                Expect.floatClose Accuracy.high res exp (sprintf "Expected %f got %f" exp res)
                    
            testCase "test_accuracy" <| fun () ->
                let mass            = 1000.003
                let referenceMass   = 1000.001
                let res             = accuracy mass referenceMass 
                let exp             = 1.999998
                Expect.floatClose Accuracy.high res exp (sprintf "Expected %f got %f" exp res)

            testCase "test_deltaMassByPpm" <| fun () ->
                let mass            = 1000.
                let ppm             = 100.
                let res             = deltaMassByPpm ppm mass 
                let exp             = 0.1
                Expect.floatClose Accuracy.high res exp (sprintf "Expected %f got %f" exp res)

            testCase "test_rangePpm" <| fun () ->
                let mass            = 1000.
                let ppm             = 100.
                let min,max         = rangePpm ppm mass  
                let expMin,expMax   = 999.9, 1000.1
                Expect.floatClose Accuracy.high min expMin (sprintf "Expected %f got %f" expMin min)
                Expect.floatClose Accuracy.high max expMax (sprintf "Expected %f got %f" expMax max)

        ]
        