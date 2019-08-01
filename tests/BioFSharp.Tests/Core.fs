namespace BioFSharp.Tests

open Expecto
open FsCheck
open GeneratorsCode
open BioFSharp
open System
module Core =

    open BioFSharp.Isotopes
    [<Tests>]
    let testIsotopes =

        testList "BioFSharp.Isotopes" [

            testCase "test_createIsotope" <| fun () ->
                let H1 = { 
                    AtomicSymbol  =  "H" 
                    AtomicNumberZ = 1
                    MassNumber    = 1
                    Mass          = 1.00782503207
                    NatAbundance  = 0.999885
                    RelAtomicMass = 1.007947
                    }
                let H1' = create "H" 1 1 1.00782503207 0.999885 1.007947
                Expect.equal H1 H1' "Record initialization via function differs from initialization via record expression. Check parameter order of 'create'" 
        ]
    
    open BioFSharp.Elements
    [<Tests>]
    let testElements =

        testList "BioFSharp.Elements" [

            testCase "test_createMonoIsotopic" <| fun () ->
                let Na = { 
                    Symbol = "Na"
                    X      = Isotopes.Table.Na23
                    Xcomp  = Isotopes.Table.Na23.NatAbundance
                    Root   = Isotopes.Table.Na23.NatAbundance 
                    }
                let Na' = createMono "Na" (Isotopes.Table.Na23,Isotopes.Table.Na23.NatAbundance) 
                Expect.equal Na Na' "Record initialization via function differs from initialization via record expression. Check parameter order of 'createMono'"

            testCase "test_createDiIsotopic" <| fun () ->
                let H = { 
                    Symbol = "H" 
                    X      = Isotopes.Table.H1
                    Xcomp  = Isotopes.Table.H1.NatAbundance
                    X1     = Isotopes.Table.H2
                    X1comp = Isotopes.Table.H2.NatAbundance
                    Root   = (-1. * Isotopes.Table.H1.NatAbundance / Isotopes.Table.H2.NatAbundance)
                    }
                let H' = createDi "H" (Isotopes.Table.H1,Isotopes.Table.H1.NatAbundance) (Isotopes.Table.H2,Isotopes.Table.H2.NatAbundance) 
                Expect.equal H H' "Record initialization via function differs from initialization via record expression. Check parameter order of 'createDi'" 

            testList "test_createTriIsotopic" [    
                let (rootO,rootO') =
                    System.Numerics.Complex(-0.0926829268292669, 22.0592593273255),
                    System.Numerics.Complex(-0.0926829268292696, -22.0592593273255)
                let O = { 
                    Symbol = "O"
                    X      = Isotopes.Table.O16
                    Xcomp  = Isotopes.Table.O16.NatAbundance
                    X1     = Isotopes.Table.O17
                    X1comp = Isotopes.Table.O17.NatAbundance
                    X2     = Isotopes.Table.O18
                    X2comp = Isotopes.Table.O18.NatAbundance
                    Root   = (rootO,rootO')//
                    }
                let O' = createTri "O" (Isotopes.Table.O16,Isotopes.Table.O16.NatAbundance) (Isotopes.Table.O17,Isotopes.Table.O17.NatAbundance) (Isotopes.Table.O18,Isotopes.Table.O18.NatAbundance)
                yield 
                    testList "test_createTriIsotopic" [
                        testCase "calcRootsTri_firstReal" <| fun () ->
                            Expect.floatClose Accuracy.high (fst O.Root).Real (fst O'.Root).Real ""
                        testCase "calcRootsTri_secondReal" <| fun () ->
                            Expect.floatClose Accuracy.high (snd O.Root).Real (snd O'.Root).Real ""
                        testCase "calcRootsTri_firstImaginary" <| fun () ->
                            Expect.floatClose Accuracy.high (fst O.Root).Imaginary (fst O'.Root).Imaginary ""
                        testCase "calcRootsTri_secondImaginary" <| fun () ->
                            Expect.floatClose Accuracy.high (snd O.Root).Imaginary (snd O'.Root).Imaginary ""
                    ]                                 
                yield 
                    testCase "initialization" <| fun () ->
                        Expect.equal  O O' "Record initialization via function differs from initialization via record expression. Check parameter order of 'createTri'"                       
            ]

            testList "test_TriIsotopicCompare" [    
                let O   = createTri "O" (Isotopes.Table.O16,Isotopes.Table.O16.NatAbundance) (Isotopes.Table.O17,Isotopes.Table.O17.NatAbundance) (Isotopes.Table.O18,Isotopes.Table.O18.NatAbundance)
                let O'  = {O with Symbol = "N"}
                let O'' = {O with Xcomp = nan}
                yield 
                    testCase "negative" <| fun () ->
                        Expect.notEqual O O' "Expected equality, because the member 'CompareTo' checks for equality of the record field Symbol"                           
                yield 
                    testCase "positive" <| fun () ->
                        Expect.equal O O'' "Expected equality, because the member 'CompareTo' checks for equality of the record field Symbol"                             
            ]                                    
        ]
    
    open BioFSharp.Formula
    [<Tests>]
    let testFormula =

        testList "BioFSharp.Formula" [

            testCase "test_toString" <| fun () ->
                let res = "H2O"  |> parseFormulaString |> toString
                Expect.equal res "H2.00 O1.00" "Parsing of formula string failed."

            testCase "test_add" <| fun () ->
                let f1  = "H2"  |> parseFormulaString 
                let f2  = "O"   |> parseFormulaString 
                let res = add f1 f2 |> toString 
                Expect.equal res "H2.00 O1.00" "Addition of formulas failed."
            
            testCase "test_substract" <| fun () ->
                let f1  = "H2O"  |> parseFormulaString 
                let f2  = "H4O2" |> parseFormulaString 
                let res = substract f1 f2 |> toString 
                Expect.equal res "H2.00 O1.00" "Substraction of formulas failed"

            testCase "test_substract_neg" <| fun () ->
                let f1  = "H2O"   |> parseFormulaString 
                let f2  = emptyFormula 
                let res = substract f1 f2 |> toString 
                Expect.equal res  "H-2.00 O-1.00" "Substraction of formulas failed"

            testCase "test_substract_Zero" <| fun () ->
                let f1  = "H2O" |> parseFormulaString 
                let f2  = "H2O" |> parseFormulaString 
                let res = substract f1 f2 |> toString 
                Expect.equal res "H0.00 O0.00" "Substraction of formulas failed"
        ]
    
    open BioFSharp.Mass  
    [<Tests>]
    let testMass =

        testList "BioFSharp.Mass" [

            testCase "test_toMz" <| fun () ->
                let mass = 1000.  
                let res  = toMZ mass 3.
                let exp  = 334.3406098
                Expect.floatClose Accuracy.high res exp "Mass to Mz conversion failed."

            testCase "test_toMass" <| fun () ->
                let mz   = 334.3406098
                let res  = ofMZ mz 3.
                let exp  = 1000.
                Expect.floatClose Accuracy.high res exp "Mz to Mass conversion failed."
                    
            testCase "test_accuracy" <| fun () ->
                let mass            = 1000.003
                let referenceMass   = 1000.001
                let res             = accuracy mass referenceMass 
                let exp             = 1.999998
                Expect.floatClose Accuracy.high res exp "Accuracy calculation failed."

            testCase "test_deltaMassByPpm" <| fun () ->
                let mass            = 1000.
                let ppm             = 100.
                let res             = deltaMassByPpm ppm mass 
                let exp             = 0.1
                Expect.floatClose Accuracy.high res exp "Delta mass calculation failed."

            testList "test_rangePpm" [
                let mass            = 1000.
                let ppm             = 100.
                let min,max         = rangePpm ppm mass  
                let expMin,expMax   = 999.9, 1000.1
                yield 
                    testCase "lower_Border" <| fun () ->
                        Expect.floatClose Accuracy.high min expMin "Delta mass calculation failed, lower border is incorrect."
                yield 
                    testCase "upper_Border" <| fun () ->
                        Expect.floatClose Accuracy.high max expMax "Delta mass calculation failed, upper border is incorrect."                   
            ]        
        ]
        