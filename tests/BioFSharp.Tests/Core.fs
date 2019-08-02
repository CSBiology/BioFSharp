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
                    Root   = (rootO,rootO')
                    }
                let O' = createTri "O" (Isotopes.Table.O16,Isotopes.Table.O16.NatAbundance) (Isotopes.Table.O17,Isotopes.Table.O17.NatAbundance) (Isotopes.Table.O18,Isotopes.Table.O18.NatAbundance)
                yield 
                    testList "test_calcRootsTri" [
                        testCase "firstReal" <| fun () ->
                            Expect.floatClose Accuracy.high (fst O.Root).Real (fst O'.Root).Real ""
                        testCase "secondReal" <| fun () ->
                            Expect.floatClose Accuracy.high (snd O.Root).Real (snd O'.Root).Real ""
                        testCase "firstImaginary" <| fun () ->
                            Expect.floatClose Accuracy.high (fst O.Root).Imaginary (fst O'.Root).Imaginary ""
                        testCase "secondImaginary" <| fun () ->
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

            testList "test_getMainIsotope" [    
                yield 
                    testCase "Mono" <| fun () ->
                        let iso = getMainIsotope Table.Na
                        let exp = Isotopes.Table.Na23
                        Expect.equal iso exp ""                           
                yield 
                    testCase "Di" <| fun () ->
                        let iso = getMainIsotope Table.H
                        let exp = Isotopes.Table.H1
                        Expect.equal iso exp ""                           
                yield 
                    testCase "Tri" <| fun () ->
                        let iso = getMainIsotope Table.O
                        let exp = Isotopes.Table.O16
                        Expect.equal iso exp ""                           
                yield 
                    testCase "Multi" <| fun () ->                         
                        let iso = getMainIsotope Table.Zn
                        let exp = Isotopes.Table.Zn64
                        Expect.equal iso exp ""                                               
            ]     

            testList "test_getMainXComp" [    
                yield 
                    testCase "Mono" <| fun () ->
                        let abu = getMainXComp Table.Na
                        let exp = Isotopes.Table.Na23.NatAbundance
                        Expect.floatClose Accuracy.high abu exp ""                           
                yield 
                    testCase "Di" <| fun () ->
                        let abu = getMainXComp Table.H
                        let exp = Isotopes.Table.H1.NatAbundance
                        Expect.floatClose Accuracy.high abu exp ""                           
                yield 
                    testCase "Tri" <| fun () ->
                        let abu = getMainXComp Table.O
                        let exp = Isotopes.Table.O16.NatAbundance
                        Expect.floatClose Accuracy.high abu exp ""                           
                yield 
                    testCase "Multi" <| fun () ->                         
                        let abu = getMainXComp Table.Zn
                        let exp = Isotopes.Table.Zn64.NatAbundance
                        Expect.floatClose Accuracy.high abu exp ""                                               
            ]   
            
            testList "test_getSinglePhiL" [    
                yield 
                    testCase "Mono" <| fun () ->
                        let res = getSinglePhiL Table.Na 1000000. 2.
                        let exp = 1000000.0
                        Expect.floatClose Accuracy.high res exp ""                           
                yield 
                    testCase "Di" <| fun () ->
                        let res = getSinglePhiL Table.H 1000000. 2.
                        let exp = 0.01322804227
                        Expect.floatClose Accuracy.high res exp ""                           
                yield 
                    testCase "Tri" <| fun () ->
                        let res = getSinglePhiL Table.O 1000000. 2.
                        let exp = -4109.842165
                        Expect.floatClose Accuracy.high res exp ""                           
                yield 
                    testCase "Multi" <| fun () ->                         
                        let res = getSinglePhiL Table.Zn 1000000. 2.
                        Expect.isTrue (nan.Equals(res)) ""                                              
            ]

            testList "test_getSinglePhiM" [    
                yield 
                    testCase "Mono" <| fun () ->
                        let res = getSinglePhiM Table.Na 1000000. 2.
                        let exp = 1892.042007
                        Expect.floatClose Accuracy.high res exp ""                           
                yield 
                    testCase "Di" <| fun () ->
                        let res = getSinglePhiM Table.H 1000000. 2.
                        let exp = 0.0528309132
                        Expect.floatClose Accuracy.high res exp ""                           
                yield 
                    testCase "Tri" <| fun () ->
                        let res = getSinglePhiM Table.O 1000000. 2.
                        let exp = -4109.842165
                        Expect.floatClose Accuracy.high res exp ""                           
                yield 
                    testCase "Multi" <| fun () ->                         
                        let res = getSinglePhiM Table.Zn 1000000. 2.
                        Expect.isTrue (nan.Equals(res)) ""
            ]

            testCase "test_getAtomicSymbol" <| fun () ->
                let naS'    = "Na"
                let na      =  Mono (createMono naS' (Isotopes.Table.Na23,Isotopes.Table.Na23.NatAbundance) )
                let naS    = getAtomicSymbol na
                Expect.equal naS naS' "Symbols are not equal." 

            testCase "test_ofSymbol" <| fun () ->
                let H   = Elements.Table.ofSymbol "H" 
                let H'  = Elements.Table.H
                Expect.equal H H' "Symbols are not equal." 
                  
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
            
            testList "test_substract" [    
                yield
                    testCase "substract_basic" <| fun () ->
                        let f1  = "H2O"  |> parseFormulaString 
                        let f2  = "H4O2" |> parseFormulaString 
                        let res = substract f1 f2 |> toString 
                        Expect.equal res "H2.00 O1.00" "Substraction of formulas failed"
                yield
                    testCase "substract_neg" <| fun () ->
                        let f1  = "H2O"   |> parseFormulaString 
                        let f2  = emptyFormula 
                        let res = substract f1 f2 |> toString 
                        Expect.equal res  "H-2.00 O-1.00" "Substraction of formulas failed"
                yield 
                    testCase "substract_Zero" <| fun () ->
                        let f1  = "H2O" |> parseFormulaString 
                        let f2  = "H2O" |> parseFormulaString 
                        let res = substract f1 f2 |> toString 
                        Expect.equal res "H0.00 O0.00" "Substraction of formulas failed"
            ]

            testCase "test_averageMass" <| fun () ->
                let res = "H2O" |> parseFormulaString |> Formula.averageMass 
                let exp = 18.015294
                Expect.floatClose Accuracy.high res exp "Substraction of formulas failed"

            testCase "test_monoisoMass" <| fun () ->
                let res = "H2O" |> parseFormulaString |> Formula.monoisoMass 
                let exp = 18.01056468
                Expect.floatClose Accuracy.high res exp "Substraction of formulas failed"            
            
            testList "test_replaceElement" [    
                let f = "N10" |> parseFormulaString 
                let labeledf  = Formula.replaceElement f Elements.Table.N Elements.Table.Heavy.N15 
                yield
                    testCase "isReplaced" <| fun () ->
                        let res = Map.containsKey Elements.Table.N labeledf
                        Expect.isFalse res "Element was not removed."     
                yield
                    testCase "newElementInserted" <| fun () ->
                        let res = Map.containsKey Elements.Table.Heavy.N15 labeledf
                        Expect.isTrue res "Element was not replaced."
                yield 
                    testCase "NumberIsNotChanged" <| fun () ->
                        let res  = Map.find Elements.Table.N f
                        let res' = Map.find Elements.Table.Heavy.N15 labeledf
                        
                        Expect.floatClose Accuracy.high res res' "Element number has changed."
                ]

            testList "test_replaceNumberOfElement" [    
                let f = "N10" |> parseFormulaString 
                let labeledf  = Formula.replaceNumberOfElement f Elements.Table.N Elements.Table.Heavy.N15 4.
                yield
                    testCase "isNotReplaced" <| fun () ->
                        let res = Map.containsKey Elements.Table.N labeledf
                        Expect.isTrue res "Element was removed."        
                yield
                    testCase "newElementInserted" <| fun () ->
                        let res = Map.containsKey Elements.Table.Heavy.N15 labeledf
                        Expect.isTrue res "Element was not replaced."
                yield 
                    testList "stoichiomentry" [
                        testCase "old" <| fun () ->
                            let res = Map.find Elements.Table.N labeledf
                            let exp = 6. 
                            Expect.floatClose Accuracy.high res exp "Element number not correct."
                        testCase "new" <| fun () ->
                            let res = Map.find Elements.Table.Heavy.N15 labeledf
                            let exp = 4. 
                            Expect.floatClose Accuracy.high res exp "Element number not correct."
                    ]
                ]

            testList "test_contains" [
                let f = "N10" |> parseFormulaString  
                yield 
                    testCase "positive" <| fun () ->
                        let res = contains Elements.Table.N f 
                        Expect.isTrue res ""
                yield 
                    testCase "negative" <| fun () ->
                        let res = contains Elements.Table.H f 
                        Expect.isFalse res ""
            ]

            testList "test_count" [
                let f = "N10" |> parseFormulaString  
                yield 
                    testCase "positive" <| fun () ->
                        let res = count Elements.Table.N f
                        let exp = 10. 
                        Expect.floatClose Accuracy.high (res) (exp) "Element number not correct."
                yield 
                    testCase "negative" <| fun () ->
                        let res = count Elements.Table.H f
                        let exp = 0.
                        Expect.floatClose Accuracy.high (res) (exp) "Element number not correct."
            ]
                
            testList "test_countBySym" [
                let f = "N10" |> parseFormulaString  
                yield 
                    testCase "positive" <| fun () ->
                        let res = countBySym "N" f 
                        let exp = 10. 
                        Expect.floatClose Accuracy.high (res) (exp) "Element number not correct."
                yield 
                    testCase "negative" <| fun () ->
                        let res = countBySym "H" f
                        let exp = 0.
                        Expect.floatClose Accuracy.high (res) (exp) "Element number not correct."
            ]
    
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
        