module BioFSharp.Tests.NetCore

open Expecto

[<EntryPoint>]
let main argv =

    //BioFSharp core tests
    Tests.runTestsWithCLIArgs [] argv BioItemTests.testIsotopes         |> ignore
    Tests.runTestsWithCLIArgs [] argv BioItemTests.testElements         |> ignore
    Tests.runTestsWithCLIArgs [] argv BioItemTests.testFormula          |> ignore
    Tests.runTestsWithCLIArgs [] argv BioItemTests.testMass             |> ignore
    Tests.runTestsWithCLIArgs [] argv BioItemTests.testIBioItem         |> ignore
    Tests.runTestsWithCLIArgs [] argv BioItemTests.testTaggedSequence   |> ignore
    Tests.runTestsWithCLIArgs [] argv BioItemTests.testIsotopicDistribution     |> ignore

    Tests.runTestsWithCLIArgs [] argv BioCollectionsTests.bioCollectionsTests |> ignore

    Tests.runTestsWithCLIArgs [] argv DigestionTests.digestionTests |> ignore
    
    Tests.runTestsWithCLIArgs [] argv AminoAcidTests.aminoAcidTests |> ignore

    Tests.runTestsWithCLIArgs [] argv NucleotideTests.nucleotideTests |> ignore

    Tests.runTestsWithCLIArgs [] argv PhylTreeTests.phylTreeTests |> ignore

    0