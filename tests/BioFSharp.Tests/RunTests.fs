namespace BioFSharp.Tests

open Expecto

module RunTests =

    [<EntryPoint>]
    let main args =

        Tests.runTestsWithArgs defaultConfig args Tests.testSimpleTests |> ignore
        //BioFSharp core tests
        Tests.runTestsWithArgs defaultConfig args Core.testIsotopes         |> ignore
        Tests.runTestsWithArgs defaultConfig args Core.testElements         |> ignore
        Tests.runTestsWithArgs defaultConfig args Core.testFormula          |> ignore
        Tests.runTestsWithArgs defaultConfig args Core.testMass             |> ignore
        Tests.runTestsWithArgs defaultConfig args Core.testIBioItem         |> ignore
        Tests.runTestsWithArgs defaultConfig args Core.testTaggedSequence   |> ignore
        Tests.runTestsWithArgs defaultConfig args BioArray.parsingTests     |> ignore
        Tests.runTestsWithArgs defaultConfig args Core.testIsotopicDistribution     |> ignore
        
        0

