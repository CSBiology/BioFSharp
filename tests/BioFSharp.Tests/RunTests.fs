namespace BioFSharp.Tests

open Expecto

module RunTests =

    [<EntryPoint>]
    let main args =

        Tests.runTestsWithArgs defaultConfig args Tests.testSimpleTests |> ignore
        //BioFSharp core tests
        Tests.runTestsWithArgs defaultConfig args Core.testIsotopes |> ignore
        Tests.runTestsWithArgs defaultConfig args Core.testElements |> ignore
        Tests.runTestsWithArgs defaultConfig args Core.testFormula  |> ignore
        Tests.runTestsWithArgs defaultConfig args Core.testMass     |> ignore
        //Run BioArrayTests
        Tests.runTestsWithArgs defaultConfig args BioArray.parsingTests  |> ignore
        0

