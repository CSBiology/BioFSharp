namespace BioFSharp.Tests

open Expecto

module RunTests =

    [<EntryPoint>]
    let main args =

        Tests.runTestsWithArgs defaultConfig args Tests.testSimpleTests |> ignore
        //BioFSharp core tests
        Tests.runTestsWithArgs defaultConfig args Core.testFormula |> ignore
        //Run BioArrayTests
        Tests.runTestsWithArgs defaultConfig args BioArray.parsingTests  |> ignore
        0

