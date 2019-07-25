namespace BioFSharp.Tests

open Expecto

module RunTests =

    [<EntryPoint>]
    let main args =

        Tests.runTestsWithArgs defaultConfig args Tests.testSimpleTests |> ignore

        //BioFSharp core tests
        //Run BioArrayTests
        Tests.runTestsWithArgs defaultConfig args BioArray.parsingTests  |> ignore
        0

