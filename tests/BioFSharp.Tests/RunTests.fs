namespace BioFSharp.Tests

open Expecto

module RunTests =

    [<EntryPoint>]
    let main args =

        Tests.runTestsWithArgs defaultConfig args Tests.testSimpleTests |> ignore
        Tests.runTestsWithArgs defaultConfig args BioArray.tests  |> ignore
        0

