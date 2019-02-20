namespace BioFSharp.ML

module Say =

    open CNTK
    let hello name =
        printfn "Hello %s" name
