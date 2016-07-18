namespace BioFSharp

module BioList =
    
    open FSharp.Care
    open IBioSequence
    
    type BioList<[<EqualityConditionalOn; ComparisonConditionalOn >]'a when 'a :> IBioItem> = list<'a>

    /// Returns monoisotopic mass of the given sequence including water (+H20)
    let toMonoisotopicMassWithWater<'a when 'a :> IBioItem> : (BioList<_> -> float) =
        let water = Formula.Table.H2O |> Formula.averageMass
        let memMonoisoMass =
            Memoization.memoizeP (BioItem.formula >> Formula.monoisoMass)
        (fun bs -> 
            bs 
            |> List.sumBy memMonoisoMass
            |> (+) water )


    /// Returns average mass of the given sequence including water (+H20)
    let toAverageMassWithWater<'a when 'a :> IBioItem> : (BioList<_> -> float) =
        let water = Formula.Table.H2O |> Formula.averageMass
        let memAverageMass =
            Memoization.memoizeP (BioItem.formula >> Formula.averageMass)
        (fun bs -> 
            bs 
            |> List.sumBy memAverageMass
            |> (+) water )