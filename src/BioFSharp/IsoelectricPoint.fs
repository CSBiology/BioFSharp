namespace BioFSharp

open BioFSharp
open AminoAcidSymbols
open AminoProperties

///Finding the isoelectric point of peptides
module IsoelectricPoint = 

    ///Finds the value in an interval for which a given function returns a value close to 0 
    let private tryFindRoot func accuracy lowerBound upperBound maxIter = 
        let acc = abs accuracy
        let rec loop a b i = 
            let c = (a + b)/2.
            let fc = func c
            if (abs fc) < acc then 
                Some c
            else
                if i = maxIter then None
                else 
                    if sign fc = sign (func a) then loop c b (i+1)
                    else loop a c (i+1)

        let checkConditions a b = 
            let fa = func a
            let fb = func b
            if (abs fa) < acc then Some a
            elif (abs fb) < acc then Some b
            else 
                if fa < 0. then 
                    if fb > 0. then
                        loop a b 0
                    else None
                else 
                    if fb < 0. then 
                        loop a b 0
                    else None
        if lowerBound < upperBound then checkConditions lowerBound upperBound
        else checkConditions upperBound lowerBound

    ///Maps AminoAcidSymbol to default pK value of it's sidechain. Returns 0.0 if sidechain is neither acidic nor basic
    let getpKr  = initGetAminoProperty AminoProperty.PKr

    ///Finds the pH for which the global charge of the aaSeq is closer to 0 than the given accuracy
    let tryFind (pKrFunc: AminoAcidSymbol -> float) accuracy (aaSeq : AminoAcidSymbol seq) = 
        let en = aaSeq.GetEnumerator()
        let compVec = Array.zeroCreate 26
        let rec loop current =
            match en.MoveNext() with
            | false ->
                current
            | true ->
                let index = (int (BioItem.symbol en.Current)) - 65
                compVec.[index] <- compVec.[index] + 1
                loop en.Current
        match en.MoveNext() with 
        |false -> None
        |true -> 
            compVec.[(int (BioItem.symbol en.Current)) - 65] <- 1            
            let nTerm,cTerm = en.Current, loop en.Current
            
            let f pH = 
                let posChargeState = 
                    let CR = 10. ** (7.4 - pH)
                    CR/(CR+1.)
                    +
                    Seq.fold 
                        (fun chargeState aa -> 
                            let CR = 10. ** ((pKrFunc aa) - pH)
                            let partialCharge = CR/(CR+1.)
                            let count = float compVec.[(int (BioItem.symbol aa)) - 65]                  
                            chargeState + partialCharge * count
                            )
                        0.
                        AminoSymbolSetPosCharged
                let negChargeState =
                    let CR = 10. ** (pH - 3.9)
                    CR/(CR+1.)
                    +
                    Seq.fold 
                        (fun chargeState aa -> 
                            let CR = 10. ** (pH - (pKrFunc aa))
                            let partialCharge = CR/(CR+1.)
                            let count = float compVec.[(int (BioItem.symbol aa)) - 65]                  
                            chargeState + partialCharge * count
                            )
                        0.
                        AminoSymbolSetNegCharged
                posChargeState - negChargeState
            tryFindRoot f accuracy 0. 14. 50
            |> Option.map (fun pH -> pH, f pH)
