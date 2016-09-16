namespace BioFSharp.Mz

type PeakList<[<EqualityConditionalOn; ComparisonConditionalOn >]'a when 'a :> IPeak> = list<'a>

module PeakList =
    
    let map f pkl : PeakList<_> = 
        List.map f pkl

    let zipMzInt (mz:list<float>) (intensity:list<float>) : PeakList<_> = 
        List.map2 (fun m i -> Peak(m,i)) mz intensity

    let unzipMzInt (pkl : PeakList<_>) = 
        let rec loop (l:PeakList<_>) mz intens =
            match l with
            | h::t -> loop t (h.Mz::mz) (h.Intensity::intens)
            | _ -> List.rev mz ,List.rev intens           // TODO: Continuation passing
        loop pkl [] []
