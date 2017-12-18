namespace BioFSharp

///Contains functionality for working with isotopic abundancies of molecules
module IsotopicDistribution =

    open BioFSharp.Formula

    // Implementation according to BRAIN - algortihm
    /// Generates the distribution of the isotopic abundancy
    let ofFormula (limit:int) (fml:Formula) =
        let calcP0 (fml:Formula) = 
            fml 
            |> Map.fold 
                (fun s k v -> 
                    let stdIso = Elements.getMainIsotope k
                    s * stdIso.NatAbundance**float(v) ) 1.       
        
        // Calculate coefficient ql (Equ. 12 + 7)
        let calcPhiL (fml:Formula) (l:int) = 
            let lf = float l 
            fml
            |> Map.fold 
                (fun s k v -> 
                    s + (Elements.getSinglePhiL k (float v) lf ) ) 0.


        let rec calcPJ (ps:List<float>) (phis:List<float>) (state:float)=
            match ps,phis with
            | [],[] -> state
            | pj::ps,phi::phis ->  calcPJ ps phis (state + pj * phi)
            | _ ,[] -> raise ( System.Exception("ps can't be longer then phis see function w") )
            | [],_  -> state
            

        let rec w (ps:List<float>) (phis:List<float>) (i:int) =
            if ps.Length < phis.Length then
                let np = calcPJ ps phis 0.
                w ((np / - (float i))::ps) phis (i + 1)
            else
                ps |> List.rev // TODO : Continuation passing

        let allPhis = [ for i=1 to limit do yield (calcPhiL fml i) ]
        let p0 = calcP0 fml
        let ps = w [p0] allPhis 1
        ps   



