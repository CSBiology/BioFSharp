namespace BioFSharp

open System.Text.RegularExpressions

module Formula =
        
       
    let private REGEX_ELEMENT_SYM = new Regex(@"(?<element>[A-Z][a-z]*)(?:[ ({})]*)(?<number>[0-9.]*)", RegexOptions.Compiled);
    
    /// private function to merge two maps
    /// f is the function how to handel key conflicts
    let private merge (a : Map<'a, 'b>) (b : Map<'a, 'b>) (f : 'a -> 'b * 'b -> 'b) =
        Map.fold (fun s k v ->
            match Map.tryFind k s with
            | Some v' -> Map.add k (f k (v, v')) s
            | None -> Map.add k v s) a b

    
    /// Type abreviation for Map<Elements.Element,float>
    type Formula      = Map<Elements.Element,float>
    
    /// Empty formula
    let emptyFormula : Formula = Map.empty 

    /// Returns Formula as string
    let toString (f:Formula) =
        seq { for e in f do yield sprintf "%s%.2f " (Elements.getMainIsotope e.Key).AtomicSymbol e.Value } |> String.concat ""

    /// adds two formula
    let add (f1:Formula) (f2:Formula) =
        merge (f1) (f2) (fun _ (v, v') -> v + v')
    
    /// adds two formula
    //let (+)  (f1:Formula) (f2:Formula) = add f1 f2

    /// substract two formula
    let substract (f1:Formula) (f2:Formula) =
        merge (f1) (f2) (fun _ (v, v') -> v - v')

    /// substract two formula
    //let (-)  (f1:Formula) (f2:Formula) = substract f1 f2

    /// Returns average mass of sum formula
    let averageMass (f:Formula) =
        f |> Seq.sumBy (fun elem -> (Elements.getMainIsotope elem.Key).RelAtomicMass * elem.Value)

    /// Returns monoisotopic mass of sum formula
    let monoisoMass (f:Formula) =
        f |> Seq.sumBy (fun elem -> (Elements.getMainIsotope elem.Key).Mass * elem.Value)
    
    
    /// Lables all elements of a certain kind within a formula
    let lableElement (f:Formula) (unlabled:Elements.Element) (labled:Elements.Element) =
        let result : Formula = 
            f 
            |> Seq.map (fun (keyValue) -> if keyValue.Key = unlabled then (labled,keyValue.Value) else (keyValue.Key,keyValue.Value) )
            |> Map.ofSeq
        result
    
    /// Lables a given number of elements of a certain kind within a formula
    let lableNumberOfElement (f:Formula) (unlabled:Elements.Element) (labled:Elements.Element) (number:float) =
        let result : Formula = 
            f 
            |> Seq.map (fun (keyValue) -> if keyValue.Key = unlabled then (keyValue.Key,keyValue.Value - number) else (keyValue.Key,keyValue.Value) )            
            |> Map.ofSeq
        result.Add(labled,number)
    
    // TODO: (NO)2 
    /// Parse formula string and returns formula type
    let parseFormulaString (strFormula:string) =
        let matches = REGEX_ELEMENT_SYM.Matches (strFormula)
        // regex results as seq
        let ms = seq {for i = 0 to matches.Count - 1 do yield matches.[i]}
        let msItems = ms |> Seq.map ( fun g -> 
                let elem = Elements.Table.ElementAsObject((g.Groups.["element"].Value))
                let n    = (g.Groups.["number"].Value)
                (elem, (if n <> "" then float(n) else 1.)) )
        let result : Formula =
            msItems 
            |> Seq.fold (fun acc (key,value) -> if acc.ContainsKey(key) then acc.Add(key,(value + acc.[key])) else acc.Add(key,value) ) Map.empty
        result



                                    

    module Table =

        let CO  = parseFormulaString "CO"
        let CO2 = parseFormulaString "CO2"
        let OH  = parseFormulaString "OH"  //
        let H2O = parseFormulaString "H2O"
        let NH  = parseFormulaString "NH"  //
        let NH2 = parseFormulaString "NH2"
        let NH3 = parseFormulaString "NH3"

