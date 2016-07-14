namespace BioFSharp

module Formula =

    open System.Text.RegularExpressions
    open FSharp.Care.Collections
    open System.Collections.Generic
                    
    // http://stackoverflow.com/questions/23602175/regex-for-parsing-chemical-formulas 
    let private REGEX_ELEMENT_SYM = new Regex(@"(?<element>[A-Z][a-z]*)(?:[ ({})]*)(?<number>[0-9.]*)", RegexOptions.Compiled);

    
    /// Type abreviation for Map<Elements.Element,float>
    type Formula      = Dictionary<Elements.Element,float>
    
    /// Empty formula
    let emptyFormula : Formula = new Dictionary<Elements.Element,float>(10)

    /// Returns Formula as string
    let toString (f:Formula) =
        seq { for e in f do yield sprintf "%s%f " (Elements.getMainIsotope e.Key).AtomicSymbol e.Value } |> String.concat ""

    /// adds two formula
    let add (f1:Formula) (f2:Formula) =
        Dictionary.merge (f1) (f2) (fun _ v v' -> v + v')
        :?> Formula
    
    /// adds one element to a Formula 
    let addElement e (f:Formula)  =
        Dictionary.addOrUpdateInPlaceBy (+) e 1. f

    /// adds two formula
    //let (+)  (f1:Formula) (f2:Formula) = add f1 f2

    /// substract two formula
    let substract (f1:Formula) (f2:Formula) =
        Dictionary.merge (f1) (f2) (fun _ v  v' -> v - v')
        :?> Formula

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
        match f |> Dictionary.containsKey unlabled with
        | true -> 
            let v = f.[unlabled]
            f
            |> Dictionary.remove unlabled
            |> Dictionary.addInPlace labled v 
            :?> Formula
        | false -> f
        
    /// Lables a given number of elements of a certain kind within a formula
    let lableNumberOfElement (f:Formula) (unlabled:Elements.Element) (labled:Elements.Element) (number:float) =
        match f |> Dictionary.containsKey unlabled with
        | true -> 
            let v = f.[unlabled] - number
            if v < 0. then failwithf "Not enaugh elemets in formula"
            f            
            |> Dictionary.addInPlace unlabled v 
            |> Dictionary.addInPlace labled number 
            :?> Formula
        | false -> f
  
  
//        let result : Formula = 
//            f 
//            |> Seq.map (fun (keyValue) -> if keyValue.Key = unlabled then (keyValue.Key,keyValue.Value - number) else (keyValue.Key,keyValue.Value) )            
//            |> Dictionary.ofSeq
//        result.Add(labled,number)
    
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
            |> Seq.fold (fun acc (key,value) ->   
                Dictionary.addOrUpdateInPlaceBy (+) key value acc :?> Formula
                 ) emptyFormula
        result



                                    

    module Table =

        let CO  = parseFormulaString "CO"
        let CO2 = parseFormulaString "CO2"
        let OH  = parseFormulaString "OH"  //
        let H2O = parseFormulaString "H2O"
        let NH  = parseFormulaString "NH"  //
        let NH2 = parseFormulaString "NH2"
        let NH3 = parseFormulaString "NH3"

