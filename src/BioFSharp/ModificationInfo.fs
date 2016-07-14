namespace BioFSharp

open Microsoft.FSharp.Reflection 
open System.Reflection 
open System.Runtime.Serialization 

module ModificationInfo =
    
    type ModLocation = | Residual = 0 | Cterm = 1 | Nterm = 2 | ProteinCterm = 3 | ProteinNterm = 4 | Isotopic = 5 

    [<CustomEquality; CustomComparison>]
    type Modification = {
         Name     : string
         Location : ModLocation
         // Labeled Atom
         Modify   : Formula.Formula -> Formula.Formula 
                        }
                        
                        override x.Equals(yobj) =
                            match yobj with
                            | :? Modification as y -> (x.Name = y.Name)
                            | _ -> false
 
                        override x.GetHashCode() = hash x.Name
                        
                        interface System.IComparable with
                            member x.CompareTo yobj =
                                match yobj with
                                | :? Modification as y -> compare x.Name y.Name
                                | _ -> invalidArg "yobj" "cannot compare values of different types"


    let createModification name location modifier =
        { Name = name; Location = location; Modify = modifier}            


    let createModificationWithAdd name location formula =
        createModification name location (Formula.add formula)            

    let createModificationWithSubstract name location formula =
        createModification name location (Formula.substract formula)  

    /// Returns modification name as string
    let toString (md:Modification) =
        md.Name

                                                    
