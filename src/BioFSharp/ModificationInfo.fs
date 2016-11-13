namespace BioFSharp

//open Microsoft.FSharp.Reflection 
//open System.Reflection 
//open System.Runtime.Serialization 

module ModificationInfo =
    
    open FSharp.Care.Collections
        
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

                        interface IBioItem with
                            member this.Name = this.Name
                            member this.Symbol = '#'             
                            member this.isTerminator = false
                            member this.isGap        = false
                            
                            member this.Formula  = this.Modify Formula.emptyFormula

    let createModification name location modifier =
        { Name = name; Location = location; Modify = modifier}           

    let createModificationWithAdd name location formula =
        createModification name location (Formula.add (Formula.parseFormulaString formula))            

    let createModificationWithSubstract name location formula =
        createModification name location (Formula.substract (Formula.parseFormulaString formula))    

    /// Returns modification name as string
    let toString (md:Modification) =
        md.Name

                                                    
    /// Returns then display name of a modification
    let name (md:Modification) =
        BioItem.name md
    
    /// Returns then symbol of a modification
    let symbol (md:Modification) =
        BioItem.symbol md


    /// Returns then byteCode of a modification
    let formula (md:Modification) =
        BioItem.formula md

    /// Returns true if the modification represents a sequence terminator
    let isTerminator (md:Modification) =
        BioItem.isTerminator md

    /// Returns true if the modification represents a sequence gap
    let isGap (md:Modification) =
        BioItem.isGap md

 
    module Table = 
        
        let N15    = createModification "#N15" ModLocation.Isotopic (fun f -> Formula.lableElement f Elements.Table.N Elements.Table.Heavy.N15)
        let N15NH3 = createModification "#N15 NH3" ModLocation.Isotopic (fun f -> Formula.lableElement f Elements.Table.N Elements.Table.Heavy.N15)
        let NH3    = createModificationWithAdd "NH3" ModLocation.Nterm "NH3"
        let H2O    = createModificationWithAdd "H2O" ModLocation.Nterm "H2O"
        