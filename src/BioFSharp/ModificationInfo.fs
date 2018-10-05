namespace BioFSharp

//open Microsoft.FSharp.Reflection 
//open System.Reflection 
//open System.Runtime.Serialization 

/// Functionality for creating formula modifications
module ModificationInfo =
    
    
    /// Specifier for location of modification
    type ModLocation = | Residual = 0 | Cterm = 1 | Nterm = 2 | ProteinCterm = 3 | ProteinNterm = 4 | Isotopic = 5 

    /// Modification consisting of name, location specifier and a formula modifier
    [<CustomEquality; CustomComparison>]
    type Modification = {
         Name     : string
         IsBiological: bool
         Location : ModLocation
         // Labeled Atom
         Modify   : Formula.Formula -> Formula.Formula 
                        }
                        ///Returns true if both modifications have the same name, else returns false
                        override x.Equals(yobj) =
                            match yobj with
                            | :? Modification as y -> (x.Name = y.Name)
                            | _ -> false
                        ///Returns hash code of name of modification
                        override x.GetHashCode() = hash x.Name
                        
                        interface System.IComparable with
                            member x.CompareTo yobj =
                                match yobj with
                                | :? Modification as y -> compare x.Name y.Name
                                | _ -> invalidArg "yobj" "cannot compare values of different types"

                        interface IBioItem with
                            ///Returns name of modification
                            member this.Name = this.Name
                            ///Returns '#'
                            member this.Symbol = '#'         
                            ///Returns false 
                            member this.isTerminator = false
                            ///Returns false 
                            member this.isGap        = false
                            ///Returns formula of modification
                            member this.Formula  = this.Modify Formula.emptyFormula

    /// Create modification, where molecule will be modified by application of given modifier
    let createModification name isBiological location modifier =
        { Name = name; IsBiological = isBiological; Location = location; Modify = modifier}           

    /// Create modification, where elements of given formula will be added to molecule
    let createModificationWithAdd name isBiological location formula =
        createModification name isBiological location (Formula.add (Formula.parseFormulaString formula))            

    /// Create modification, where elements of given formula will be substracted molecule
    let createModificationWithSubstract name isBiological location formula =
        createModification name isBiological location (Formula.substract (Formula.parseFormulaString formula))    

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

    ///Contains frequent modifications
    module Table = 
        
        let N15    = createModification "#N15" true ModLocation.Isotopic (fun f -> Formula.lableElement f Elements.Table.N Elements.Table.Heavy.N15)
        let N15'   = createModification "#N15" true ModLocation.Isotopic (fun f -> Formula.lableElement f Elements.Table.N Elements.Table.Heavy.N15)
        let N15NH3 = createModification "#N15 NH3" true ModLocation.Isotopic (fun f -> Formula.lableElement f Elements.Table.N Elements.Table.Heavy.N15)
        let NH3    = createModificationWithAdd "NH3" true ModLocation.Nterm "NH3"
        let H2O    = createModificationWithAdd "H2O" true ModLocation.Nterm "H2O"
        