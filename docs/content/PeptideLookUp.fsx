(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
//#I "../../bin"
//TODO: Add FILES and LINKS to other tutorials
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.Mz.dll"
(**
PeptideLookUp
=============

*)


(**
This part of the documentation aims to give you a brief overview of the functionality of SearchDB.fs. This Module contains functions  
to initialize a user configured peptide database and to subsequently perform common database operations such as inserts or lookUps.


Creating a peptide database
---------------------------

Prior to database creation the user has to specify the database parameters. The following examples will show this process in detail. 
Some complex parameters will be discussed more specifically.

The concept of peptide modifications
------------------------------------
In the library the term "Modification" is understood as literally any change to an amino acid formula. These changes can vary 
from the exchange of an isotope (e.g. a N14 gets replaced with a N15) to an addition/deletion of chemical groups e.g. a phosphorylation.

Differentiation of fixed, variable and isotopic modifications
-----------------------------------------------------------
Creating a peptide database the user not only has to specify the modification itself but also its persistance. This is due to the fact 
that the abundance of a modification can vary from one sample to another. This thought lead to a distinction between fixed, variable and 
isotopic modifications during the construction of this library. 

In a natural sample it is very unlikely for a modification like a phosphorylation to be present at every matching amino acid of every
peptide. The conclusion of this finding is that a database has to contain both, a modified and a unmodified version of the peptide simulating a 
**variable** occurency of the modification coining the term **"VariableModification"**.   

Contrary, further manipulations to the sample e.g. a selection of only N-terminally phosphorylated peptides can result in a sample
for which it can be assumed that roughly every peptide contains this modification. This would justify a creation of a peptide database with a 
**fixed** N-terminal phosphorylation at every matching amino acid of every peptide. Hereby coining the term **"FixedModification"**.

-----------------------------------------------------------
This expression constructs a new instance of the type SearchModification, which is needed to define Fixed- and VariableModifications
This type is inferred of the XMod ontology and the information stored in this type is needed to build instances of the type Modification.     
*)

open BioFSharp
open BioFSharp.Mz
open ModificationInfo
open AminoAcids
open SearchDB
/// Returns a instance of the type SearchModification
let phosphorylation = {
    // Name of this SearchModification 
    Name="Phosphorylation" 
    // Xmod accession Number
    Accession="21" 
    // A brief description of the SearchModification
    Description="Addition of a phosphate group to the AA residue"
    // A string representation of the formula composition 
    Composition= "HO3P"
    // Specifices the ModLocation
    Site= [Specific(Ser,ModLocation.Residual); Specific(Thr,ModLocation.Residual); Specific(Tyr,ModLocation.Residual)]
    // Specifies if the Modification is additive or subtractive
    MType=SearchModType.Plus
    // If a modified AminoAcid is shown via string representation this code is placed before
    // the AminoAcid Symbol e.g. "phS"
    XModCode= "ph"
    } 

(**

As mentioned before, there is a need to model modifications that affect the peptide sequences on a **isotopic** level. This could be understood 
as a special kind of FixedModification with the additional constrain that there is no addition or deletion of a chemical group but a isotopic 
modification affecting every amino acid. e.g. a exchange of every N14 to a N15. As one could expect, these features coined the term "IsotopicModification".   

This expression returns a instance of the type SearchInfoIsotopic, which is needed to define IsotopicModifications. This type is needed if the user 
plans to introduce a IsotopicModifications e.g a replacement of the N14 isotopes with heavier N15 isotopes.
*)

/// Returns a instance of the type SearchInfoIsotopic 
let isoMod = (SearchDB.createSearchInfoIsotopic "N15" Elements.Table.N Elements.Table.Heavy.N15)

(**
Selecting and initializing a mass function
------------------------------------------
The following expression returns a instance of a massfunction. The user can choose between functions that either return 
the monoisotopic or the average mass of types that inherit the IBioItem interface (e.g. AminoAcids).  
Moreover, each function exists in a "WhitMemP" version that uses a dictionary to "remember" (store) already computed masses. 
This approach is especially useful when the user aims to do many mass computations subsequently, as it is the case when
building a new database. The usage of a dictionary is the reason for this function to be initialized prior to database
creation.      
*)

///Returns a instance of the massfunction BioItem.initMonoisoMassWithMemP
let massF : (IBioItem -> float) =  BioItem.initMonoisoMassWithMemP

(**
Incorporation of all parameters in a single record type
-------------------------------------------------------
This type contains all formerly designed Parameters and additional parameters. 
*)

/// Returns a instance of the type SearchDbParams
let paramTestN15 = {
        Name="Creinhardtii_CpN15"
        // Path of db storage folder
        DbFolder            = (__SOURCE_DIRECTORY__ + "/data/")
        // Path of the user selected fasta file  
        FastaPath           = (__SOURCE_DIRECTORY__ + "/data/Chlamy_Cp.fastA")
        // Function that specifies the conversion of the Fasta Header to the protein name inserted in the database
        FastaHeaderToName   = id
        // Protease that is used during in silico digestion of the proteins
        Protease            = Digestion.Table.getProteaseBy "Trypsin"
        // Minimum number of missed cleavages
        MinMissedCleavages  = 1
        // Maximum number of missed cleavages
        MaxMissedCleavages  = 3
        // Maximum peptide mass inserted in the database
        MaxMass             = 15000.
        // Mininmum peptide length 
        MinPepLength        = 4
        // Maximum pepide length
        MaxPepLength        = 65
        // Contains a instance of SearchInfoIsotopic, set to None if no isotopic modification shall be introduced
        IsotopicMod           = [isoMod] 
        // MassMode used during db creation
        MassMode            = SearchDB.MassMode.Monoisotopic
        // Massfunction used during db creation
        MassFunction        = massF
        // List of "Fixedmodifications". 
        FixedMods           = [phosphorylation]
        // List of "VariableModificaions".  
        VariableMods        = []
        // Maximum number of variable modifications per peptide this constrain is used to limit the computational 
        VarModThreshold     = 5
        }

(**

The following example illustrates how a database lookUp can be initialized. The function SearchDB.getPeptideLookUpBy 
takes the defined SearchDbParams and returns a function that takes two float values (representing masses) as input 
parameters and returns a list of peptides each with a mass in the range of first to the second mass used as input 
parameters. 

SearchDB.getPeptideLookUpBy first checks if a database with the given parameters already exists before it starts the 
database creation process. If there is a database matching the user given SearchParameters the process will be finished 
nearly immediatly. If a new database has to be created the computation time will depend on the complexity of the given 
parameters such as the size of the FASTA file, the number of given Modifications, the maxPepLength et cetera. 
   
*)
/// Returns a function that takes two masses as input parameters and returns a list of peptides (wrapped in the type LookUpResult)
let n15DBLookUpBy =  
        SearchDB.getPeptideLookUpBy paramTestN15   
         
(**
This example shows how the pattern used before can be modified slightly to return a function that only needs one parameter as 
input to return a LookUpResult list. 
*)

/// Returns a function that takes one inputMass and returns a list of peptides (wrapped in the type LookUpResult) in the range 
/// of the inputMass-0.5 to inputMass+0.5 
let n15LookUpPeptideBy inputMass =
        let lowerBorder = inputMass-0.5
        let upperBorder = inputMass+0.5 
        n15DBLookUpBy lowerBorder upperBorder

(**
This function can be used as shown in the code snippet below. As a inputMass the monoisotopic mass of the peptide *ANLGMEVMHER* can be used as a input. 
This peptide is encoded by a protein which can be found in the FASTA used for the database creation. Surprisingly the list you obtain will be empty because 
the database does not contain any peptides in the given massRange.   
*)

/// PeptideMass of the peptide known from the peptide *ANLGMEVMHER*. 
let peptideMass = 1285.591258

/// Returns the result of the peptide lookup 
let n15LookUpResult = n15LookUpPeptideBy peptideMass

(**
The possibility that the database is empty can be ruled out by performing a lookUp with the mass 1000., which should return a list filled with 
items of the type LookUpResult. The explanation to this behaviour can be found examining the SearchDbParameters bound to the name **paramTestN15**.
When looking at the parameters we see that we introduced a IsotopicModification. If the peptide was optained from a sample without this isotopic 
modification, a second lookUp in a database lacking IsotopicModification might return a result list containing the peptide of interest.

As a first step a new instance of the type SearchDbParams has to be instantiated and a database has to be created:
*)

/// Returns a instance of the type SearchDbParams
let paramTestN14 = {
        Name="Creinhardtii_CpN14UnMod"
        DbFolder            = (__SOURCE_DIRECTORY__ + "/data/")
        FastaPath           = (__SOURCE_DIRECTORY__ + "/data/Chlamy_Cp.fastA")
        FastaHeaderToName   = id
        Protease            = Digestion.Table.getProteaseBy "Trypsin"
        MinMissedCleavages  = 1
        MaxMissedCleavages  = 3
        MaxMass             = 15000.
        MinPepLength        = 4
        MaxPepLength        = 65
        IsotopicMod           = []
        MassMode            = SearchDB.MassMode.Monoisotopic
        MassFunction        = massF
        FixedMods           = [] 
        VariableMods        = []
        VarModThreshold     = 5
        }
/// Returns a function that takes two masses as input parameters and returns a list of peptides (wrapped in the type LookUpResult)
let n14DBLookUpBy =  
        SearchDB.getPeptideLookUpBy paramTestN14   
/// Returns a function that takes one inputMass and returns a list of peptides (wrapped in the type LookUpResult) in the range 
/// of the inputMass-0.5 to inputMass+0.5 
let n14LookUpPeptideBy inputMass =
        let lowerBorder = inputMass-0.5
        let upperBorder = inputMass+0.5 
        n14DBLookUpBy lowerBorder upperBorder

(**
After these steps the following lookup will result in list of lookupResults containing the peptide "ANLGMEVMHER".
*)

/// Returns the result of the peptide lookup
let n14LookUpResult = n14LookUpPeptideBy peptideMass

(*** hide ***)
n14LookUpResult
