namespace BioFSharp.Mz


open BioFSharp
open BioFSharp.IO

open System
open FSharp.Care
open FSharp.Care.Collections
open FSharp.Care.Monads
open AminoAcids 
open ModificationInfo
//open BioSequences

module SearchDB =

//    let private unionCaseToString (x:'a) = 
//        match  Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x, typeof<'a>) with
//        | case, _ -> case.Name  

    type SearchModType =
        | Minus = 0
        | Plus  = 1

    type SearchModSite =
        | Any      of ModificationInfo.ModLocation
        | Specific of AminoAcids.AminoAcid * ModificationInfo.ModLocation 
    
    type SearchModification = {
        Name        : string
        Accession   : string
        Description : string
        Composition : Formula.Formula
        Site        : SearchModSite list 
        MType       : SearchModType
        XModCode    : string
        }

    type MassMode = 
        | Average
        | Monoisotopic
        override this.ToString() = 
            match this with
            | Average -> "Average"
            | Monoisotopic -> "Monoisotopic"

    type SearchDbParams = {
        // name of database i.e. Creinhardtii_236_protein_full_labeled
        Name            : string
        // path of db storage folder
        DbFolder        : string
        FastaPath       : string
        Protease        : Digestion.Protease
        MissedCleavages : int
        MaxMass         : float
        // valid symbol name of isotopic label in label table i.e. #N15
        IsotopicLabel   : string
        MassMode        : MassMode

        FixedMods       : SearchModification list            
        VariableMods    : SearchModification list
        }

    let createSearchDbParams name dbPath fastapath protease missedCleavages maxmass isotopicLabel massMode fixedMods variableMods = {
         Name=name; 
         DbFolder=dbPath; 
         FastaPath=fastapath; 
         Protease=protease; 
         MissedCleavages=missedCleavages; 
         MaxMass=maxmass; 
         IsotopicLabel=isotopicLabel; 
         MassMode=massMode; 
         FixedMods=List.sort fixedMods; 
         VariableMods=List.sort variableMods
         }

    ///needed as input if element of SearchModSite is of UnionCase | Any
    let listOfAA = [
        AminoAcid.Ala; 
        AminoAcid.Cys; 
        AminoAcid.Asp; 
        AminoAcid.Glu; 
        AminoAcid.Phe; 
        AminoAcid.Gly; 
        AminoAcid.His; 
        AminoAcid.Ile; 
        AminoAcid.Lys; 
        AminoAcid.Leu; 
        AminoAcid.Met; 
        AminoAcid.Asn; 
        AminoAcid.Pyl; 
        AminoAcid.Pro; 
        AminoAcid.Gln; 
        AminoAcid.Arg; 
        AminoAcid.Ser; 
        AminoAcid.Thr; 
        AminoAcid.Sel; 
        AminoAcid.Val; 
        AminoAcid.Trp; 
        AminoAcid.Tyr
        ]


    ///Generates list of modified AminoAcids based on a list of Searchmodifications
    let createAAModTLOfSearchModL (aal: AminoAcid list) (searchModL: SearchModification list) =
        ///Matches elements of the SearchModification.Site with the SearchModSites Any or Specific; Returns tuple of AminoAcids and Modifications
        let createAAModTLOfSearchMod (aal: AminoAcid list) (searchMod: SearchModification)=
            ///Creates AminoAcid Modification tuple. Concerns MType of the Searchmodification  
            let createAAModT (searchMod : SearchModification) modLoc (aa:AminoAcid) =
                match searchMod.MType with
                | SearchModType.Plus -> (aa, createModificationWithAdd searchMod.Name modLoc searchMod.Composition)
                | SearchModType.Minus -> (aa, createModificationWithSubstract searchMod.Name modLoc searchMod.Composition)
            searchMod.Site 
            |> List.map (fun elemOfSiteL  ->  
                            match elemOfSiteL with
                            |Any(modLoc) -> aal 
                                            |> List.map (createAAModT searchMod modLoc)
                                                                                                                                       
                            |Specific(aa, modLoc)->  [createAAModT searchMod modLoc aa]
                        ) 
            |> List.concat
        
        searchModL
        |> List.map (createAAModTLOfSearchMod aal)
        |> List.concat 
    
    ///Würde ich später in das XmodModul auslagern
    ///Generates a Map with SearchModification.Name as key and the XModCode as a value.
    let createXmodMap(searchModL: SearchModification list) =                                                                                              
        searchModL
        |> Seq.map (fun searchmod -> searchmod.Name, searchmod.XModCode)  
        |> Map.ofSeq

    type ModLookUp = {
        ResidualFixed: AminoAcid -> Modification list option
        NTermAndResidualFixed: AminoAcid -> Modification list option
        CTermAndResidualFixed: AminoAcid -> Modification list option
        ResidualVariable: AminoAcid -> Modification list option
        NTermAndResidualVariable: AminoAcid -> Modification list option
        CTermAndResidualVariable: AminoAcid -> Modification list option
        Total: string -> string option
        }

    let createModLookUp (dbParams:SearchDbParams) = 
        ///Filters list of AminoAcid Modification tuples depending on the used logexp    
        let filterAAModTL (logexp: _*Modification -> bool) searchModL = 
            createAAModTLOfSearchModL listOfAA searchModL 
            |> List.filter (logexp: _*Modification -> bool)
            |> Map.compose

        ///Logexp that returns true if ModLocation of modified AminoAcid equals Residual 
        let residual (_,modi) = 
            ModLocation.Residual.Equals(modi.Location)
        ///Logexp that returns true if ModLocation of modified AminoAcid equals Residual, Nterm or ProteinNterm 
        let nTermAndResidual (_,modi) = 
            ModLocation.Nterm.Equals(modi.Location)||ModLocation.ProteinNterm.Equals(modi.Location) || ModLocation.Residual.Equals(modi.Location)
        ///Logexp that returns true if ModLocation of modified AminoAcid equals Residual, Cterm or ProteinCterm  
        let cTermAndResidual (_,modi) = 
            ModLocation.Cterm.Equals(modi.Location)||ModLocation.ProteinCterm.Equals(modi.Location) || ModLocation.Residual.Equals(modi.Location)
        {                                                      
            ResidualFixed=
                let lookUpR = filterAAModTL residual dbParams.FixedMods
                fun aa -> Map.tryFind aa lookUpR     
            NTermAndResidualFixed=
                let lookUpNR = filterAAModTL nTermAndResidual dbParams.FixedMods
                fun aa -> Map.tryFind aa lookUpNR      
            CTermAndResidualFixed=
                let lookUpCR = filterAAModTL cTermAndResidual dbParams.FixedMods
                fun aa -> Map.tryFind aa lookUpCR     
            ResidualVariable =
                let lookUpR = filterAAModTL residual dbParams.VariableMods
                fun aa -> Map.tryFind aa lookUpR     
            NTermAndResidualVariable =
                let lookUpNR = filterAAModTL nTermAndResidual dbParams.VariableMods
                fun aa -> Map.tryFind aa lookUpNR      
            CTermAndResidualVariable =
                let lookUpCR = filterAAModTL cTermAndResidual dbParams.VariableMods
                fun aa -> Map.tryFind aa lookUpCR   
            Total=
                let lookupT = createXmodMap (dbParams.FixedMods@dbParams.VariableMods)
                fun aa -> Map.tryFind aa lookupT
         }

    ///Returns modified or unmodified AminoAcid depending on the matching expression
    ///The boolean value "false" is used to state that the Modification is fixed
    let createFixedModAA (modLookUp:AminoAcid->Modification list option) (aa: AminoAcid) = 
        match modLookUp aa with
        | Some modiList -> 
            false, modiList
            |> List.fold (fun aa modi -> AminoAcids.setModification modi aa  ) aa                              
        | None -> 
            false,aa
                                                     
    ///First match: checks if input AminoAcid is modified
    ///Second match: returns a (bool*modifiedAminoAcid) list containing all variable modified AminoAcids and the AminoAcid that was given as the function input
    ///The boolean value "false" is used to state that the Modification is fixed, the value "true" marks the Modification as variable 
    // setVarModsFromLookUp
    let createVariableModAAL (modLookUp:AminoAcid->Modification list option) (boolAA: bool*AminoAcid) = 
        //create modified aminoacids
        let createModAAL aa = 
            match modLookUp aa  with
            | Some modiList -> 
                let variableModboolAAL = 
                    modiList 
                    |> List.map (fun modi -> (true,AminoAcids.setModification modi aa))
                boolAA::variableModboolAAL
                                                                
            | None -> [boolAA] 
              
        match boolAA with
        | bool,Mod (aa, _) -> createModAAL aa 
                                                 
        | bool,(aa: AminoAcid) -> createModAAL aa

    type ModifiedPeptide = {
        RealMass: float
        RoundedMass: int64
        PepSequence: string
        }

    let createModifiedPeptide realMass roundedMass pepSequence = {
        RealMass=realMass
        RoundedMass=roundedMass
        PepSequence=pepSequence
        }


    let createModifiedPeptides (modLookUp:ModLookUp) dbParams (threshold:int) (aal: BioList.BioList<AminoAcid>) =
        let createAASString (xModLookUp:string->string option) (aa: AminoAcid) =
            match (aa:AminoAcid) with
            | AminoAcids.Mod (aa, mds) ->  
                                    mds
                                    |> List.fold (fun acc (md:Modification) -> match xModLookUp md.Name with
                                                                                | Some x -> x + acc 
                                                                                | None -> failwithf "Failed"                                                                                         
                                                                                ) "" 
                                    |> (fun x -> x + ((BioItem.symbol aa).ToString()))
                                                     
            | aa -> ((BioItem.symbol aa).ToString())                                                                              
        let addAASymbolToString stringacc (aa:AminoAcid) = ((createAASString modLookUp.Total aa) + stringacc)
        // TODO: Mono or average from user param
        let addMassofMod massOfPeptide (m:Modification list) = massOfPeptide + AminoAcids.monoisoMass (Mod(AminoAcid.Gap, m))  
        // TODO: Mono or average from user param
        let massOfPeptide = BioList.toMonoisotopicMassWithWater (aal:>BioList.BioList<_>)
        let revertedSeq = List.rev aal
        let rec createModifiedPeptidesLoop (massOfPeptide:float) c threshold  (acc:string)  (fixedModLookUp:AminoAcid->Modification list option) (variableModLookUp:AminoAcid->Modification list option) (aal: AminoAcid list) =
            match c with
            | c when c = threshold -> 
                   match aal with
                   | h::tail -> match tail with 
                                | [] -> createFixedModAA modLookUp.NTermAndResidualFixed h 
                                | _  -> createFixedModAA fixedModLookUp h
                                |> (fun item ->
                                        match item with
                                        | false,Mod (a,m) -> 
                                            createModifiedPeptidesLoop (addMassofMod massOfPeptide m) (c) threshold (addAASymbolToString acc (snd item))  modLookUp.ResidualFixed modLookUp.ResidualVariable tail
                                        | false,a         -> 
                                            createModifiedPeptidesLoop massOfPeptide (c) threshold (addAASymbolToString acc (snd item)) modLookUp.ResidualFixed modLookUp.ResidualVariable tail  
                                    )
                   | [] -> [createModifiedPeptide massOfPeptide (Convert.ToInt64(massOfPeptide*1000000.)) acc]  
                                     
            | c -> match aal with
                   | h::tail -> 
                        match tail with
                        | [] -> 
                            h 
                            |> createFixedModAA modLookUp.NTermAndResidualFixed 
                            |> createVariableModAAL modLookUp.NTermAndResidualVariable 
                        | _  -> 
                            h
                            |> createFixedModAA fixedModLookUp  
                            |> createVariableModAAL variableModLookUp
                        |> List.collect (fun item ->
                                                match item with
                                                | true,Mod (a,m)   -> 
                                                    createModifiedPeptidesLoop (addMassofMod massOfPeptide m) (c+1) threshold  (addAASymbolToString acc (snd item)) modLookUp.ResidualFixed modLookUp.ResidualVariable tail                                       
                                                | false, Mod (a,m) -> 
                                                    createModifiedPeptidesLoop (addMassofMod massOfPeptide m) (c) threshold  (addAASymbolToString acc (snd item)) modLookUp.ResidualFixed modLookUp.ResidualVariable tail
                                                | false, a         -> 
                                                    createModifiedPeptidesLoop massOfPeptide (c) threshold  (addAASymbolToString acc (snd item))  modLookUp.ResidualFixed modLookUp.ResidualVariable tail  
                                            )                   
                                                   
                   | [] ->  [createModifiedPeptide massOfPeptide (Convert.ToInt64(massOfPeptide*1000000.)) acc]
                    
    
        createModifiedPeptidesLoop massOfPeptide 0 threshold "" modLookUp.CTermAndResidualFixed modLookUp.CTermAndResidualVariable  revertedSeq  
    
    open System.Data.SQLite
    open SQLiteQuery

    let createStringOfsearchModL (searchModL:SearchModification list) =
        searchModL
        |> List.map (fun a -> a.Name)
        |> String.concat ", "

    let insertSearchDbParams (dbParams:SearchDbParams) (cn:SQLiteConnection) = 
        prepareInsertSearchDbParams cn dbParams.Name dbParams.DbFolder dbParams.FastaPath dbParams.Protease.Name
            dbParams.MissedCleavages dbParams.MaxMass dbParams.IsotopicLabel (dbParams.MassMode.ToString())
            (createStringOfsearchModL dbParams.FixedMods) (createStringOfsearchModL dbParams.VariableMods)
    
    ///Creates DB and inserts Tables
    let initializeDatabase (dbParams:SearchDbParams) =
        let databaseName  = dbParams.DbFolder+dbParams.Name+".db"
        let connectionString = sprintf "Data Source=%s;Version=3" databaseName
        use cn = new SQLiteConnection(connectionString)
        cn.Open()
        SQLiteQuery.createTableSearchDbParams cn
        |> ignore
        SQLiteQuery.createTableProtein cn 
        |> ignore
        SQLiteQuery.createTableCleavageIndex cn
        |> ignore
        SQLiteQuery.createTablePepSequence cn
        |> ignore
        SQLiteQuery.createTableModSequence cn
        |> ignore
        SQLiteQuery.setSequenceIndexOnPepSequence cn
        |> ignore
        cn.Close() 
    
    let insertProcessedFastaReadOutToDB (dbParams:SearchDbParams) fastaReadOut =
        // prepares LookUpMaps of modLookUp based on the dbParams
        let modLookUp = createModLookUp dbParams
        //Create and open SQLiteConnection
        let databaseName = dbParams.DbFolder+dbParams.Name+".db"
        let connectionString = sprintf "Data Source=%s;Version=3" databaseName
        let cn = new SQLiteConnection(connectionString)
        cn.Open()
        //Insertion of SearchDbParams
        insertSearchDbParams dbParams cn 
        |> ignore
        // Begin Transaction
        let tr = cn.BeginTransaction()
        // Bind Insert and Select statements to connection and transaction / prepares statements
        let insertProtein = prepareInsertProtein cn tr
        let insertCleavageIndex = prepareInsertCleavageIndex cn tr
        let insertPepSequence = prepareInsertPepSequence cn tr
        let selectPepSequenceBySequence = prepareSelectPepSequenceBySequence cn tr
        let insertModSequence = prepareInsertModSequence cn tr
        // Insert Protein-, Peptide-, Modified Peptide-Sequences and the CleavageIndices
        let insertProtPepCleavIdxModPepToDB  (proteinID: int32) (protein: FastA.FastaItem<AminoAcid []>) = 
            //printfn "ProtID %i" proteinID
            match insertProtein proteinID protein.Header (toString protein.Sequence) with
            | 1 ->
                Digestion.digest Digestion.trypsin proteinID (protein.Sequence |> Seq.toArray)
                |> Digestion.concernMissCleavages 1 3  
                |> Array.filter (fun x -> 
                                    x.MissCleavageEnd - x.MissCleavageStart > 4
                                ) 
                |> Array.filter (fun x -> 
                                    x.MissCleavageEnd - x.MissCleavageStart < 63
                                )
                |> Array.mapi (fun pepId pep ->
                                            match insertPepSequence (proteinID*10000+pepId) (toString pep.PepSequence) with 
                                            | 1 ->
                                                insertCleavageIndex proteinID (proteinID*10000+pepId) pep.MissCleavageStart
                                                    pep.MissCleavageEnd pep.MissCleavages 
                                                |>ignore
                                                pep.PepSequence
                                                |> createModifiedPeptides modLookUp dbParams 5 
                                                |> List.iter (fun modPep -> 
                                                                insertModSequence (proteinID*10000+pepId) modPep.RealMass modPep.RoundedMass 
                                                                    modPep.PepSequence dbParams.IsotopicLabel 
                                                                |> ignore
                                                              )  

                                            | 0  -> 
                                                insertCleavageIndex proteinID (selectPepSequenceBySequence (toString pep.PepSequence)) 
                                                    pep.MissCleavageStart pep.MissCleavageEnd pep.MissCleavages 
                                                |>ignore 
                                        
                                ) |>ignore
            | 0 -> printfn "Protein is already in the database" |>ignore     
        fastaReadOut 
        |> Seq.iteri insertProtPepCleavIdxModPepToDB
        SQLiteQuery.setMassIndexOnModSequence cn |> ignore
        //Commit and dispose the transaction and close the SQLiteConnection
        tr.Commit()
        tr.Dispose()
        cn.Close() 
        
        

    let selectSearchDbParamsbyParams cn (dbParams:SearchDbParams) = 
        prepareSelectSearchDbParamsbyParams cn dbParams.FastaPath dbParams.Protease.Name dbParams.MissedCleavages 
            dbParams.MaxMass dbParams.IsotopicLabel (dbParams.MassMode.ToString()) 
                (createStringOfsearchModL dbParams.FixedMods) (createStringOfsearchModL dbParams.VariableMods)

    let converter = 
        BioArray.ofAminoAcidStringWithOptionConverter (IBioSequence.OptionConverter.charToOptionStandardAminoAcid)

    let createDatabase (dbParams:SearchDbParams) =
        let checkFileExistance (dbParams:SearchDbParams) = 
            (System.IO.File.Exists (dbParams.DbFolder+dbParams.Name+".db"))
        match checkFileExistance dbParams with 
        | true  -> 
            let databaseName = dbParams.DbFolder+dbParams.Name+".db"
            let connectionString = sprintf "Data Source=%s;Version=3" databaseName
            use cn = new SQLiteConnection(connectionString)
            cn.Open()
            match selectSearchDbParamsbyParams cn dbParams with
            | Success (_)   -> 
                PeptideLookUpError.DbInitialisation_Database_Matching_The_Selected_Parameters_Already_Exists
                |> Either.fail
            | Failure DbSearchParamsItemNotFound -> 
                PeptideLookUpError.DbInitialisation_Database_With_Identical_Name_But_Different_Parameters_Already_Exists
                |> Either.fail
                                                               
        | false -> 
            try
                initializeDatabase dbParams
                insertProcessedFastaReadOutToDB dbParams (FastA.fromFile converter dbParams.FastaPath)
                "Database has been successfully created."
                |> Either.succeed 
            with
            |    ex -> PeptideLookUpError.DbInitialisation (SqlAction.Create,sqlErrorCodeFromException ex)
                        |> Either.fail   
      
                                  
    let selectModSequenceByMassRange cn (toleranceWidth:float) (msMeasuredMass:float) =
        let mass1 = Convert.ToInt64((msMeasuredMass - toleranceWidth)*1000000.)
        let mass2 = Convert.ToInt64((msMeasuredMass + toleranceWidth)*1000000.)
        prepareSelectModsequenceByMassRange cn mass1 mass2