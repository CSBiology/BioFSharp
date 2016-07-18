namespace BioFSharp.Mz

open BioFSharp
open System
open FSharp.Care.Monads

module SQLiteQuery =
    
    open System.Data
    open System.Data.SQLite

        //#define SQLITE_ERROR        1   /* SQL error or missing database */
        //#define SQLITE_INTERNAL     2   /* Internal logic error in SQLite */
        //#define SQLITE_PERM         3   /* Access permission denied */
        //#define SQLITE_ABORT        4   /* Callback routine requested an abort */
        //#define SQLITE_BUSY         5   /* The database file is locked */
        //#define SQLITE_LOCKED       6   /* A table in the database is locked */
        //#define SQLITE_NOMEM        7   /* A malloc() failed */
        //#define SQLITE_READONLY     8   /* Attempt to write a readonly database */
        //#define SQLITE_INTERRUPT    9   /* Operation terminated by sqlite3_interrupt()*/
        //#define SQLITE_IOERR       10   /* Some kind of disk I/O error occurred */
        //#define SQLITE_CORRUPT     11   /* The database disk image is malformed */
        //#define SQLITE_NOTFOUND    12   /* Unknown opcode in sqlite3_file_control() */
        //#define SQLITE_FULL        13   /* Insertion failed because database is full */
        //#define SQLITE_CANTOPEN    14   /* Unable to open the database file */
        //#define SQLITE_PROTOCOL    15   /* Database lock protocol error */
        //#define SQLITE_EMPTY       16   /* Database is empty */
        //#define SQLITE_SCHEMA      17   /* The database schema changed */
        //#define SQLITE_TOOBIG      18   /* String or BLOB exceeds size limit */
        //#define SQLITE_CONSTRAINT  19   /* Abort due to constraint violation */
        //#define SQLITE_MISMATCH    20   /* Data type mismatch */
        //#define SQLITE_MISUSE      21   /* Library used incorrectly */
        //#define SQLITE_NOLFS       22   /* Uses OS features not supported on host */
        //#define SQLITE_AUTH        23   /* Authorization denied */
        //#define SQLITE_FORMAT      24   /* Auxiliary database format error */
        //#define SQLITE_RANGE       25   /* 2nd parameter to sqlite3_bind out of range */
        //#define SQLITE_NOTADB      26   /* File opened that is not a database file */
        //#define SQLITE_NOTICE      27   /* Notifications from sqlite3_log() */
        //#define SQLITE_WARNING     28   /* Warnings from sqlite3_log() */
        //#define SQLITE_ROW         100  /* sqlite3_step() has another row ready */
        //#define SQLITE_DONE        101  /* sqlite3_step() has finished executing */


    type SqlErrorCode =
        | DbDataBaseNotFound
        | DbInternalLogicError
        | DbAccessDenied   
        | UnknownSqlException of SQLiteException
        | Unknown of Exception

    let sqlErrorCodeFromException (ex: Exception) =
        match ex with
        | :? SQLiteException  as ex ->
                match ex.ErrorCode with
                | 1 -> SqlErrorCode.DbDataBaseNotFound
                | 2 -> SqlErrorCode.DbInternalLogicError
                | 3 -> SqlErrorCode.DbAccessDenied
                | _ -> SqlErrorCode.UnknownSqlException ex
        |  _ ->  SqlErrorCode.Unknown ex

    type SqlAction =
        | Create
        | Select
        | Insert
        | Delet
        | Update

    type PeptideLookUpError =
        | DbProtein of SqlAction * SqlErrorCode
        | DbProteinItemNotFound
        | DbCleavageIndex of SqlAction * SqlErrorCode 
        | DbCleavageIndexItemNotFound
        | DbPepSequence of SqlAction * SqlErrorCode
        | DbPepSequenceItemNotFound
        | DbModSequence of SqlAction * SqlErrorCode
        | DbModSequenceItemNotFound
        | DbSearchmodification of SqlAction * SqlErrorCode
        | DbSearchmodificationItemNotFound
        | DbSearchParams of SqlAction * SqlErrorCode
        | DbSearchParamsItemNotFound
        | DbInitialisation of SqlAction * SqlErrorCode
        | DbInitialisation_Database_Matching_The_Selected_Parameters_Already_Exists
        | DbInitialisation_Database_With_Identical_Name_But_Different_Parameters_Already_Exists

    //Create DB table statements
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    /// Creates Table SearchDbParams
    let createTableSearchDbParams  (cn:SQLiteConnection) = 
        let querystring = 
            "CREATE TABLE SearchDbParams (ID INTEGER, 
                                         Name TEXT NOT NULL, 
                                         DbFolder TEXT NOT NULL, 
                                         FastaPath  TEXT NOT NULL, 
                                         Protease  TEXT NOT NULL, 
                                         MissedCleavages  INTEGER NOT NULL,
                                         MaxMass  INTEGER NOT NULL, 
                                         IsotopicLabel  TEXT NOT NULL, 
                                         MassMode  TEXT NOT NULL, 
                                         FixedMods TEXT NOT NULL,   
                                         VariableMods TEXT NOT NULL,   
                                         PRIMARY KEY (ID ASC)
                                         )"
        let cmd = new SQLiteCommand(querystring, cn)
        cmd.ExecuteNonQuery()

    /// Creates Table Protein
    let createTableProtein  (cn:SQLiteConnection) =
        let querystring = 
            "CREATE TABLE Protein (ID  INTEGER,
                                   Accession  TEXT NOT NULL,
                                   Sequence  TEXT NOT NULL,
                                   PRIMARY KEY (ID ASC)
                                   )"
        let cmd = new SQLiteCommand(querystring, cn)
        cmd.ExecuteNonQuery()

    /// Creates Table CleavageIndex
    let createTableCleavageIndex  (cn:SQLiteConnection) = 
        let querystring = 
            "CREATE TABLE CleavageIndex (ID INTEGER, 
                                         ProteinID INTEGER NOT NULL, 
                                         PepSequenceID INTEGER NOT NULL, 
                                         CleavageStart  INTEGER NOT NULL, 
                                         CleavageEnd  INTEGER NOT NULL, 
                                         MissCleavages  INTEGER NOT NULL, 
                                         PRIMARY KEY (ID ASC),
                                         CONSTRAINT ProteinID FOREIGN KEY (ProteinID) REFERENCES Protein (ID),
                                         CONSTRAINT PepSequenceID FOREIGN KEY (PepSequenceID) REFERENCES PepSequence (ID)
                                         )"
        let cmd = new SQLiteCommand(querystring, cn)
        cmd.ExecuteNonQuery()

    /// Creates Table PepSequence
    let createTablePepSequence  (cn:SQLiteConnection) = 
        let querystring = 
            "CREATE TABLE PepSequence (ID INTEGER,
                                       Sequence TEXT NOT NULL COLLATE NOCASE ,
                                       PRIMARY KEY (ID ASC),
                                       CONSTRAINT PepSequenceUnique UNIQUE (Sequence ASC) ON CONFLICT IGNORE
                                       )"
        let cmd = new SQLiteCommand(querystring, cn)
        cmd.ExecuteNonQuery()

    /// Creates Table ModSequence
    let createTableModSequence  (cn:SQLiteConnection) =
        let querystring = 
            "CREATE TABLE ModSequence (ID	INTEGER,
	                                   PepSequenceID INTEGER NOT NULL,
	                                   RealMass REAL NOT NULL,
	                                   RoundedMass INTEGER NOT NULL,
	                                   Sequence TEXT NOT NULL,
	                                   GlobalMod TEXT NOT NULL,
	                                   PRIMARY KEY (ID),
	                                   FOREIGN KEY (PepSequenceID) REFERENCES PepSequence (ID) 
                                       )"
        let cmd = new SQLiteCommand(querystring, cn)
        cmd.ExecuteNonQuery()

    //Create Index Statements
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    let setMassIndexOnModSequence (cn:SQLiteConnection) = 
        let querystring = "CREATE INDEX RoundedMassIndex ON ModSequence (RoundedMass ASC) "
        let cmd = new SQLiteCommand(querystring, cn)    
        cmd.ExecuteNonQuery()

    let setSequenceIndexOnPepSequence (cn:SQLiteConnection) = 
        let querystring = "CREATE INDEX SequenceIndex ON PepSequence (Sequence ASC) "
        let cmd = new SQLiteCommand(querystring, cn)    
        cmd.ExecuteNonQuery()

    //Manipulate Pragma Statements
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    let pragmaSynchronousOFF (cn:SQLiteConnection) = 
        let querystring = "PRAGMA synchronous = 0 "
        let cmd = new SQLiteCommand(querystring, cn)
        // result equals number of affected rows
        cmd.ExecuteNonQuery()
         
    //Insert Statements
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    /// Prepares statement to insert a Protein entry
    let prepareInsertProtein (cn:SQLiteConnection) (tr) =
        let querystring = "INSERT INTO Protein (ID, Accession, Sequence) VALUES (@id, @accession, @sequence)"
        let cmd = new SQLiteCommand(querystring, cn, tr)
        cmd.Parameters.Add("@id", Data.DbType.Int32) |> ignore
        cmd.Parameters.Add("@accession", Data.DbType.String) |> ignore
        cmd.Parameters.Add("@sequence", Data.DbType.String) |> ignore
    
        (fun (id:int32) (accession:string) (sequence:string)  ->  
                cmd.Parameters.["@id"].Value        <- id
                cmd.Parameters.["@accession"].Value <- accession
                cmd.Parameters.["@sequence"].Value  <- sequence
                // result equals number of affected rows
                cmd.ExecuteNonQuery()
                )

   
    /// Prepares statement to insert a CleavageIndex entry
    let prepareInsertCleavageIndex (cn:SQLiteConnection) (tr) =
        let querystring = "INSERT INTO CleavageIndex (ProteinID, 
                                                      PepSequenceID, 
                                                      CleavageStart, 
                                                      CleavageEnd, 
                                                      MissCleavages) 
                                                      VALUES (@proteinID, 
                                                              @pepSequenceID, 
                                                              @cleavageStart, 
                                                              @cleavageEnd, 
                                                              @missCleavages)"
        let cmd = new SQLiteCommand(querystring, cn, tr)
        //  cmd.Parameters.Add("@id", Data.DbType.Int64) |> ignore
        cmd.Parameters.Add("@proteinID", Data.DbType.Int32) |> ignore
        cmd.Parameters.Add("@pepSequenceID", Data.DbType.Int32) |> ignore
        cmd.Parameters.Add("@cleavageStart", Data.DbType.Int32) |> ignore
        cmd.Parameters.Add("@cleavageEnd", Data.DbType.Int32) |> ignore
        cmd.Parameters.Add("@missCleavages", Data.DbType.Int32) |> ignore
    
        (fun  (proteinID:int32) (pepSequenceID:int32) (cleavageStart:int32) (cleavageEnd:int32) (missCleavages:int32)  -> // (id:uint64)
                // cmd.Parameters.["@id"].Value            <- id
                cmd.Parameters.["@proteinID"].Value     <- proteinID
                cmd.Parameters.["@pepSequenceID"].Value <- pepSequenceID
                cmd.Parameters.["@cleavageStart"].Value <- cleavageStart
                cmd.Parameters.["@cleavageEnd"].Value   <- cleavageEnd
                cmd.Parameters.["@missCleavages"].Value <- missCleavages
                // result equals number of affected rows
                cmd.ExecuteNonQuery()
                )


    /// Prepares statement to insert a PepSequence entry
    let prepareInsertPepSequence (cn:SQLiteConnection) (tr) =
        let querystring = "INSERT INTO PepSequence (ID, Sequence) VALUES (@id, @sequence)"
        let cmd = new SQLiteCommand(querystring, cn, tr)
        cmd.Parameters.Add("@id", Data.DbType.Int32) |> ignore
        cmd.Parameters.Add("@sequence", Data.DbType.String) |> ignore
    
        (fun (id:int32) (sequence:string)  ->
                cmd.Parameters.["@id"].Value  <- id
                cmd.Parameters.["@sequence"].Value  <- sequence
                // result equals number of affected rows
                cmd.ExecuteNonQuery()
                )



    /// Prepares statement to insert a ModSequence entry
    let prepareInsertModSequence (cn:SQLiteConnection) (tr) =
        let querystring = "INSERT INTO ModSequence (PepSequenceID, 
                                                    RealMass, 
                                                    RoundedMass, 
                                                    Sequence, 
                                                    GlobalMod) 
                                                    VALUES (@pepSequenceID, 
                                                            @realmass, 
                                                            @roundedmass, 
                                                            @sequence, 
                                                            @globalMod)" //ID, @id
        let cmd = new SQLiteCommand(querystring, cn, tr)
        //cmd.Parameters.Add("@id", Data.DbType.Int64) |> ignore
        cmd.Parameters.Add("@pepSequenceID", Data.DbType.Int32) |> ignore
        cmd.Parameters.Add("@realmass", Data.DbType.Double) |> ignore
        cmd.Parameters.Add("@roundedmass", Data.DbType.Int64) |> ignore        
        cmd.Parameters.Add("@sequence", Data.DbType.String) |> ignore
        cmd.Parameters.Add("@globalMod", Data.DbType.String) |> ignore
    
        (fun  (pepSequenceID:int32) (realmass: float) (roundedmass: int64) (sequence: string) (globalMod:string)  -> //(id:uint64)
                // cmd.Parameters.["@id"].Value            <- id            
                cmd.Parameters.["@pepSequenceID"].Value <- pepSequenceID
                cmd.Parameters.["@realmass"].Value     <- realmass
                cmd.Parameters.["@roundedmass"].Value     <- roundedmass
                cmd.Parameters.["@sequence"].Value      <- sequence
                cmd.Parameters.["@globalMod"].Value          <- globalMod
                // result equals number of affected rows
                cmd.ExecuteNonQuery()
                )

    /// Prepares statement to insert a SearchDBParams entry
    let prepareInsertSearchDbParams (cn:SQLiteConnection) =
        let querystring = "INSERT INTO SearchDbParams (Name,
                                                       DbFolder, 
                                                       FastaPath, 
                                                       Protease, 
                                                       MissedCleavages, 
                                                       MaxMass, 
                                                       IsotopicLabel, 
                                                       MassMode, 
                                                       FixedMods, 
                                                       VariableMods) 
                                                       VALUES (@name, 
                                                               @dbFolder, 
                                                               @fastaPath, 
                                                               @protease, 
                                                               @missedCleavages, 
                                                               @maxMass, 
                                                               @isotopicLabel, 
                                                               @massMode, 
                                                               @fixedMods, 
                                                               @variableMods)"
        let cmd = new SQLiteCommand(querystring, cn)
        cmd.Parameters.Add("@name", Data.DbType.String) |> ignore
        cmd.Parameters.Add("@dbFolder", Data.DbType.String) |> ignore
        cmd.Parameters.Add("@fastaPath", Data.DbType.String) |> ignore
        cmd.Parameters.Add("@protease", Data.DbType.String) |> ignore
        cmd.Parameters.Add("@missedCleavages", Data.DbType.Int32) |> ignore
        cmd.Parameters.Add("@maxMass", Data.DbType.Double) |> ignore
        cmd.Parameters.Add("@isotopicLabel", Data.DbType.String) |> ignore
        cmd.Parameters.Add("@massMode", Data.DbType.String) |> ignore
        cmd.Parameters.Add("@fixedMods", Data.DbType.String) |> ignore
        cmd.Parameters.Add("@variableMods", Data.DbType.String) |> ignore 
        (fun (name:string) (dbFolder:string) (fastaPath:string) (protease:string) (missedCleavages:int32) (maxMass:float) 
            (isotopicLabel:string) (massMode:string) (fixedMods:string) (variableMods:string)  ->  
                cmd.Parameters.["@name"].Value             <- name
                cmd.Parameters.["@dbFolder"].Value         <- dbFolder
                cmd.Parameters.["@fastaPath"].Value        <- fastaPath
                cmd.Parameters.["@protease"].Value         <- protease
                cmd.Parameters.["@missedCleavages"].Value  <- missedCleavages
                cmd.Parameters.["@maxMass"].Value          <- maxMass
                cmd.Parameters.["@isotopicLabel"].Value    <- isotopicLabel
                cmd.Parameters.["@massMode"].Value         <- massMode
                cmd.Parameters.["@fixedMods"].Value        <- fixedMods
                cmd.Parameters.["@variableMods"].Value     <- variableMods
                // result equals number of affected rows
                cmd.ExecuteNonQuery()
                )
                    
    //Select Statements
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    /// Prepares statement to select all SearchDbParams entries by FastaPath, Protease MissedCleavages, MaxMass, IsotopicLabel, MassMode, FixedMods, VariableMods
    let prepareSelectSearchDbParamsbyParams (cn:SQLiteConnection) =
        let querystring = "SELECT * FROM SearchDbParams WHERE FastaPath=@fastaPath 
                                                        AND Protease=@protease 
                                                        AND MissedCleavages=@missedCleavages 
                                                        AND MaxMass=@maxMass 
                                                        AND IsotopicLabel=@isotopicLabel 
                                                        AND MassMode=@massMode 
                                                        AND FixedMods=@fixedMods 
                                                        AND VariableMods=@variableMods"
        let cmd = new SQLiteCommand(querystring, cn) 
        cmd.Parameters.Add("@fastaPath", Data.DbType.String) |> ignore
        cmd.Parameters.Add("@protease", Data.DbType.String) |> ignore  
        cmd.Parameters.Add("@missedCleavages", Data.DbType.Int32) |> ignore  
        cmd.Parameters.Add("@maxMass", Data.DbType.Double) |> ignore  
        cmd.Parameters.Add("@isotopicLabel", Data.DbType.String) |> ignore  
        cmd.Parameters.Add("@massMode", Data.DbType.String) |> ignore  
        cmd.Parameters.Add("@fixedMods", Data.DbType.String) |> ignore  
        cmd.Parameters.Add("@variableMods", Data.DbType.String) |> ignore       
        (fun (fastaPath:string) (protease:string) (missedCleavages:int32) (maxMass:float) 
            (isotopicLabel:string) (massMode:string) (fixedMods:string) (variableMods:string) ->         
            cmd.Parameters.["@fastaPath"].Value <- fastaPath
            cmd.Parameters.["@protease"].Value <- protease
            cmd.Parameters.["@missedCleavages"].Value <- missedCleavages
            cmd.Parameters.["@maxMass"].Value <- maxMass
            cmd.Parameters.["@isotopicLabel"].Value <- isotopicLabel
            cmd.Parameters.["@massMode"].Value <- massMode
            cmd.Parameters.["@fixedMods"].Value <- fixedMods
            cmd.Parameters.["@variableMods"].Value <- variableMods
            try         
                use reader = cmd.ExecuteReader()            
                match reader.Read() with
                | true -> (reader.GetInt32(0), reader.GetString(1), reader.GetString(2), reader.GetString(3), reader.GetString(4), 
                            reader.GetInt32(5), reader.GetDouble(6), reader.GetString(7), reader.GetString(8), reader.GetString(9), reader.GetString(10))
                           |> Either.succeed
                | false -> PeptideLookUpError.DbSearchParamsItemNotFound
                            |> Either.fail

             
            with            
            | ex -> PeptideLookUpError.DbSearchParams (SqlAction.Select,sqlErrorCodeFromException ex) 
                    |> Either.fail
        )

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    /// Prepares statement to select a Protein entry by ID        
    let prepareSelectProteinByID (cn:SQLiteConnection) (tr) =
        let querystring = "SELECT * FROM Protein WHERE ID=@id "
        let cmd = new SQLiteCommand(querystring, cn, tr) 
        cmd.Parameters.Add("@id", Data.DbType.Int32) |> ignore       
        (fun (id:int32)  ->         
            cmd.Parameters.["@id"].Value <- id
            try
                        
                use reader = cmd.ExecuteReader()            
                match reader.Read() with
                | true -> (reader.GetInt32(0),reader.GetString(1), reader.GetString(2)) |> Either.succeed
                | false -> PeptideLookUpError.DbProteinItemNotFound
                            |> Either.fail

             
            with            
            | ex -> PeptideLookUpError.DbProtein (SqlAction.Select,sqlErrorCodeFromException ex) 
                    |> Either.fail
        )
    /// Prepares statement to select a Protein entry by Accession     
    let prepareSelectProteinByAccession (cn:SQLiteConnection) (tr) =
        let querystring = "SELECT * FROM Protein WHERE Accession=@accession"
        let cmd = new SQLiteCommand(querystring, cn, tr) 
        cmd.Parameters.Add("@accession", Data.DbType.String) |> ignore       
        (fun (accession:string)  ->         
            cmd.Parameters.["@accession"].Value <- accession
        
            try
                        
                use reader = cmd.ExecuteReader()            
                match reader.Read() with
                | true -> (reader.GetInt32(0),reader.GetString(1), reader.GetString(2)) |> Either.succeed
                | false -> PeptideLookUpError.DbProteinItemNotFound
                            |> Either.fail

             
            with            
            | ex -> PeptideLookUpError.DbProtein (SqlAction.Select,sqlErrorCodeFromException ex) 
                    |> Either.fail
        )
    /// Prepares statement to select a Protein entry by Sequence     
    let prepareSelectProteinBySequence (cn:SQLiteConnection) (tr) =
        let querystring = "SELECT * FROM Protein WHERE Sequence=@sequence"
        let cmd = new SQLiteCommand(querystring, cn, tr) 
        cmd.Parameters.Add("@sequence", Data.DbType.String) |> ignore       
        (fun (sequence:string)  ->         
            cmd.Parameters.["@sequence"].Value <- sequence
        
            try
                        
                use reader = cmd.ExecuteReader()            
                match reader.Read() with
                | true -> (reader.GetInt32(0),reader.GetString(1), reader.GetString(2)) |> Either.succeed
                | false -> PeptideLookUpError.DbProteinItemNotFound
                            |> Either.fail

             
            with            
            | ex -> PeptideLookUpError.DbProtein (SqlAction.Select,sqlErrorCodeFromException ex) 
                    |> Either.fail
        )

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    /// Prepares statement to select a CleavageIndex entry ProteinID 
    let prepareSelectCleavageIndexByProteinID (cn:SQLiteConnection) (tr) =
        let querystring = "SELECT * FROM CleavageIndex WHERE ProteinID=@proteinID"
        let cmd = new SQLiteCommand(querystring, cn, tr) 
        cmd.Parameters.Add("@proteinID", Data.DbType.Int64) |> ignore       
        (fun (proteinID:int32)  ->         
            cmd.Parameters.["@proteinID"].Value <- proteinID
        
            try
                        
                use reader = cmd.ExecuteReader()            
                match reader.Read() with
                | true -> (reader.GetInt32(0), reader.GetInt32(1), reader.GetInt32(2), reader.GetInt32(3), reader.GetInt32(4), reader.GetInt32(5)) 
                          |> Either.succeed
                | false -> PeptideLookUpError.DbCleavageIndexItemNotFound
                            |> Either.fail

             
            with            
            | ex -> PeptideLookUpError.DbCleavageIndex (SqlAction.Select,sqlErrorCodeFromException ex) 
                    |> Either.fail
        )

    /// Prepares statement to select a CleavageIndex entry PepSequenceID 
    let prepareSelectCleavageIndexByPepSequenceID  (cn:SQLiteConnection) (tr) =
        let querystring = "SELECT * FROM CleavageIndex WHERE PepSequenceID=@pepSequenceID"
        let cmd = new SQLiteCommand(querystring, cn, tr) 
        cmd.Parameters.Add("@pepSequenceID", Data.DbType.Int32) |> ignore       
        (fun (pepSequenceID:int32)  ->         
            cmd.Parameters.["@pepSequenceID"].Value <- pepSequenceID
        
            try
                        
                use reader = cmd.ExecuteReader()            
                match reader.Read() with
                | true -> (reader.GetInt32(0), reader.GetInt32(1), reader.GetInt32(2), reader.GetInt32(3), reader.GetInt32(4), reader.GetInt32(5)) 
                          |> Either.succeed
                | false -> PeptideLookUpError.DbCleavageIndexItemNotFound
                            |> Either.fail

             
            with            
            | ex -> PeptideLookUpError.DbCleavageIndex (SqlAction.Select,sqlErrorCodeFromException ex) 
                    |> Either.fail
        )

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    /// Prepares statement to select a PepSequence entry by PepSequence 
    let prepareSelectPepSequenceBySequence' (cn:SQLiteConnection) (tr) =
        let querystring = "SELECT * FROM PepSequence WHERE Sequence=@sequence"
        let cmd = new SQLiteCommand(querystring, cn, tr) 
        cmd.Parameters.Add("@sequence", Data.DbType.String) |> ignore       
        (fun (sequence:string)  ->         
            cmd.Parameters.["@sequence"].Value <- sequence
            try       
                use reader = cmd.ExecuteReader()
                match reader.Read() with 
                | true ->  reader.GetInt32(0) |> Either.succeed         
                | false -> PeptideLookUpError.DbPepSequenceItemNotFound
                           |> Either.fail


            with
            | ex -> PeptideLookUpError.DbCleavageIndex (SqlAction.Select,sqlErrorCodeFromException ex)
                    |> Either.fail
        )
    /// Prepares statement to select a PepSequence entry by PepSequence - Version without try.. with pattern to enhance the Select performance
    let prepareSelectPepSequenceBySequence (cn:SQLiteConnection) (tr) =
        let querystring = "SELECT * FROM PepSequence WHERE Sequence=@sequence"
        let cmd = new SQLiteCommand(querystring, cn, tr) 
        cmd.Parameters.Add("@sequence", Data.DbType.String) |> ignore       
        (fun (sequence:string)  ->         
            cmd.Parameters.["@sequence"].Value <- sequence       
            use reader = cmd.ExecuteReader()
            reader.Read() |> ignore 
            reader.GetInt32(0)           
            )

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    /// Prepares statement to select a ModSequence entry by PepSequenceID
    let prepareSelectModsequenceByPepSequenceID (cn:SQLiteConnection) (tr) =
        let querystring = "SELECT * FROM ModSequence WHERE PepSequenceID=@pepSequenceID"
        let cmd = new SQLiteCommand(querystring, cn, tr) 
        cmd.Parameters.Add("@pepSequenceID", Data.DbType.Int32) |> ignore       
        (fun (pepSequenceID:int32)  ->        
            cmd.Parameters.["@pepSequenceID"].Value <- pepSequenceID
            try
                        
                use reader = cmd.ExecuteReader()            
                match reader.Read() with
                | true ->  (reader.GetInt32(0), reader.GetInt32(1),reader.GetDouble(2), reader.GetInt64(3), reader.GetString(4), reader.GetString(5)) 
                           |> Either.succeed
                | false -> PeptideLookUpError.DbModSequenceItemNotFound
                            |> Either.fail

             
            with            
            | ex -> PeptideLookUpError.DbModSequence (SqlAction.Select,sqlErrorCodeFromException ex) 
                    |> Either.fail
        )

    /// Prepares statement to select a ModSequence entry by Mass
    let prepareSelectModsequenceByMass (cn:SQLiteConnection) (tr) =
        let querystring = "SELECT * FROM ModSequence WHERE Mass=@mass"
        let cmd = new SQLiteCommand(querystring, cn, tr) 
        cmd.Parameters.Add("@mass", Data.DbType.Int32) |> ignore       
        (fun (mass: int)  ->        
            cmd.Parameters.["@mass"].Value <- mass
            try
                        
                use reader = cmd.ExecuteReader()            
                match reader.Read() with
                | true -> (reader.GetInt32(0), reader.GetInt32(1),reader.GetDouble(2), reader.GetInt64(3), reader.GetString(4), reader.GetString(5))  
                          |> Either.succeed
                | false -> PeptideLookUpError.DbModSequenceItemNotFound
                            |> Either.fail

             
            with            
            | ex -> PeptideLookUpError.DbModSequence (SqlAction.Select,sqlErrorCodeFromException ex) 
                    |> Either.fail
        )

    /// Prepares statement to select a ModSequence entry by Massrange (Between selected Mass -/+ the selected toleranceWidth
    let prepareSelectModsequenceByMassRange (cn:SQLiteConnection) (mass1:int64) (mass2:int64) =
        let querystring = "SELECT * FROM ModSequence WHERE RoundedMass BETWEEN @mass1 AND @mass2"
        let cmd = new SQLiteCommand(querystring, cn) 
        cmd.Parameters.Add("@mass1", Data.DbType.Int64) |> ignore
        cmd.Parameters.Add("@mass2", Data.DbType.Int64) |> ignore
        let rec readerloop (reader:SQLiteDataReader) (acc:(int*int*float*int64*string*string) list) =
                match reader.Read() with 
                | true  -> readerloop reader (( reader.GetInt32(0), reader.GetInt32(1),reader.GetDouble(2), reader.GetInt64(3), reader.GetString(4), reader.GetString(5) ) :: acc)
                | false ->  acc 

        cmd.Parameters.["@mass1"].Value <- mass1
        cmd.Parameters.["@mass2"].Value <- mass2
                
        use reader = cmd.ExecuteReader()            
        readerloop reader [] 

    /// Prepares statement to select a ModSequence entry by Sequence
    let prepareSelectModsequenceBySequence (cn:SQLiteConnection) (tr) =
        let querystring = "SELECT * FROM ModSequence WHERE Sequence=@sequence"
        let cmd = new SQLiteCommand(querystring, cn, tr) 
        cmd.Parameters.Add("@sequence", Data.DbType.Double) |> ignore       
        (fun (sequence:string)  ->        
            cmd.Parameters.["@sequence"].Value <- sequence
            try
                                    
                use reader = cmd.ExecuteReader()            
                match reader.Read() with
                | true -> (reader.GetInt32(0), reader.GetInt32(1),reader.GetDouble(2), reader.GetInt64(3), reader.GetString(4), reader.GetString(5))  
                          |> Either.succeed
                | false -> PeptideLookUpError.DbModSequenceItemNotFound
                            |> Either.fail

             
            with            
            | ex -> PeptideLookUpError.DbModSequence (SqlAction.Select,sqlErrorCodeFromException ex) 
                    |> Either.fail
        )

        /// Prepares statement to select all SearchDbParams entries by ID
    let prepareSelectSearchDbParams (cn:SQLiteConnection) =
        let querystring = "SELECT * FROM SearchDbParams"
        let cmd = new SQLiteCommand(querystring, cn) 
        cmd.Parameters.Add("@id", Data.DbType.Int32) |> ignore       
        (fun (id:int32)  ->         
            cmd.Parameters.["@id"].Value <- id
            try         
                use reader = cmd.ExecuteReader()            
                match reader.Read() with
                | true -> (reader.GetInt32(0), reader.GetString(1), reader.GetString(2), reader.GetString(3), reader.GetString(4), 
                            reader.GetInt32(5), reader.GetDouble(6), reader.GetString(7), reader.GetString(8), reader.GetString(9), reader.GetString(10))
                            |> Either.succeed
                | false -> PeptideLookUpError.DbProteinItemNotFound
                            |> Either.fail

             
            with            
            | ex -> PeptideLookUpError.DbSearchParams (SqlAction.Select,sqlErrorCodeFromException ex) 
                    |> Either.fail
        )
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////    
    ////How to create a new Connection:
//    let connectionString = ("Data Source=C:/Users/Databasefolder/Database.db;Version=3")
//    let cn = new SQLiteConnection(connectionString)
//    cn.Open()
//    cn.Close()

    ////How to open Databases and deal with Transactions:
    //cn.Open()
    //let tr = cn.BeginTransaction()
    //Insert 1
    //  .
    //  .
    //  .
    //Insert N
    //tr.Close()
    //tr.Dispose()
    //cn.Close()

    //// How to use prepared Statements via Closure: 

//    let connectionString = ("Data Source=C:/Users/Databasefolder/Database.db;Version=3")
//    let cn = new SQLiteConnection(connectionString)
//    let tr = cn.BeginTransaction()
 
//    let insertProteinSequence = prepareInsertProtein cn tr
//    insertProtein "PEPTIDE"

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////   





 