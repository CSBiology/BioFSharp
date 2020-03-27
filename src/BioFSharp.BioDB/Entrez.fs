namespace BioFSharp.BioDB


module Entrez =

    open Hopac
    open HttpFs
    open HttpFs.Client
    open Newtonsoft.Json
    open System
    
    [<AutoOpen>]
    module internal Request =
        
        let queryStringItems (qItems: (string*string) list) (r:Request) =
            let rec loop (qItems: (string*string) list) (r:Request) =
                match qItems with
                | []         -> r
                | (name,value)::tail   -> 
                    loop tail (r |> Request.queryStringItem name value)
            loop qItems r

    module internal BaseUrls =
        [<Literal>] 
        let eInfo       = @"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/einfo.fcgi"
        [<Literal>] 
        let eSearch     = @"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
        [<Literal>] 
        let ePost       = @"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/epost.fcgi"
        [<Literal>] 
        let eSummary    = @"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"
        [<Literal>] 
        let eFetch      = @"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
        [<Literal>] 
        let eLink       = @"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi"
        [<Literal>] 
        let egQuery     = @"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/egquery.fcgi"
        [<Literal>] 
        let eSpell      = @"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/espell.fcgi"
        [<Literal>] 
        let ecitMatch   = @"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/ecitmatch.cgi"


    type RetrievalModeOptions =
        |XML
        |JSON

        static member make = function
             |XML   -> "xml"
             |JSON  -> "json"

    ///DSL for constructing and executing eInfo queries
    ///
    ///Endpoint Functions:
    ///
    /// - Provides a list of the names of all valid Entrez databases
    ///
    /// - Provides statistics for a single database, including lists of indexing fields and available link names
    module EntrezInfo =
        
        type EntrezInfoParameters =
            ///Target database about which to gather statistics. Value must be a valid Entrez database name.
            |Db             of string
            ///Used to specify version 2.0 EInfo XML. The only supported value is ‘2.0’. When present, EInfo will return XML that includes two new fields: <IsTruncatable> and <IsRangeable>. Fields that are truncatable allow the wildcard character ‘*’ in terms. The wildcard character will expand to match any set of characters up to a limit of 600 unique expansions. Fields that are rangeable allow the range operator ‘:’ to be placed between a lower and upper limit for the desired range (e.g. 2008:2010[pdat]).
            |Version        of string
            ///Retrieval type. Determines the format of the returned output. The default value is ‘xml’ for EInfo XML, but ‘json’ is also supported to return output in JSON format.
            |RetrievalMode  of RetrievalModeOptions

            static member makeQuery = function
                |Db            q    -> ("db"        , q)
                |Version       q    -> ("version"   , q)
                |RetrievalMode q    -> ("retmode"   , q |> RetrievalModeOptions.make)

        type EntrezInfoQuery = 
            {
                OptionalParameters: EntrezInfoParameters list
            }

            static member makeRequest (q : EntrezInfoQuery) = 

                let optParams = 
                    q.OptionalParameters |> List.map EntrezInfoParameters.makeQuery

                Request.createUrl Get BaseUrls.eInfo
                |> Request.queryStringItems optParams

    ///DSL for constructing and executing eSearch queries
    ///
    ///Endpoint Functions:
    ///
    /// - Provides a list of UIDs matching a text query
    ///
    /// - Posts the results of a search on the History server
    ///
    /// - Downloads all UIDs from a dataset stored on the History server
    ///
    /// - Combines or limits UID datasets stored on the History server
    ///
    /// - Sorts sets of UIDs
    module EntrezSearch =
    
        type EntrezSearchRetrievalTypeOptions =
            ///
            |Standard
            ///
            |Count
            
            static member make = function
                 |Standard  -> "uilist"
                 |Count     -> "count"

        type EntrezSearchHistoryServerParams =
            ///ESearch will post the UIDs resulting from the search operation onto the History server so that they can be used directly in a subsequent E-utility call. Also, usehistory must be set to 'y' for ESearch to interpret query key values included in term or to accept a WebEnv as input.
            |UseHistory     
            ///Web environment string returned from a previous ESearch, EPost or ELink call. When provided, ESearch will post the results of the search operation to this pre-existing WebEnv, thereby appending the results to the existing environment. In addition, providing WebEnv allows query keys to be used in term so that previous search sets can be combined or limited. As described above, if WebEnv is used, usehistory must be set to 'y'.
            |WebEnvironment of string
            ///Integer query key returned by a previous ESearch, EPost or ELink call. When provided, ESearch will find the intersection of the set specified by query_key and the set retrieved by the query in term (i.e. joins the two with AND). For query_key to function, WebEnv must be assigned an existing WebEnv string and usehistory must be set to 'y'.
            |QueryKey       of int

            static member makeQuery = function
                |UseHistory       -> ("usehistory"  ,"y"        )
                |WebEnvironment q -> ("WebEnv"      , q         )
                |QueryKey       q -> ("query_key"   , string q  )

        type EntrezSearchRetrievalParams =
            ///Sequential index of the first UID in the retrieved set to be shown in the XML output (default=0, corresponding to the first record of the entire set). This parameter can be used in conjunction with retmax to download an arbitrary subset of UIDs retrieved from a search
            |RetrievalStart of int
            ///Total number of UIDs from the retrieved set to be shown in the XML output (default=20). By default, ESearch only includes the first 20 UIDs retrieved in the XML output. If usehistory is set to 'y', the remainder of the retrieved set will be stored on the History server; otherwise these UIDs are lost. Increasing retmax allows more of the retrieved UIDs to be included in the XML output, up to a maximum of 100,000 records. To retrieve more than 100,000 UIDs, submit multiple esearch requests while incrementing the value of retstart
            |RetrievalMax   of int
            ///Retrieval type. There are two allowed values for ESearch: 'uilist' (default), which displays the standard XML output, and 'count', which displays only the <Count> tag.
            |RetrievalType  of EntrezSearchRetrievalTypeOptions
            ///Retrieval type. Determines the format of the returned output. The default value is ‘xml’ for ESearch XML, but ‘json’ is also supported to return output in JSON format.
            |RetrievalMode  of RetrievalModeOptions
            ///Specifies the method used to sort UIDs in the ESearch output. The available values vary by database (db) and may be found in the Display Settings menu on an Entrez search results page. If usehistory is set to ‘y’, the UIDs are loaded onto the History Server in the specified sort order and will be retrieved in that order by ESummary or EFetch. Example values are ‘relevance’ and ‘name’ for Gene and ‘first+author’ and ‘pub+date’ for PubMed. Users should be aware that the default value of sort varies from one database to another, and that the default value used by ESearch for a given database may differ from that used on NCBI web search pages.
            |Sort           of string
            ///Search field. If used, the entire search term will be limited to the specified Entrez field.
            |Field          of string
            ///Specifies the type of identifier to return for sequence databases (nuccore, nucest, nucgss, popset, protein). By default, ESearch returns GI numbers in its output. If idtype is set to ‘acc’, ESearch will return accession.version identifiers rather than GI numbers
            |Idtype         of string
            
            static member makeQuery = function
                |RetrievalStart q -> ("retstart"    ,q |> string)
                |RetrievalMax   q -> ("retmax"      ,q |> string)
                |RetrievalType  q -> ("rettype"     ,q |> EntrezSearchRetrievalTypeOptions.make)
                |RetrievalMode  q -> ("retmode"     ,q |> RetrievalModeOptions.make)
                |Sort           q -> ("sort"        ,q )
                |Field          q -> ("field"       ,q )
                |Idtype         q -> ("idtype"      ,q )

        
        type EntrezSearchDateParams =
            ///Type of date used to limit a search. The allowed values vary between Entrez databases, but common values are 'mdat' (modification date), 'pdat' (publication date) and 'edat' (Entrez date). Generally an Entrez database will have only two allowed values for datetype.
            |Datetype   of string
            ///When reldate is set to an integer n, the search returns only those items that have a date specified by datetype within the last n days.
            |RelDate    of int
            ///Lower Border of Date range used to limit a search result by the date specified by datetype. These two parameters (mindate, maxdate) must be used together to specify an arbitrary date range. The general date format is YYYY/MM/DD, and these variants are also allowed: YYYY, YYYY/MM.
            |MinDate    of System.DateTime
            ///Upper Border of Date range used to limit a search result by the date specified by datetype. These two parameters (mindate, maxdate) must be used together to specify an arbitrary date range. The general date format is YYYY/MM/DD, and these variants are also allowed: YYYY, YYYY/MM.
            |MaxDate    of System.DateTime

            static member makeQuery = function
                |Datetype   q   -> ("datetype"  , q )
                |RelDate    q   -> ("reldate"   , q |> string)
                |MinDate    q   -> ("mindate"   , q.ToString("YYYY/MM/DD"))
                |MaxDate    q   -> ("mindate"   , q.ToString("YYYY/MM/DD"))
                 
        type EntrezSearchParameters =
            |HistoryServerParameters    of EntrezSearchHistoryServerParams list
            |RetrievalParameters        of EntrezSearchRetrievalParams     list
            |DateParameters             of EntrezSearchDateParams          list

            static member makeQuery = function
                |HistoryServerParameters ql -> ql |> List.map EntrezSearchHistoryServerParams.makeQuery
                |RetrievalParameters     ql -> ql |> List.map EntrezSearchRetrievalParams    .makeQuery
                |DateParameters          ql -> ql |> List.map EntrezSearchDateParams         .makeQuery

        type EntrezSearchQuery = 
            {
                ///Database to search. Value must be a valid Entrez database name (default = pubmed).
                Db                  : string
                ///Entrez text query. All special characters must be URL encoded. Spaces may be replaced by '+' signs. For very long queries (more than several hundred characters long), consider using an HTTP POST call. See the PubMed or Entrez help for information about search field descriptions and tags. Search fields and tags are database specific.
                ///
                ///esearch.fcgi?db=pubmed&term=asthma
                Term                : string
                OptionalParameters  : EntrezSearchParameters list
            }

            static member makeRequest (q : EntrezSearchQuery) = 

                let optParams = 
                    q.OptionalParameters 
                    |> List.map EntrezSearchParameters.makeQuery
                    |> List.concat

                let db = q.Db

                let term =
                    q.Term.Replace(" ", "+")
                    
                Request.createUrl Get BaseUrls.eSearch
                |> Request.queryStringItem "db" db
                |> Request.queryStringItem "term" term
                |> Request.queryStringItems optParams

    ///DSL for constructing and executing eFetch queries
    ///
    ///Functions
    ///
    ///- Returns formatted data records for a list of input UIDs
    ///
    ///- Returns formatted data records for a set of UIDs stored on the Entrez History server
    module EntrezFetch =

        type EntrezFetchHistoryServerParams =
            ///Query key. This integer specifies which of the UID lists attached to the given Web Environment will be used as input to EFetch. Query keys are obtained from the output of previous ESearch, EPost or ELInk calls. The query_key parameter must be used in conjunction with WebEnv.
            |WebEnvironment of string
            ///Web Environment. This parameter specifies the Web Environment that contains the UID list to be provided as input to EFetch. Usually this WebEnv value is obtained from the output of a previous ESearch, EPost or ELink call. The WebEnv parameter must be used in conjunction with query_key.
            |QueryKey       of int

            static member makeQuery = function
                |WebEnvironment q -> ("WebEnv"      , q         )
                |QueryKey       q -> ("query_key"   , string q  )

        type EntrezFetchRetrievalParams =
            ///Sequential index of the first record to be retrieved (default=0, corresponding to the first record of the entire set). This parameter can be used in conjunction with retmax to download an arbitrary subset of records from the input set.
            |RetrievalStart of int
            ///Total number of records from the input set to be retrieved, up to a maximum of 10,000. Optionally, for a large set the value of retstart can be iterated while holding retmax constant, thereby downloading the entire set in batches of size retmax.
            |RetrievalMax   of int
            ///Retrieval type. This parameter specifies the record view returned, such as Abstract or MEDLINE from PubMed, or GenPept or FASTA from protein. Please see https://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.T._valid_values_of__retmode_and/?report=objectonly for a full list of allowed values for each database.
            |RetrievalType  of string
            ///Retrieval mode. This parameter specifies the data format of the records returned, such as plain text, HMTL or XML. See https://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.T._valid_values_of__retmode_and/?report=objectonly
            |RetrievalMode  of string

            static member makeQuery = function
                |RetrievalStart q -> ("retstart"    ,q |> string)
                |RetrievalMax   q -> ("retmax"      ,q |> string)
                |RetrievalType  q -> ("rettype"     ,q )
                |RetrievalMode  q -> ("retmode"     ,q )

        type EntrezFetchSequenceDatabaseOptions =
            |Plus   
            |Minus
                
            static member make = function
                |Plus   -> "1"
                |Minus  -> "2"

        type EntrezFetchComplexityOptions =
            |EntireBlob
            |Bioseq
            |MinimalBioseqSet
            |MinimalNucProt
            |MinimalPubSet

            static member make = function
                |EntireBlob         -> "0"
                |Bioseq             -> "1"
                |MinimalBioseqSet   -> "2"
                |MinimalNucProt     -> "3"
                |MinimalPubSet      -> "4"
            
        type EntrezFetchSequenceDatabaseParams =
            ///Strand of DNA to retrieve. Available values are "1" for the plus strand and "2" for the minus strand.
            |Strand     of EntrezFetchSequenceDatabaseOptions
            ///First sequence base to retrieve. The value should be the integer coordinate of the first desired base, with "1" representing the first base of the seqence.
            |SeqStart   of int
            ///Last sequence base to retrieve. The value should be the integer coordinate of the last desired base, with "1" representing the first base of the seqence.
            |SeqStop    of int
            ///Data content to return. Many sequence records are part of a larger data structure or "blob", and the complexity parameter determines how much of that blob to return. For example, an mRNA may be stored together with its protein product. The available values are as follows:
            ///Value of complexity	Data returned for each requested GI
            |Complexity of EntrezFetchComplexityOptions
            
            static member makeQuery = function
                |Strand      q -> ("strand"       , q |> EntrezFetchSequenceDatabaseOptions.make)
                |SeqStart    q -> ("seq_start"    , q |> string)
                |SeqStop     q -> ("seq_stop"     , q |> string)
                |Complexity  q -> ("complexity"   , q |> EntrezFetchComplexityOptions.make)
            

        type EntrezFetchParameters =
            |HistoryServerParameters    of EntrezFetchHistoryServerParams       list
            |RetrievalParameters        of EntrezFetchRetrievalParams           list
            |SequenceDatabaseParameters of EntrezFetchSequenceDatabaseParams    list

            static member makeQuery = function
                |HistoryServerParameters    ql -> ql |> List.map EntrezFetchHistoryServerParams     .makeQuery
                |RetrievalParameters        ql -> ql |> List.map EntrezFetchRetrievalParams         .makeQuery
                |SequenceDatabaseParameters ql -> ql |> List.map EntrezFetchSequenceDatabaseParams  .makeQuery

        type EntrezFetchQuery = 
            {
                ///Database from which to retrieve records. The value must be a valid Entrez database name (default = pubmed). Currently EFetch does not support all Entrez databases.
                Db                  : string
                ///UID list. Either a single UID or a comma-delimited list of UIDs may be provided. All of the UIDs must be from the database specified by db. There is no set maximum for the number of UIDs that can be passed to EFetch, but if more than about 200 UIDs are to be provided, the request should be made using the HTTP POST method.
                ///
                ///For sequence databases (nuccore, nucest, nucgss, popset, protein), the UID list may be a mixed list of GI numbers and accession.version identifiers.
                ///
                ///efetch.fcgi?db=pubmed&id=19393038,30242208,29453458
                ///efetch.fcgi?db=protein&id=15718680,NP_001098858.1,119703751
                ///Special note for sequence databases.
                ///
                ///NCBI is no longer assigning GI numbers to a growing number of new sequence records. As such, these records are not indexed in Entrez, and so cannot be retrieved using ESearch or ESummary, and have no Entrez links accessible by ELink. EFetch can retrieve these records by including their accession.version identifier in the id parameter.
                UIDs                : string list
                OptionalParameters  : EntrezFetchParameters list
            }

            static member makeRequest (q : EntrezFetchQuery) = 

                let optParams = 
                    q.OptionalParameters 
                    |> List.map EntrezFetchParameters.makeQuery
                    |> List.concat

                let db = q.Db

                let uIDs = 
                    match q.UIDs with
                    | [] -> ""
                    | _ -> q.UIDs |> String.concat ","
                
                Request.createUrl Get BaseUrls.eFetch
                |> Request.queryStringItem "db" db
                |> fun r -> 
                    match uIDs with
                    |"" -> r
                    |_ -> r |> Request.queryStringItem "id" uIDs
                |> Request.queryStringItems optParams

    ///DSL for constructing and executing ePost queries
    ///
    ///Endpoint Functions:
    ///
    /// - Uploads a list of UIDs to the Entrez History server
    ///
    /// - Appends a list of UIDs to an existing set of UID lists attached to a Web Environment
    module EntrezPost =
        
        type EntrezPostQuery =
            {
                ///Database containing the UIDs in the input list. The value must be a valid Entrez database name (default = pubmed).
                Db              : string
                ///UID list. Either a single UID or a comma-delimited list of UIDs may be provided. All of the UIDs must be from the database specified by db. There is no set maximum for the number of UIDs that can be passed to epost, but if more than about 200 UIDs are to be posted, the request should be made using the HTTP POST method.
                ///
                ///For sequence databases (nuccore, nucest, nucgss, popset, protein), the UID list may be a mixed list of GI numbers and accession.version identifiers.
                UIDs            : string list
                ///Web Environment. If provided, this parameter specifies the Web Environment that will receive the UID list sent by post. EPost will create a new query key associated with that Web Environment. Usually this WebEnv value is obtained from the output of a previous ESearch, EPost or ELink call. If no WebEnv parameter is provided, EPost will create a new Web Environment and post the UID list to query_key 1.
                WebEnvironment  : string option
            }

            static member makeRequest (q : EntrezPostQuery) = 

                let db = q.Db

                let uIDs = 
                    match q.UIDs with
                    | [] -> ""
                    | _ -> q.UIDs |> String.concat ","
                
                Request.createUrl Get BaseUrls.ePost
                |> Request.queryStringItem "db" db
                |> fun r -> 
                    match uIDs with
                    |"" -> r
                    |_ -> r |> Request.queryStringItem "id" uIDs
                |> fun r -> 
                    match q.WebEnvironment with
                    |None -> r
                    |Some w -> r |> Request.queryStringItem "WebEnv" w

    ///DSL for constructing and executing eSummary queries
    ///
    ///Functions
    ///
    /// - Returns document summaries (DocSums) for a list of input UIDs
    ///
    /// - Returns DocSums for a set of UIDs stored on the Entrez History server
    module EntrezSummary =
        
        type EntrezSummaryHistoryServerParams =
            ///Query key. This integer specifies which of the UID lists attached to the given Web Environment will be used as input to EFetch. Query keys are obtained from the output of previous ESearch, EPost or ELInk calls. The query_key parameter must be used in conjunction with WebEnv.
            |WebEnvironment of string
            ///Web Environment. This parameter specifies the Web Environment that contains the UID list to be provided as input to EFetch. Usually this WebEnv value is obtained from the output of a previous ESearch, EPost or ELink call. The WebEnv parameter must be used in conjunction with query_key.
            |QueryKey       of int

            static member makeQuery = function
                |WebEnvironment q -> ("WebEnv"      , q         )
                |QueryKey       q -> ("query_key"   , string q  )

        type EntrezSummaryRetrievalParams =
            ///Sequential index of the first record to be retrieved (default=0, corresponding to the first record of the entire set). This parameter can be used in conjunction with retmax to download an arbitrary subset of records from the input set.
            |RetrievalStart of int
            ///Total number of records from the input set to be retrieved, up to a maximum of 10,000. Optionally, for a large set the value of retstart can be iterated while holding retmax constant, thereby downloading the entire set in batches of size retmax.
            |RetrievalMax   of int
            ///Retrieval mode. Determines the format of the returned output. The default value is ‘xml’ for ESummary XML, but ‘json’ is also supported to return output in JSON format.
            |RetrievalMode  of RetrievalModeOptions
            ///Used to specify version 2.0 ESummary XML. The only supported value is ‘2.0’. When present, ESummary will return version 2.0 DocSum XML that is unique to each Entrez database and that often contains more data than the default DocSum XML.
            |Version        of string

            static member makeQuery = function
                |RetrievalStart q -> ("retstart"    ,q |> string)
                |RetrievalMax   q -> ("retmax"      ,q |> string)
                |RetrievalMode  q -> ("retmode"     ,q |> RetrievalModeOptions.make)
                |Version        q -> ("version"     ,q )    

        type EntrezSummaryParameters =
            |HistoryServerParameters    of EntrezSummaryHistoryServerParams list
            |RetrievalParameters        of EntrezSummaryRetrievalParams     list

            static member makeQuery = function
                |HistoryServerParameters    ql -> ql |> List.map EntrezSummaryHistoryServerParams     .makeQuery
                |RetrievalParameters        ql -> ql |> List.map EntrezSummaryRetrievalParams         .makeQuery

        type EntrezSummaryQuery =
            {
                ///Database containing the UIDs in the input list. The value must be a valid Entrez database name (default = pubmed).
                Db                  : string
                ///UID list. Either a single UID or a comma-delimited list of UIDs may be provided. All of the UIDs must be from the database specified by db. There is no set maximum for the number of UIDs that can be passed to epost, but if more than about 200 UIDs are to be posted, the request should be made using the HTTP POST method.
                ///
                ///For sequence databases (nuccore, nucest, nucgss, popset, protein), the UID list may be a mixed list of GI numbers and accession.version identifiers.
                UIDs                : string list
                ///Web Environment. If provided, this parameter specifies the Web Environment that will receive the UID list sent by post. EPost will create a new query key associated with that Web Environment. Usually this WebEnv value is obtained from the output of a previous ESearch, EPost or ELink call. If no WebEnv parameter is provided, EPost will create a new Web Environment and post the UID list to query_key 1.
                OptionalParameters  : EntrezSummaryParameters list
            }

            static member makeRequest (q : EntrezSummaryQuery) = 

                let db = q.Db

                let uIDs = 
                    match q.UIDs with
                    | [] -> ""
                    | _ -> q.UIDs |> String.concat ","
                       
                let optParams = 
                    q.OptionalParameters 
                    |> List.map EntrezSummaryParameters.makeQuery
                    |> List.concat

                Request.createUrl Get BaseUrls.eSummary
                |> Request.queryStringItem "db" db
                |> fun r -> 
                    match uIDs with
                    |"" -> r
                    |_ -> r |> Request.queryStringItem "id" uIDs
