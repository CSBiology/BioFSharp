namespace BioFSharp.BioDB


module Entrez =

    open Hopac
    open HttpFs
    open HttpFs.Client
    open Newtonsoft.Json
    
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
    module EntrezInfo =
        
        type EntrezInfoParameters =
            |Db             of string
            |Version        of string
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
                    


