#r "netstandard"
#I @"../../packages/biodb/Hopac/lib/netstandard2.0/"
#r "Hopac.dll"
#r "Hopac.Core.dll"
#r @"../../packages/biodb/Http.fs/lib/netstandard2.0/Httpfs.dll"
#r @"../../packages/biodb/Newtonsoft.Json/lib/netstandard2.0/Newtonsoft.Json.dll"
#r @"../../packages/biodb\FSharp.Data.TypeProviders\lib\net40\FSharp.Data.TypeProviders.dll"
#load "Entrez.fs"

open BioFSharp.BioDB.Entrez
open Hopac
open HttpFs
open HttpFs.Client


//=============================== eInfo Tests ======================================
open EntrezInfo

let eInfoQuery =
    {OptionalParameters = 
        [
            EntrezInfoParameters.RetrievalMode RetrievalModeOptions.JSON
            EntrezInfoParameters.Db "sra"
        ]
    }
    |> EntrezInfoQuery.makeRequest

let eInfoRequest =

    job {
      use! response = getResponse eInfoQuery // disposed at the end of async, don't
                                          // fetch outside async body
      // the above doesn't download the response, so you'll have to do that:
      let! bodyStr = Response.readBodyAsString response
      // OR:
      //let! bodyBs = Response.readBodyAsBytes

      // remember HttpFs doesn't buffer the stream (how would we know if we're
      // downloading 3GiB?), so once you use one of the above methods, you can't do it
      // again, but have to buffer/stash it yourself somewhere.
      return bodyStr
    }

let eInfoResponse = eInfoRequest |> run

//=============================== eSearch Tests ======================================
open EntrezSearch

let eSearchQuery =
    {   
        Db = "sra"
        Term = "SRX245325"
        OptionalParameters = 
            [
                RetrievalParameters [
                    EntrezSearchRetrievalParams.RetrievalMode RetrievalModeOptions.XML
                ]
                HistoryServerParameters [
                    EntrezSearchHistoryServerParams.UseHistory
                ]
            ]
    }
    |> EntrezSearchQuery.makeRequest

let eSearchRequest =

    job {
      use! response = getResponse eSearchQuery
      let! bodyStr = Response.readBodyAsString response
      return bodyStr
    }

let eISearchResponse = 
    eSearchRequest 
    |> run

open System.Xml

let eSearchDoc =
    eISearchResponse
    |> fun x -> 
        let doc = XmlDocument()
        doc.LoadXml(x)
        doc

let queryKey = eSearchDoc.SelectSingleNode("eSearchResult/QueryKey").InnerText
let webenv = eSearchDoc.SelectSingleNode("eSearchResult/WebEnv").InnerText


//=============================== eFetch Tests ======================================
open EntrezFetch

let eFetchQuery =
    {   
        Db = "sra"
        UIDs = []
        OptionalParameters = 
            [
                HistoryServerParameters [
                    EntrezFetchHistoryServerParams.QueryKey (queryKey |> int )
                    EntrezFetchHistoryServerParams.WebEnvironment webenv
                ]
                RetrievalParameters [
                    EntrezFetchRetrievalParams.RetrievalType "RunInfo"
                ]
            ]
    }
    |> EntrezFetchQuery.makeRequest

let eFetchRequest =

    job {
      use! response = getResponse eFetchQuery
      let! bodyStr = Response.readBodyAsString response
      return bodyStr
    }

let eFetchResponse = 
    eFetchRequest 
    |> run

open System.Xml

let xmlResponse = System.Xml.XmlDocument()

xmlResponse.LoadXml(eFetchResponse)

xmlResponse.SelectNodes ("EXPERIMENT_PACKAGE_SET/EXPERIMENT_PACKAGE/RUN_SET/RUN")
|> Seq.cast<XmlNode>
|> List.ofSeq
|> List.map 
    (
        fun node -> 
            node.Attributes.["accession"].Value
    )


//=============================== ePost Tests ======================================
open EntrezPost

let ePostQuery =
    let r = 
        {
            Db = "sra"
            UIDs = ["336327"]
            WebEnvironment = Some webenv
        }
    r |> EntrezPostQuery.makeRequest

let ePostRequest =

    job {
      use! response = getResponse ePostQuery 
      let! bodyStr = Response.readBodyAsString response
      return bodyStr
    }

let eIPostResponse = ePostRequest |> run

//=============================== eSummary Tests ======================================
open EntrezSummary

let eSummaryQuery =
    let r = 
        {
            Db = "sra"
            UIDs = ["336327";"336326"]
            OptionalParameters = [
                HistoryServerParameters [
                    EntrezSummaryHistoryServerParams.WebEnvironment webenv
                ] 
            ]
        }
    r |> EntrezSummaryQuery.makeRequest

let eSummaryRequest =

    job {
      use! response = getResponse eSummaryQuery
      let! bodyStr = Response.readBodyAsString response
      return bodyStr
    }

let eSummaryResponse = eSummaryRequest |> run


//=============================== eLink Tests ======================================
open EntrezLink

let eLinkQuery =
    let r = 
        {
            SourceDb = "sra"
            TargetDb = "gds"
            UIDs     = ["336327";"336326"]
            OptionalParameters = [
                HistoryServerParameters [
                    EntrezLinkHistoryServerParams.WebEnvironment webenv
                ] 
            ]
            LinkCommand = EntrezLinkCommandOptions.Neighbor
        }
    r |> EntrezLinkQuery.makeRequest

let eLinkRequest =

    job {
      use! response = getResponse eLinkQuery
      let! bodyStr = Response.readBodyAsString response
      return bodyStr
    }

let eLinkResponse = eLinkRequest |> run

//=============================== eLink Tests ======================================
open EntrezGQuery

let eGQuery =
    let r = 
        {
            Term = "SRX245325"
        }
    r |> EntrezGQueryQuery.makeRequest

let eGQueryRequest =

    job {
      use! response = getResponse eGQuery
      let! bodyStr = Response.readBodyAsString response
      return bodyStr
    }

let eGQueryResponse = eGQueryRequest |> run

//=============================== eLink Tests ======================================
open EntrezSpell

let eSpellQuery =
    let r = 
        {
            Db   = "sra"
            Term = "asthmaa+OR+alergies"
        }
    r |> EntrezSpellQuery.makeRequest

let eSpellRequest =

    job {
      use! response = getResponse eSpellQuery
      let! bodyStr = Response.readBodyAsString response
      return bodyStr
    }

let eSpellResponse = eSpellRequest |> run

//=============================== eLink Tests ======================================
open EntrezCitMatch

let eCitMatchQuery =
    let r = 
        {
            Db    = "pubmed"
            BData = "proc+natl+acad+sci+u+s+a|1991|88|3248|mann+bj|Art1|%0Dscience|1987|235|182|palmenberg+ac|Art2|"
        }
    r |> EntrezCitMatchQuery.makeRequest

let eCitMatchRequest =

    job {
      use! response = getResponse eCitMatchQuery
      let! bodyStr = Response.readBodyAsString response
      return bodyStr
    }

let eCitMatchResponse = eCitMatchRequest |> run