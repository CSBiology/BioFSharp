#r "netstandard"
#I @"../../packages/biodb/Hopac/lib/netstandard2.0/"
#r "Hopac.dll"
#r "Hopac.Core.dll"
#r @"../../packages/biodb/Http.fs/lib/netstandard2.0/Httpfs.dll"
#r @"../../packages/biodb/Newtonsoft.Json/lib/netstandard2.0/Newtonsoft.Json.dll"

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
                    EntrezSearchRetrievalParams.RetrievalMode RetrievalModeOptions.JSON
                ]
            ]
    }
    |> EntrezSearchQuery.makeRequest

let eSearchRequest =

    job {
      use! response = getResponse eSearchQuery // disposed at the end of async, don't
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

let eISearchResponse = 
    eSearchRequest 
    |> run
