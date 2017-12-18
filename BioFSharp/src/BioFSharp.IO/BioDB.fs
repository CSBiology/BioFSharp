namespace BioFSharp.IO

open System
open FSharpAux
open FSharpAux.IO

open Newtonsoft.Json
open FSharp.Data
open FSharp.Data.HttpRequestHeaders


module BioDB =
    


    module ServiceUrls =

        let loginUrl baseUrl = 
            sprintf "%s/api/account/login" baseUrl

        let getItemTypes baseUrl = 
            sprintf "%s/api/biodb/itemtypes" baseUrl

            

    module DataModel =

        type LoginModel = {
            [<JsonProperty("UserName")>]
            UserName : string;
            [<JsonProperty("Password")>]
            Password   : string
        }    
 
        let createLoginModel user pass =
            {UserName = user; Password = pass}
        
        type ItemType = {
            Id : int
            Name : string
            Description : string
        }
        
        let createItemType id name description =
            {Id = id; Name = name; Description = description}

  
  
    let login baseUrl user pass =         

        let user = DataModel.createLoginModel user pass |> JsonConvert.SerializeObject
        let cookieString = Http.Request(ServiceUrls.loginUrl baseUrl,headers = [ ContentType HttpContentTypes.Json  ],body =  HttpRequestBody.TextRequest user, silentHttpErrors = true).Headers.["Set-Cookie"]
        let cookie = new System.Net.CookieContainer()
        cookie.SetCookies(new Uri(baseUrl) ,cookieString)
        cookie
       




    let getItemTypes baseUrl login  =    
        ServiceUrls.getItemTypes baseUrl 
        |> WebApiClient.httpResponseToJSON<DataModel.ItemType seq> login 
