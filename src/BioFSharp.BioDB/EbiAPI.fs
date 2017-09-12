namespace BioFSharp.BioDB

open SwaggerProvider
 
open System.Reflection

// Swagger schema downloaded from http://www.ebi.ac.uk/proteins/api/swagger.json
// Swagger tools:
// http://editor.swagger.io
// http://playground.apistudio.io


open System.Runtime.InteropServices

module EbiAPI =

    let [<Literal>] private schemaURL = __SOURCE_DIRECTORY__ + "/Resources/ebiProteinsAPIswagger.json" //"http://www.ebi.ac.uk/proteins/api/swagger.json"

    type ProteinsAPIschema = SwaggerProvider<schemaURL> // ,EmbeddedResource=("BioFSharp.dll,ebiProteinsAPIswagger.json")>
    
    let private proteinsAPI = ProteinsAPIschema()

    
    /// Access to the uniprot protein data base
    type UniProteinDB () =
       
       /// Get UniProt protein sequence features by accession
       static member getSeqFeatureByAccession
            (
                accession   : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?categories : string[],
                [<Optional;DefaultParameterValue(null)>] 
                ?types : string[]
            ) = 
            let categories = defaultArg categories [||]
            let types = defaultArg types [||]
            proteinsAPI.GetByAccession2(accession,categories,types)
        

       /// Search protein sequence features in UniProt
       static member searchFeature
            (
                [<Optional;DefaultParameterValue(null)>] 
                ?offset     : int,
                [<Optional;DefaultParameterValue(null)>] 
                ?size       : int,
                [<Optional;DefaultParameterValue(null)>] 
                ?accession  : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?reviewed   : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?gene       : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?protein    : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?organism   : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?taxid      : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?categories : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?types      : string

            ) = 
            //let offset' = defaultArg offset 0
            //let size' = defaultArg size 0
            let accession = defaultArg accession ""
            let reviewed = defaultArg reviewed ""
            let gene = defaultArg gene ""
            let protein = defaultArg protein ""
            let organism = defaultArg organism ""
            let taxid = defaultArg taxid ""
            let categories = defaultArg categories ""
            let types = defaultArg types ""
            proteinsAPI.Search2(offset,size,accession,reviewed,gene,protein,organism,taxid,categories,types)


       /// Search protein sequence features in UniProt
       static member searchFeature2
            (
                [<Optional;DefaultParameterValue(null)>] 
                ?offset     : int,
                [<Optional;DefaultParameterValue(null)>] 
                ?size       : int,
                [<Optional;DefaultParameterValue(null)>] 
                ?accession  : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?reviewed   : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?gene       : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?protein    : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?organism   : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?taxid      : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?categories : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?types      : string

            ) = 
            //let offset' = defaultArg offset 0
            //let size' = defaultArg size 0
            let accession = defaultArg accession ""
            let reviewed = defaultArg reviewed ""
            let gene = defaultArg gene ""
            let protein = defaultArg protein ""
            let organism = defaultArg organism ""
            let taxid = defaultArg taxid ""
            let categories = defaultArg categories ""
            let types = defaultArg types ""
            
            proteinsAPI.Search2(offset,size,accession,reviewed,gene,protein,organism,taxid,categories,types)
            

    //let getProteinFeaturesById (id:string) = proteinsAPI.GetByAccession2 id


    
    
