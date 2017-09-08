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
        

    //let getProteinFeaturesById (id:string) = proteinsAPI.GetByAccession2 id


    
    
