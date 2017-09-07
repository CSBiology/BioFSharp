namespace BioFSharp.BioDB

open SwaggerProvider
 
open System.Reflection

// Swagger schema downloaded from http://www.ebi.ac.uk/proteins/api/swagger.json
// Swagger tools:
// http://editor.swagger.io
// http://playground.apistudio.io




module EbiAPI =

    let [<Literal>]schemaURL = __SOURCE_DIRECTORY__ + "/Resources/ebiProteinsAPIswagger.json" //"http://www.ebi.ac.uk/proteins/api/swagger.json"

    
    type ProteinsAPIschema = SwaggerProvider<schemaURL> // ,EmbeddedResource=("BioFSharp.dll,ebiProteinsAPIswagger.json")>
    
    let proteinsAPI = ProteinsAPIschema()
    
    let getProteinFeaturesById (id:string) = proteinsAPI.GetByAccession2 id

    
