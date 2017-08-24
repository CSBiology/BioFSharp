namespace BioFSharp.BioDB

open SwaggerProvider
 
open System.Reflection

// Swagger schema downloaded from http://www.ebi.ac.uk/proteins/api/swagger.json
// Swagger tools:
// http://editor.swagger.io
// http://playground.apistudio.io




module EbiAPI =

//    let assembly = Assembly.GetExecutingAssembly()
//    let resourceName = "ebiProteinsAPIswagger.json"
//    let resourceFileName = assembly.GetManifestResourceInfo(resourceName).FileName

    let [<Literal>]schemaURL = "http://www.ebi.ac.uk/proteins/api/swagger.json"


    type ProteinsAPIschema = SwaggerProvider<schemaURL>
    
    let proteinsAPI = ProteinsAPIschema()
    
    