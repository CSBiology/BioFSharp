namespace BioFSharp.IO

module WorkflowLanguage = 
    
    let operationToJSon parameters =
        Newtonsoft.Json.JsonConvert.SerializeObject parameters

    let operationOfJSon<'a> json = 
        Newtonsoft.Json.JsonConvert.DeserializeObject<BioFSharp.WorkflowLanguage.Definition.Operation<'a>>(json)
