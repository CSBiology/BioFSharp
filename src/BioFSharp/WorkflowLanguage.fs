namespace BioFSharp

module WorkflowLanguage = 

    module Definition = 
       
        type IOType = 
            | File of System.IO.FileInfo
            | Files of System.IO.FileInfo list 
    
        type Operation<'a>  = {
            Id               : System.Guid
            Name             : string
            Operator         : string
            Input            : IOType
            Output           : IOType
            Parameters       : 'a       
            }
            
        let createProcessDescription id name operator input output parameters = { 
            Id = id; Name = name; Operator = operator; Input = input; Output = output; Parameters = parameters }
        
        type Workflow<'a> = {
            Processes : Operation<'a> list 
            }

        let createWorkflow processes = 
            {Processes = processes}
