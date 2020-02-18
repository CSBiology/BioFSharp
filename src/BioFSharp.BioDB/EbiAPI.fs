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

    type ProteinsAPIschema = SwaggerClientProvider<schemaURL, PreferAsync=true> // ,EmbeddedResource=("BioFSharp.dll,ebiProteinsAPIswagger.json")>
    
    let private proteinsAPI = ProteinsAPIschema.Client()

    /// Access to the uniprot protein data base
    type UniProteinDB () =

       /// Get antigen by UniProt accession
       static member getAntigen(accession:string) = 

            proteinsAPI.GetByAccession(accession)

       /// Get genomic coordinates by UniProt accession
       static member getGenomicCoordinates(accession:string) = 

            proteinsAPI.GetByAccession1(accession) 
            
       /// Get UniProt protein sequence features by accession
       static member getProteinSeqFeature
            (
                accession   : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?categories : string[],
                [<Optional;DefaultParameterValue(null)>] 
                ?types      : string[]
            ) = 
            let categories = defaultArg categories [||]
            let types = defaultArg types [||]
            proteinsAPI.GetByAccession2(accession,categories,types)

       /// Get UniProt entry by accession
       static member getUniProtEntry(accession:string) = 

            proteinsAPI.GetByAccession3(accession) 

       /// Get proteomics peptide mapped to UniProt by accession
       static member getProteomicsPeptide(accession:string) = 

            proteinsAPI.GetByAccession4(accession)

//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//!!!!!!!!!! These functions currently dont work with dotnet 2.1.500. The project does not build on  !!!!!!!!!!!!!!
//!!!!!!!!!! this sdk anymore when these functions are included.                                     !!!!!!!!!!!!!!
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       /// Search protein sequence features in UniProt
       static member searchProteinSeqFeature
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
            let accession = defaultArg accession null
            let reviewed = defaultArg reviewed null
            let gene = defaultArg gene null
            let protein = defaultArg protein null
            let organism = defaultArg organism null
            let taxid = defaultArg taxid null
            let categories = defaultArg categories null
            let types = defaultArg types null
            proteinsAPI.Search2(offset,size,accession,reviewed,gene,protein,organism,taxid,categories,types)


       /// Search antigen in UniProt
       static member searchAntigens
            (
                [<Optional;DefaultParameterValue(null)>] 
                ?offset             : int,
                [<Optional;DefaultParameterValue(null)>] 
                ?size               : int,
                [<Optional;DefaultParameterValue(null)>] 
                ?accession          : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?antigenSequence    : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?antigenID          : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?ensemblIDs         : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?matchscore         : int

            ) = 
            let accession = defaultArg accession null
            let antigenSequence = defaultArg antigenSequence null
            let antigenID = defaultArg antigenID null
            let ensemblIDs = defaultArg ensemblIDs null
            
            proteinsAPI.Search(offset,size,accession,antigenSequence,antigenID,ensemblIDs,matchscore)


       /// Search genomic coordinates in UniProt
       static member searchGenomicCoordinates
            (
                [<Optional;DefaultParameterValue(null)>] 
                ?offset     : int,
                [<Optional;DefaultParameterValue(null)>] 
                ?size       : int,
                [<Optional;DefaultParameterValue(null)>] 
                ?accession  : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?chromosome : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?ensembl    : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?gene       : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?protein    : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?taxid      : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?location   : string

            ) = 

            let accession = defaultArg accession null
            let chromosome = defaultArg chromosome null
            let ensembl = defaultArg ensembl null
            let gene = defaultArg gene null
            let protein = defaultArg protein null
            let taxid = defaultArg taxid null
            let location = defaultArg location null
            
            proteinsAPI.Search1(offset,size,accession,chromosome,ensembl,gene,protein,taxid,location)


       /// Search entries in UniProt
       static member searchEntries
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
                ?isoforms   : int,
                [<Optional;DefaultParameterValue(null)>] 
                ?goterms    : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?keywords   : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?ec         : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?gene       : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?protein    : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?organism   : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?taxid      : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?pubmed     : string

            ) = 

            let accession = defaultArg accession null
            let reviewed = defaultArg reviewed null
            let goterms = defaultArg goterms null
            let keywords = defaultArg keywords null
            let ec = defaultArg ec null
            let gene = defaultArg gene null
            let protein = defaultArg protein null
            let organism  = defaultArg organism  null
            let taxid = defaultArg taxid null
            let pubmed = defaultArg pubmed null
            
            proteinsAPI.Search3(offset,size,accession,reviewed,isoforms,goterms,keywords,ec,gene,protein,organism,taxid,pubmed)

       /// Search proteomes in UniProt
       static member searchProteomes
            (
                [<Optional;DefaultParameterValue(null)>] 
                ?offset         : int,
                [<Optional;DefaultParameterValue(null)>] 
                ?size           : int,
                [<Optional;DefaultParameterValue(null)>] 
                ?upid      : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?name   : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?taxid   : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?keyword    : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?xref           : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?genomeAcc      : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?isRefProteome  : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?isRedundant    : string

            ) = 

            let upid = defaultArg upid null
            let name = defaultArg name null
            let taxid = defaultArg taxid null
            let keyword = defaultArg keyword null
            let xref = defaultArg xref null
            let genomeAcc  = defaultArg genomeAcc  null
            let isRefProteome = defaultArg isRefProteome null
            let isRedundant = defaultArg isRedundant null
            
            proteinsAPI.Search4(offset,size,upid,name,taxid,keyword,xref,genomeAcc,isRefProteome,isRedundant)
    
       /// Search ProteomicsPeptides in UniProt
       static member searchProteomicsPeptides
            (
                [<Optional;DefaultParameterValue(null)>] 
                ?offset         : int,
                [<Optional;DefaultParameterValue(null)>] 
                ?size           : int,
                [<Optional;DefaultParameterValue(null)>] 
                ?accession      : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?taxid   : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?upid      : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?datasource   : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?peptide    : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?unique           : string

            ) = 

            let accession  = defaultArg accession  null
            let taxid = defaultArg taxid null
            let upid = defaultArg upid null
            let datasource = defaultArg datasource null
            let peptide = defaultArg peptide null
            let unique = defaultArg unique null
            
            proteinsAPI.Search5(offset,size,accession,taxid,upid,datasource,peptide,unique)    


       /// Search natural variants in UniProt
       static member searchNaturalVariants
            (
                [<Optional;DefaultParameterValue(null)>] 
                ?offset         : int,
                [<Optional;DefaultParameterValue(null)>] 
                ?size           : int,
                [<Optional;DefaultParameterValue(null)>] 
                ?accession      : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?taxid   : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?sourcetype      : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?consequencetype   : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?wildtype    : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?alternativesequence      : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?location      : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?disease      : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?omim      : string,
                [<Optional;DefaultParameterValue(null)>] 
                ?evidence           : string

            ) = 

            let accession  = defaultArg accession  null
            let taxid = defaultArg taxid null
            let sourcetype = defaultArg sourcetype null
            let consequencetype = defaultArg consequencetype null
            let wildtype = defaultArg wildtype null
            let alternativesequence = defaultArg alternativesequence null
            let location = defaultArg location null
            let disease = defaultArg disease null
            let omim = defaultArg omim null
            let evidence = defaultArg evidence null
            
            proteinsAPI.Search6(sourcetype,consequencetype,wildtype,alternativesequence,location,accession,disease,omim,evidence,taxid,offset,size) 