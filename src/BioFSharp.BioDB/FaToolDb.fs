namespace BioFSharp.BioDB
open FSharp.Data.TypeProviders

///Tools for the odata service FaTools available at http://iomiqsweb1.bio.uni-kl.de/Services/FaToolDbDataService.svc
//all credit for the database goes to https://github.com/goedels and https://github.com/luede
module FaToolDb =
    
    //let[<Literal>]url = "http://iomiqsweb1.bio.uni-kl.de/Services/FaToolDbDataService.svc"

    //server adress the service migrated to
    let [<Literal>]url = "http://hyperweb.bio.uni-kl.de:8015/Services/FaToolDbDataService.svc"

    type FaToolData = 
        ODataService<url>

    let context = FaToolData.GetDataContext()
    let fullContext = FaToolData.ServiceTypes.FaToolDbEntities()

    ///Represents the different ontologies used in the database
    type OntologyId = 
        | SubCellularLocalisationOntology
        | FaToolObjectAttributeOntology
        | HomologyOntology
        | GeneOntology 
        | MapManOntology
        | NCBIOrganismalTaxonomyOntology
        | PSIMSOntology
        | UnitOntology

        static member toString = function
            | SubCellularLocalisationOntology   -> "FaToolSubCellLoc"
            | FaToolObjectAttributeOntology     -> "FTOATTR"
            | HomologyOntology                  -> "Homology"
            | GeneOntology                      -> "GO"
            | MapManOntology                    -> "MapMan"
            | NCBIOrganismalTaxonomyOntology    -> "NcbiTaxonomy"
            | PSIMSOntology                     -> "PSI-MS"
            | UnitOntology                      -> "UO"
               
        static member convert = OntologyId.toString >> box

    ///Contains preconstructed query functions for the FaTool Database
    module Queries =
    
        ///Wrapper for the result of an ontologyTerm query
        type OntologyTermQueryResult = 
            {
                Id: string;
                OntologyGroups: (string*string) list
            }

            member this.isEmpty =
                match (this.Id,this.OntologyGroups) with 
                |("",[]) -> true
                |_ -> false

            member this.isNotIdentified = 
                match (this.Id,this.OntologyGroups) with 
                |(_,[]) -> true
                |_ -> false
        
        ///returns ontology terms and term ids the selected ontology for a query gene
        let getOntologyterms (ontologyName:OntologyId) (identifier:string) = 
            let temp = 
                (query 
                    {
                    for ann in fullContext.Annotations.Expand("Protein,Term") do
                    where (ann.Protein.Name = identifier && ann.Term.Ontology.ID=(OntologyId.toString ontologyName))
                    select ann
                    } |> Seq.toList |> List.map (fun p -> p.Protein.Name,p.Term.ID,p.Term.Name))
        
            let (name,ontologyGroups) = temp |> List.fold (fun acc (name,termId,termName) -> (name,(termId,termName)::snd acc)) ("",[])
            {
                Id = name;
                OntologyGroups = ontologyGroups;
            }


        ///returns a map mapping the ontology results to the query ids with an option to change the formatting of the id string
        let createOntologyMapFromQueryResults (stringFormattingConverter: string -> string) (input: OntologyTermQueryResult []) =
            input 
            |> Array.filter (fun x -> not x.isEmpty)
            |> Array.map (fun x -> ((stringFormattingConverter x.Id),x.OntologyGroups |> Array.ofList))
            |> Map.ofArray


        ///returns synonyms for a query gene
        let getSynonyms (identifier:string) = 
            query 
                {
                    for syn in fullContext.Synonyms.Expand("Protein") do
                        where (syn.Protein.Name = identifier)
                        select syn
                    } |> Seq.toList |> List.map (fun x -> x.Protein.Name, x.SynonymValue)
