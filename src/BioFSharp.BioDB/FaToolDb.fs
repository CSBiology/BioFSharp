namespace BioFSharp.BioDB
open FSharp.Data.TypeProviders
open System.Data.Services.Client

///Tools for the odata service FaTools available at http://iomiqsweb1.bio.uni-kl.de/Services/FaToolDbDataService.svc
//all credit for the database goes to https://github.com/goedels and https://github.com/luede
module FaToolDb =
    
    let[<Literal>]url = "http://iomiqsweb1.bio.uni-kl.de/Services/FaToolDbDataService.svc"

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
            | HomologyOntology                  -> "GO"
            | GeneOntology                      -> "Homology"
            | MapManOntology                    -> "MapMan"
            | NCBIOrganismalTaxonomyOntology    -> "NcbiTaxonomy"
            | PSIMSOntology                     -> "PSI-MS"
            | UnitOntology                      -> "UO"
               
        static member convert = OntologyId.toString >> box

    ///Contains preconstructed query functions for the FaTool Database
    module Queries =
    
        ///Wrapper for the result of an ontologyTerm query
        type ontologyTermQueryResult = 
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
        
        ///returns ontology term and ontology id of the selected ontology
        let private getOntologyterms (ontologyName:OntologyId) (locusIdentifier:string) = 
            let temp = 
                (query 
                    {
                    for ann in fullContext.Annotations.Expand("Protein,Term") do
                    where (ann.Protein.Name = locusIdentifier && ann.Term.Ontology.ID=(OntologyId.toString ontologyName))
                    select ann
                    } |> Seq.toList |> List.map (fun p -> p.Protein.Name,p.Term.ID,p.Term.Name))
        
            let (name,ontologyGroups) = temp |> List.fold (fun acc (name,termId,termName) -> (name,(termId,termName)::snd acc)) ("",[])
            {
                Id = name;
                OntologyGroups = ontologyGroups;
            }



