(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"
#r "BioFSharp.BioDB.dll"
//#r "SwaggerProvider.Runtime.dll"

open BioFSharp.BioDB


(**
Accessing online databases
==============================
BioFSharp contains a set of reader that facilitat the access to different biological online resources. This documentation aims to give an introduction for them.


<a name="UniProt"></a>
UniProt's Proteins REST API online access
-----------------------------------------
*)


(**
The Proteins REST API provides access to key biological data from UniProt and data from Large Scale Studies data mapped to UniProt. 
The services provide sequence feature annotations from UniProtKB, variation data from UniProtKB and mapped from Large Scale data sources (1000 Genomes, ExAC and COSMIC), 
proteomics data mapped from Large Scale sources (PeptideAtlas, MaxQB and EPD) and genome coordinate mappings

*)



//let a  = 
//    EbiAPI.getProteinFeaturesById "Q64446"

EbiAPI.UniProteinDB.getSeqFeatureByAccession "Q64446"


(**


<hr>


<a name="BioDB"></a>
BioDB
------------------------------
This documentation is not yet here. Hopefully it will be soon =)

*)






