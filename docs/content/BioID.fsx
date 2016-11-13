(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
BioFSharp
======================


*)
#r "BioFSharp.dll"
open BioFSharp



BioID.parseCreId "sp| Cre01.g121121.1.1 acyl-CoA thioesterase 4 ALS=MGI:2159621;Acot4 DBV=UP"


let fh = 
    fromString "UniqueIdentifier any description DID=DisplayId ALS=Alias1;Alias2 DBV=DataBaseVersion TOU=TypeOfUniqueIdentifier SV=SequenceVersion OS=OrganismName PE=ProteinExistence GN=GeneName PN=ProteinName"
toString fh

