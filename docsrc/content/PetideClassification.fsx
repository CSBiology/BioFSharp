(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I @"../../bin/BioFSharp/net47/"
#I @"../../bin/BioFSharp.BioDB/net45/"
#I @"../../bin/BioFSharp.ImgP/net47"
#I @"../../bin/BioFSharp.IO/net47/"
#I @"../../bin/BioFSharp.Parallel/net47/"
#I @"../../bin/BioFSharp.Stats/net47/"
#I @"../../bin/BioFSharp.Vis/net47/"
#r @"../../lib/Formatting/FSharp.Plotly.dll"
#r "netstandard.dll"
#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"
#r "FSharpAux.dll"
open FSharp.Plotly
//#r "FSharpAux.IO.dll"


(**
<table class="HeadAPI">
<td class="Head"><h1>FastA format</h1></td>
<td class="API">
    <a id="APILink" href="https://csbiology.github.io/BioFSharp/reference/biofsharp-io-fasta.html" >&#128194;View module documentation</a>
</td>
</table>
One of the various biology-associated file formats that can be manipulated using BioFSharp is the FastA format.
The FastA format can be used to represent sequences of amino acids or nucleotides written in single-letter code.
<br>
\>sp|P19532| ribosomal protein L20 GN=rpl20 PE=rpl20.p01
MTRVKRGNVSRKRHKKILNMSKGFRGAASTLFRTANQQNMKALRYSYRNRRQKKRDFRRM
WITRVNSAVRRYGLNYSEFMNYLKTHKIQLNRKVIAQLSICDPEAFMQLLLF*
<br>
One sequence constists of two parts: The first line (Header) starting with a ">" is followed by a sequence identification code which should represent a unique description of the sequence. 
Subsequent lines contain the sequence itself, which is separated into chunks of 60 to 80 characters per line.
For further information about the format please visit [NCBI - FASTA](https://blast.ncbi.nlm.nih.gov/Blast.cgi?CMD=Web&PAGE_TYPE=BlastDocs&DOC_TYPE=BlastHelp).

Reading FastA files
-------------------
*)

open BioFSharp
open BioFSharp.IO
open FSharpAux

//let fileDir = __SOURCE_DIRECTORY__ + "/data/"  //FASTAExample1.fasta"


let fileDir = @"D:\OneDrive\Projects\FastA proper\data\chlamydomonas reinhardtii\"



let getProteinModelInfo str =     
    let m = System.Text.RegularExpressions.Regex.Match(str,@"Cre(-?[\d]*)\.g([\d]*)\.t([\d]*)\.([\d]*)") 
    if m.Success then 
        let tmp = 
            [ for g in m.Groups -> g.Value.Trim() ] 
            |> List.toArray
        PeptideClassification.createProteinModelInfo tmp.[0] (int tmp.[1]) PeptideClassification.StrandDirection.Forward (int tmp.[2]) (int tmp.[3]) Seq.empty Seq.empty        
        |> Some
    else
        None

//getProteinModelInfo "Cre06.g1000.t1.1"
  
//reads from file to an array of FastaItems.
let sequences = 
    //fileDir + "Chlamy_Cp.fastA"
    fileDir + "Chlamy_NCBI_Plastids.fasta"
    |> FastA.fromFile BioArray.ofAminoAcidString
    |> Seq.map (fun fi -> 
        match getProteinModelInfo fi.Header with
        | None -> None
        | Some info -> 
            PeptideClassification.createProteinModel info fi.Sequence
            |> Some                    
        )
    |> Seq.toArray
    

let ppRelationModel =
    let digest sequence =
        Digestion.BioArray.digest (Digestion.Table.getProteaseBy "Trypsin") 0 sequence
        |> Digestion.BioArray.concernMissCleavages 0 3
        |> Seq.map (fun p -> p.PepSequence |> List.toArray) // TODO not |> List.toArray
    
    PeptideClassification.createPeptideProteinRelation digest sequences
    
let spliceVariantCount = PeptideClassification.createLocusSpliceVariantCount ppRelationModel


let classified = 
    ppRelationModel.GetArrayOfKeys
    |> Array.map (fun peptide ->  
        let proteinInfo = (ppRelationModel.TryGetByKey peptide).Value
        PeptideClassification.classify spliceVariantCount (peptide, (ppRelationModel.TryGetByKey peptide).Value))


//let sequence = BioArray.ofAminoAcidString "IDEKTLASLIDEKTLASL"
//let sequence' = BioArray.ofAminoAcidSymbolString "IDEKTLASL"

//Digestion.BioArray.digest (Digestion.Table.getProteaseBy "Trypsin") 0 sequence
//|> Digestion.BioArray.concernMissCleavages 0 3

//Digestion.CleavageScore.calculateCleavageScore sequence'

