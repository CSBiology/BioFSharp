(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "../../packages/build/FSharp.Plotly/lib/net45/Fsharp.Plotly.dll"
open FSharp.Plotly

#I "../../bin"
#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"
#r "FSharp.Care.dll"
#r "FSharp.Care.IO.dll"


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

//let fileDir = __SOURCE_DIRECTORY__ + "/data/"  //FASTAExample1.fasta"


let fileDir = @"D:\OneDrive\Projects\FastA proper\data\chlamydomonas reinhardtii\"



let getProteinModelInfo str =     
    let m = System.Text.RegularExpressions.Regex.Match(str,@"Cre(-?[\d]*)\.g([\d]*)\.t([\d]*)\.([\d]*)") 
    if m.Success then 
        let tmp = 
            [ for g in m.Groups -> g.Value.Trim() ] 
            |> List.toArray
        PetideClassification.creatProteinModelInfo tmp.[0] (int tmp.[1]) PetideClassification.StrandDirection.Forward (int tmp.[2]) (int tmp.[3]) Seq.empty Seq.empty        
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
            PetideClassification.createProteinModel info fi.Sequence
            |> Some                    
        )
    |> Seq.toArray
    

let ppRelationModel =
    let digest sequence =
        Digestion.BioArray.digest (Digestion.Table.getProteaseBy "Trypsin") 0 sequence
        |> Digestion.BioArray.concernMissCleavages 0 3
        |> Seq.map (fun p -> p.PepSequence |> List.toArray) // TODO not |> List.toArray
    
    PetideClassification.createPeptideProteinRelation digest sequences
    
let spliceVariantCount = PetideClassification.createLocusSpliceVariantCount ppRelationModel


let classified = 
    ppRelationModel.GetArrayOfKeys
    |> Array.map (fun peptide ->  
        let proteinInfo = (ppRelationModel.TryGetByKey peptide).Value
        PetideClassification.classify spliceVariantCount (peptide, (ppRelationModel.TryGetByKey peptide).Value))
