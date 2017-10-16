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
<td>
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

let fileDir = __SOURCE_DIRECTORY__ + "/data/"  //FASTAExample1.fasta"

//reads from file to an array of FastaItems.
let sequences = 
    fileDir + "Chlamy_Cp.fastA"
    |> FastA.fromFile BioArray.ofAminoAcidString


(** 
Analogously it is possible to directly read compressed fastA files:
*)
let sequences2 = 
    fileDir + "Chlamy_Cp.gz"
    |> FastA.fromGzipFile BioArray.ofAminoAcidString

(**

In both cases it is worth noticing that `BioArray.ofAminoAcidString` can be replaced by any converter function.
The converter maps from the sequence of character to either amino acid or nucleotide sequences. Therefore use `BioArray.ofAminoAcidString` for petide and `BioArray.ofNucleotideString` for gene FastA files, respectively.
It is of course also possible to introduce any converter function.



Writing FastA files
-------------------

In order to write a collection of sequences (`FastaItem<_>`) into a file use the following function.

*)
sequences
|> FastA.write BioItem.symbol (fileDir + "FASTAExample3.fasta")  


(**
Example: protein length distribution
------------------------------------

With the FastA reader it is straightforward to access the protein length distribution:
*)

let sequencesLength = 
    fileDir + "Chlamy_Cp.fastA"
    |> FastA.fromFile BioArray.ofAminoAcidString
    |> Seq.map (fun item -> item.Sequence.Length)

(*** define-output:lengthHisto1 ***)
Chart.Histogram sequencesLength
|> Chart.withX_AxisStyle("length")
|> Chart.withY_AxisStyle("#count")
(*** include-it:lengthHisto1 ***)



