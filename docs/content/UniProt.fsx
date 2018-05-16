(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/BioFSharp.BioDB/net461"
#I "../../bin/BioFSharp.IO/net461"

#r "BioFSharp.dll"
#r "FSharpAux.dll"
#r @"../../packages\build\FSharp.Plotly\lib\net45\FSharp.Plotly.dll"
#r "BioFSharp.IO.dll"
#r "BioFSharp.BioDB.dll"
#r "SwaggerProvider.Runtime.dll"

open BioFSharp.BioDB
open FSharp.Plotly
open BioFSharp
open BioFSharp.Algorithm
open FSharpAux
open BioFSharp.Formula.Table

(**
Accessing online databases
==============================
BioFSharp contains a set of readers that facilitate the access to different biological online resources. This documentation aims to give an introduction for them.


<a name="UniProt"></a>
<table class="HeadAPI">
<td class="Head"><h1>UniProt's Proteins REST API online access</h1></td>
<td class="API">
    <a id="APILink" href="https://csbiology.github.io/BioFSharp/reference/biofsharp-biodb-ebiapi.html" >&#128194;View module documentation</a>
</td>
</table>
The Proteins REST API provides access to key biological data from UniProt and data from Large Scale Studies data mapped to UniProt. 
The services provide sequence feature annotations from UniProtKB, variation data from UniProtKB and mapped from Large Scale data sources (1 Genomes, ExAC and COSMIC), 
proteomics data mapped from Large Scale sources (PeptideAtlas, MaxQB and EPD) and genome coordinate mappings.
  
In this tutorial I want to show you how to access data from UniProt and give examples for what might be done with it. As a first step we will retreive data about the Copper-transporting ATPase 2 of different species. 

*)

let mouse = EbiAPI.UniProteinDB.getProteinSeqFeature "Q64446"
let rat = EbiAPI.UniProteinDB.getProteinSeqFeature "Q64535"
let human = EbiAPI.UniProteinDB.getProteinSeqFeature "P35670"
let sheep = EbiAPI.UniProteinDB.getProteinSeqFeature "Q9XT50"

(**
What we want to do now is getting an measure of phylogenetic relationship. We will do this by aligning the peptide-sequences with each other. For this we prepare the data and the alignment function.
*)

let proteinInfos = [|human;mouse;rat;sheep|]
let sequences = proteinInfos |> Array.map ((fun x -> x.Sequence) >> BioArray.ofAminoAcidSymbolString)
let names = proteinInfos |> Array.map (fun x -> x.EntryName)

(**
<br>
<button type="button" class="btn" data-toggle="collapse" data-target="#alignmentExample">Show/Hide Alignment functions</button>
<div id="alignmentExample" class="collapse alignmentExample ">
*)

///Creates the pairwise global alignment of two amino acid sequences based on the BLOSUM62 scoring matrix
let align a b =
    let scoring = ScoringMatrix.getScoringMatrixAminoAcid ScoringMatrix.BLOSUM62
    let costs : PairwiseAlignment.Costs<AminoAcidSymbols.AminoAcidSymbol> = 
        {Open = -4;
        Continuation = -3;
        Similarity = scoring}
    PairwiseAlignment.NeedlemanWunsch.runAminoAcidSymbol costs a b

///Aligns two amino acid sequences and writes their relative alignment score into a square matrix
let createAlignmentMatrix (sequences : BioArray.BioArray<AminoAcidSymbols.AminoAcidSymbol> []) = 
        let mat = Array2D.zeroCreate 4 4
        for i = 0 to 3 do
            for j = i to 3 do 
                let alignment = align sequences.[i] sequences.[j]
                mat.[i,j] <- float alignment.MetaData
                mat.[j,i] <- float alignment.MetaData
        let mat' = Array2D.map (fun x ->  x / (Array2D.maxBy id mat)) mat
        for i = 0 to 3 do
            mat'.[i,i] <- 1.
        mat'
(**
<button type="button" class="btn" data-toggle="collapse" data-target="#alignmentExample">Hide again</button>  
</div>
<br>
Let's see what we get..
*)

(*** define-output:heatmap ***)
createAlignmentMatrix sequences
|> Array2D.toJaggedArray
|> fun x -> Chart.Heatmap(data = x,RowNames = names, ColNames = names,Colorscale = StyleParam.Colorscale.Custom([0.,"#ffffff";1.,"#44546A"]),Name = "Relative pairwise alignment score")
(*** include-it:heatmap ***)

(**
We can see that the Copper-transporting ATPase 2 of mouse and rat are the most similar to each other, while the one of the sheep is relatively different.  
<br>
But not only the protein names and their sequences can be stored in UniProt. For these proteins we also get information about the cellular location of each of their amino acids. Why not see if the hydrophobic amino acids are located in the membrane after all?  
First we design a type to model the location more nicely. Afterwards we create a function for mapping the protein information to just the location information of all amino acids in one protein.
*)
type Location = 
    |Intracellular
    |Transmembrane
    |Extracellular
///Maps protein information to information of location of amino acids of this protein
let getLocationInfo (proteinInfo:EbiAPI.ProteinsAPIschema.ProteinFeatureInfo) = 
    proteinInfo.Features
    |> Array.choose (fun x -> 
        match x.Type with
        | "TOPO_DOM" | "TRANSMEM" -> Some (x.Begin,x.End,x.Description)
        | _ -> None)
    |> Array.collect (fun (s,e,info) -> 
                        Array.init 
                            ((int e) - (int s)+1)
                            (fun i -> 
                                match info with
                                | _ when info = "Cytoplasmic"   -> Intracellular
                                | _ when info = "Helical"       -> Transmembrane
                                | _ when info = "Extracellular" -> Extracellular
                            )
                        )


(**
We also create ourselves a function for retreiving hydrophobicity of all amino acids of our protein and an additional helper.
<br>
<button type="button" class="btn" data-toggle="collapse" data-target="#hydroExample">Show/Hide Additional functions</button>
<div id="hydroExample" class="collapse hydroExample ">
*)
///Evaluates the hydrophobicity of all amino acids of a sequence
let getHydrophobicity a =
    let hydro = AminoProperties.initGetAminoProperty AminoProperties.HydrophobicityIndex
    AminoProperties.ofWindowedBioArray 10 hydro a

///Creates the squares depicting the cell membrane
let createBackgroundShape (i,(location : Location)) =
    let i = float i 
    match location with 
    | Transmembrane -> 
        Some (Shape.init (ShapeType = StyleParam.ShapeType.Rectangle, X0 = (i-0.5), X1 = (i+0.5),Y0 = 0.,Y1  = 2.,Opacity = 0.05,Fillcolor = Colors.toWebColor Colors.Table.CSB.orange))
    | _ -> None
(**
<button type="button" class="btn" data-toggle="collapse" data-target="#hydroExample">Hide again</button>  
</div>
We pick the mouse protein for this analysis.
*)
let hydrophobicityArr = 
    getHydrophobicity sequences.[1]
let mouseLocInfo = mouse |> getLocationInfo

(**
Now let's see what we get. The colored areas depict the cell membrane. The line graph represents the amino acids of the protein with their given hydrophobicity.
*)
(*** define-output:shapes ***)
Chart.Line ((Array.indexed hydrophobicityArr),Color = Colors.toHex false Colors.Table.CSB.blue)
|> Chart.withShapes(mouseLocInfo |> Seq.indexed |> Seq.choose createBackgroundShape)
|> Chart.withX_AxisStyle("Sequenceindex")
|> Chart.withY_AxisStyle("Hydrophobicity")
(*** include-it:shapes ***)

(**
As you can see the hydrophobic areas overlap with the membrane areas.
*)