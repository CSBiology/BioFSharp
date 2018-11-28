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

#r "BioFSharp.dll"

open BioFSharp
open BioFSharp.Formula.Table

(**

<table class="HeadAPI">
<td class="Head"><h1>BioItems</h1></td>
<td class="API">
    <a id="APILink" href="https://csbiology.github.io/BioFSharp/reference/biofsharp-bioitem.html" >&#128194;View module documentation</a>
</td>
</table>
Often, dealing with similar problems separately results in different approaches. In a programming background, this might make things needlessly complex. Therefore in BioFSharp nucleotides and amino acids are based on the same structural scaffold, leading to a consistent way of working with them. This can come in handy especially when working with their formulas.  

## Basics
First let's define some amino acids and nucleotides 
*)
let ala = AminoAcids.Ala
let lys = AminoAcids.Lys
let glu = AminoAcids.Glu
let ter = AminoAcids.Ter

let g = Nucleotides.G
let t = Nucleotides.T
let gap = Nucleotides.Gap

(**
Many functions are similar for AminoAcids and Nucleotides, like for example:
*)
(***hide***)
let alaName = AminoAcids.name ala 
let gName = Nucleotides.name g 
let lysFormula = AminoAcids.formula lys |> Formula.toString 
let tFormula =  Nucleotides.formula t |> Formula.toString 

(**Accessing the full name:*)
(***do-not-eval***)
AminoAcids.name ala 
(*** include-value:alaName ***)
Nucleotides.name g 
(*** include-value:gName ***)
(**or the underlying chemical formula:*)
AminoAcids.formula lys |> Formula.toString 
(*** include-value:lysFormula ***)
Nucleotides.formula t |> Formula.toString 
(*** include-value:tFormula ***)

(**
Nucleotides and AminoAcids in BioFSharp are represented as Union cases. This makes applying functions selectively very easy. 
*)
let filterLysine aa = 
    match aa with
    | AminoAcids.Lys -> AminoAcids.Gap
    | _ -> aa

(*** hide ***)
let f1 = filterLysine ala
let f2 = filterLysine lys

(***do-not-eval***)
filterLysine ala // val it : AminoAcids.AminoAcid = Ala
(*** include-value:f1 ***)
filterLysine lys // val it : AminoAcids.AminoAcid = Gap
(*** include-value:f2 ***)

(**
Of course some functions like these are already defined. Let's use a predefined function to find charged amino acids.

*)


let giveMePositiveAAs aminoAcid = 
    match aminoAcid with
    | a when AminoAcids.isPosCharged a -> 
        printfn 
            "Hey, how are you? I am %s, but my friends call me %c. I'm usually in a positive mood"
            (AminoAcids.name a)
            (AminoAcids.symbol a)

    | a when AminoAcids.isNegCharged a -> 
        printfn 
            "I am %s, short: %c. I'm usually in a negative mood"
            (AminoAcids.name a)
            (AminoAcids.symbol a)

    | _ -> printfn "Just strolling around, minding my own business."

(**Alanine is usually not charged:*)

(*** define-output:out1 ***)
giveMePositiveAAs ala
(*** include-output:out1 ***)

(**Lysine is usually positively charged:*)

(*** define-output:out2 ***)
giveMePositiveAAs lys
(*** include-output:out2 ***)

(**Glutamic acid is usually negatively charged:*)

(*** define-output:out3 ***)
giveMePositiveAAs glu
(*** include-output:out3 ***)


(**
<table class="HeadAPI">
<td class="Head"><h2>Amino Acids</h2></td>
<td class="API">
    <a id="APILink" href="https://csbiology.github.io/BioFSharp/reference/biofsharp-aminoacids.html" >&#128194;View module documentation</a>
</td>
</table>
</pre>  
<table class="HeadAPI">
<td class="Head"><h3>Modification</h3></td>
<td class="API">
    <a id="APILink" href="https://csbiology.github.io/BioFSharp/reference/biofsharp-modificationinfo.html" >&#128194;View module documentation</a>
</td>
</table>
What makes working on Amino Acids with BioFSharp truly powerful is the ability to easily modify AminoAcids, even altering their mass and formula. In the following example we try to find out the mass of a phosphorylated Serine. Applications like these might be quite usefull for identification of peptides in mass spectrometry.  
As a first step, we make ourselves an easy helper function which returns the formula of an AminoAcid as string. Afterwards we start out by creating a Serine.
*)

//easy helper function
let getF (aa:AminoAcids.AminoAcid) = AminoAcids.formula aa |> Formula.toString

///Serine -H20
let mySerine = AminoAcids.AminoAcid.Ser

(***hide***)
let getF1 = getF mySerine

(***do-not-eval***)
getF mySerine
(***include-value:getF1***)

(**
As you can see by the formula. Our Serine is missing two H and an O. In BioFSharp, all Amino Acids are dehydrolysed by default, because it is assumed that the user will use collections representing a peptide, rather than single Amino Acids. For our cause we want serine in hydrolysed form. An easy way to achieve this is to modify it. An addition of H2O is quite common and therefore premade: 
*)

///Hydrolysed serine

let hydroSerine = AminoAcids.setModification ModificationInfo.Table.H2O mySerine

(***hide***)
let getF2 = getF hydroSerine
(***do-not-eval***)
getF hydroSerine 
(***include-value:getF2***)

(**
So far so good. Now let's add the phosphate. For this we first create a function which alters the formula of a given molecule in the way a phosphorylation would. In the second step we create a modification resembling a phosphorylation of a residual. At last we modify our Serine with this modification.
*)

///Phosphorylation of OH-Groups adds PO3 to formula and removes one H
let phosporylate formula =  
    Formula.add (Formula.parseFormulaString "PO3") formula
    |> Formula.substract (Formula.parseFormulaString "H")

//We create a modification at the residual called phosphorylation which in our case is hypothetical, hence the `false` for the 'isBiological` parameter
let phosphorylation = ModificationInfo.createModification "Phosphorylation" false ModificationInfo.ModLocation.Residual phosporylate

///phosphorylated Serine
let phosphoSerine = AminoAcids.setModification phosphorylation hydroSerine

(***hide***)
let getF3 = getF phosphoSerine
(***do-not-eval***)
getF phosphoSerine 
(***include-value:getF3***)

(**
As you can see the Serine is phosphorylated just as we wanted. Our inital aim was to check the mass, this can be done quite easily:
*)
(***hide***)
let m1 = AminoAcids.averageMass mySerine
let m2 = AminoAcids.averageMass phosphoSerine
(***do-not-eval***)
AminoAcids.averageMass mySerine
(***include-value:m1***)
AminoAcids.averageMass phosphoSerine
(***include-value:m2***)
(**
<table class="HeadAPI">
<td class="Head"><h2>Nucleotides</h2></td>
<td class="API">
    <a id="APILink" href="https://csbiology.github.io/BioFSharp/reference/biofsharp-nucleotides.html" >&#128194;View module documentation</a>
<td>
</table>
As working with nucleotides is usually focused on the sequence of the bases, rather than how they actually look like, the list of nucleotide specific functions would be quite short. Here are some of the basic helper functions: 
*)

let myAdenine = Nucleotides.A 
let myThymine = Nucleotides.complement myAdenine 
(***include-value:myThymine***)

Nucleotides.replaceTbyU myAdenine // val it : Nucleotides.Nucleotide = A
Nucleotides.replaceTbyU myThymine // val it : Nucleotides.Nucleotide = U