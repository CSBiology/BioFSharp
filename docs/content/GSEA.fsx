(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/BioFSharp.Stats/net461"
#r "BioFSharp.Stats.dll"
#r "FSharp.Stats.dll"
#r "FSharpAux.dll"


(** 
<table class="HeadAPI">
<td class="Head"><h1>Ontology Enrichment</h1></td>
<td class="API">
    <a id="APILink" href="https://csbiology.github.io/BioFSharp/reference/biofsharp-stats-ontologyenrichment.html" >&#128194;View module documentation</a>
</td>
</table>

Ontology Enrichment Analysis with BioFSharp builds upon the foundation of Gene Set Enrichment Analysis (GSEA) and enables you to calculate enrichments of
annotations in a group of any kind of biological dataset. For insights to the algorithm, check this [http://bioinformatics.oxfordjournals.org/cgi/content/abstract/23/4/401][publication].

For the purpose of this tutorial, we will use 

*)