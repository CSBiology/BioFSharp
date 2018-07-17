(*** hide ***)

#I "../../bin/BioFSharp.IO/net461"

#r "BioFSharp.dll"
#r "BioFSharp.IO.dll"

(**
<table class="HeadAPI">
<td class="Head"><h1>Clustal Omega Wrapper</h1></td>
<td class="API">
    <a id="APILink" href="https://csbiology.github.io/BioFSharp/reference/biofsharp-io-clustalowrapper.html" >&#128194;View module documentation</a>
</td>
</table>
Clustal Omega is a multiple sequence alignment (MSA) tool. This tutorial describes using it in F# via the ClustalOWrapper.  
For some more indepth information about which parameters to choose for your goal, also check out [the official tutorial](http://www.clustal.org/omega/README).

## Aligning sequences from files

The first step is to create a wrapper-object. As optional input it takes a path to the clustalo executable you want to use. You have to fill this argument if you work with a precompiled verion or on linux.
*)

open BioFSharp.IO
open ClustalOWrapper
open BioFSharp

let cw = ClustalOWrapper()

(**
The general structure of arguments the wrapper takes was kept the same as in the command line tool. In general, you need an `inputPath`, an `outputPath` and `parameters`. As there are several inputs possible, you have to choose what it is. As we want to align a normal sequence we just pick `SequenceFile`.
*)


let inputPath = Input.SequenceFile (__SOURCE_DIRECTORY__ + "file.fasta")

let outputPath = __SOURCE_DIRECTORY__ + "file.aln"

(**
As additional parameters go, we'll restrict input to FastA format and the output to Clustal format. Also we will use the `Force` parameter to force the overwrite of a possilby already existing file with the name `file.aln`. For a complete overview of implemented parameters, check out the [API reference](https://csbiology.github.io/BioFSharp/reference/biofsharp-io-clustalowrapper-parameters.html).
*)
//Input has to be in FastA format
let inputModifier = Parameters.ClustalParams.Input [Parameters.InputCustom.Format Parameters.FileFormat.FastA]
//Output has to be in Clustal format
let outputModifier = Parameters.ClustalParams.Output [Parameters.OutputCustom.Format Parameters.FileFormat.Clustal]
//Forces overwriting
let forceModifier = Parameters.ClustalParams.Miscallaneous [Parameters.MiscallaneousCustom.Force]

//Perform alignment
cw.AlignFromFile(inputPath,outputPath,[inputModifier;outputModifier;forceModifier])

(** 
## Aligning sequences directly in F# Interactive

With the `AlignSequences` method, one can also directly align sequences with the clustal tool and also directly receive the alignment directly in F#.

As input, it takes a collection of `TaggedSequence`s, and again a set of parameters. The default options can be used by not using any additional parameters.
*)

let sequences = 
    [
    BioID.createTaggedSequence "pep1" ("AAGECGEK")
    BioID.createTaggedSequence "pep2" ("AAGEGEK")
    BioID.createTaggedSequence "pep3" ("AAAGECGEK")
    BioID.createTaggedSequence "pep4" ("AAGECGEL")
    ]

let alignedSequences = 
    cw.AlignSequences(sequences,Seq.empty)

(**
Which will give the following result: 
<pre>
val it :
  Alignment.Alignment<BioID.TaggedSequence<string,char>,Clustal.AlignmentInfo>
= {MetaData = {Header = seq ['C'; 'L'; 'U'; 'S'; ...];
               ConservationInfo = seq [' '; '*'; '*'; '*'; ...];};
   AlignedSequences =
    [{Tag = "pep1";
      Sequence = seq ['-'; 'A'; 'A'; 'G'; ...];};
     {Tag = "pep2";
      Sequence = seq ['-'; 'A'; 'A'; 'G'; ...];};
     {Tag = "pep3";
      Sequence = seq ['A'; 'A'; 'A'; 'G'; ...];};
     {Tag = "pep4";
      Sequence = seq ['-'; 'A'; 'A'; 'G'; ...];}];}
</pre>
*)