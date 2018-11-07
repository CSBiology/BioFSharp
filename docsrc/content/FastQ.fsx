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
#r "BioFSharp.IO.dll"
#r "FSharpAux.dll"
#r "FSharpAux.IO.dll"

(**

<table class="HeadAPI">
<td class="Head"><h1>FastQ format</h1></td>
<td class="API">
    <a id="APILink" href="https://csbiology.github.io/BioFSharp/reference/biofsharp-io-fastq.html" >&#128194;View module documentation</a>
</td>
</table>
*)

(*** hide ***)
open System
open BioFSharp
open BioFSharp.IO
open FSharpAux
open FSharpAux.IO

(**
This module allows to parse FASTQ format data with original 4-lines entries into this record type
*)

/// FastqItem record contains header, sequence, qualityheader, qualitysequence of one entry
type FastqItem<'a,'b> = {
    Header          : string
    Sequence        : 'a
    QualityHeader   : string
    QualitySequence : 'b      
}

(**
To be able to use this parser you need to define two converter functions, 
one example for each you can also find in our module, but you also may need to write your own.

We can convert sequence string to predefined option type of Amino Acids, using converter function
from our library 'BioFSharp.BioItemsConverter.OptionConverter'
*)

/// get characters as sequence units
let converterToAA string =
    string
    |> String.toCharArray
    |> Array.map (BioFSharp.BioItemsConverter.OptionConverter.charToOptionAminoAcid)

(**
If you have following possible values for quality sequence:
'!""#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~',
with Sanger format, that can encode a Phred quality score from 0 to 93 using ASCII 33 to 126, 
then you can use our converting function:
*)

/// get Phred quality score
let qualityConvertFn string =
    string
    |> String.toCharArray
    |> Array.map (fun i -> i.GetHashCode()-33)

(**
And then you can easily use this module to read your FastQ file
*)

let yourFastqFile = (__SOURCE_DIRECTORY__ + "/data/FastQtest.fastq")

let FastQSequence = 
    FastQ.fromFile converterToAA qualityConvertFn yourFastqFile
