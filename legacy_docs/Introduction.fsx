(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I @"../../bin/BioFSharp/net47/"
#I @"../../bin/BioFSharp.BioDB/net47/"
#I @"../../bin/BioFSharp.ImgP/net47"
#I @"../../bin/BioFSharp.IO/net47/"
#I @"../../bin/BioFSharp.Parallel/net47/"
#I @"../../bin/BioFSharp.Stats/net47/"
#I @"../../bin/BioFSharp.Vis/net47/"
#r @"../../packages/formatting/FSharp.Plotly/lib/netstandard2.0/FSharp.Plotly.dll"
#r "BioFSharp.IO"
(**
Introduction
============
After you have downloaded and set up BioFSharp (as described [here](index.html#Installation)), it is time to see it in action. Since we are programmers, let's say 'Hello World' !  

The focus of this brief tutorial lies on introducing you to the BioFSharp workflow. We will build a protein from a raw string and look at some of it's properties.
Detailed information about the functions used can be either found in the specific tutorials in the sidebar or the API reference. 

First, we reference and open BioFSharp.
*)
//Your path may differ
#r"BioFSharp.dll"

open BioFSharp

(**

The Hello World Protein
=======================
Time to say 'Hello World' the bioinformatician way. This is our raw string:

*)

///string form of our hello world protein
let rawString = "HELLOWORLD!"

(**

BioFSharp comes with various data structures for biological objects such as amino acids. As you most likely know, you can abbreviate a
sequence of aminoacids with a three or oneletter code for each single aminoacid. We can convert a character to an aminoacid by using the
`charToOptionAminoAcid` function from the `BioItemsConverter` library. This will return us an option type, being either an aminoacid or `None` 
if the caracter is not coding for an aminoacid.

*)

open BioFSharp.BioItemsConverter

///valid character 
let valid = OptionConverter.charToOptionAminoAcid 'A'
(*** include-value:valid ***)
///invalid character
let invalid = OptionConverter.charToOptionAminoAcid '?'

(**
<pre>
 None
</pre
*)

(**
Which results in 'A' being recognized as `Some Ala` and '?' as `None`.

To parse our entire string, we can use any of the BioCollections' `ofAminoAcidString` functions, which use this converter internally. For more information about BioSeq, BioList and BioArray go [here](BioCollections.html)
*)

///Protein represented as a Bioseq
let parsedProtein1 = rawString |> BioSeq.ofAminoAcidString 
(*** include-value:parsedProtein1 ***)

///Protein represented as a BioList
let parsedProtein2 = rawString |> BioList.ofAminoAcidString 
(*** include-value:parsedProtein2 ***)

///Protein represented as a BioArray
let parsedProtein3 = rawString |> BioArray.ofAminoAcidString 
(*** include-value:parsedProtein3 ***)

(**
This yields us our Hello World protein. note that the '!' from the raw string is not contained in the sequence as it is not coding for an aminoacid.
*)

(**
Pretty Printing
===============

Especially when working with longer sequences or multiple sequences, it can be beneficial to overwrite the default printing of biological datastructures.
BioFSharp comes with a pretty printer for all BioCollections:
*)

let largerSequence = 
    """MASSMLSSATMVASPAQATMVAPFNGLKSSAAFPATRKANNDITSITSNGGRVNCMQVWP
    PIGKKKFETLSYLPDLTDSELAKEVDYLIRNKWIPCVEFELEHGFVYREHGNSPGYYDGR
    YWTMWKLPLFGCTDSAQVLKEVEECKKEYPNAFIRIIGFDNTRQVQCISFIAYKPPSFT
    MASSMLSSATMVASPAQATMVAPFNGLKSSAAFPATRKANNDITSITSNGGRVNCMQVWP
    PIGKKKFETLSYLPDLTDSELAKEVDYLIRNKWIPCVEFELEHGFVYREHGNSPGYYDGR
    YWTMWKLPLFGCTDSAQVLKEVEECKKEYPNAFIRIIGFDNTRQVQCISFIAYKPPSFT
    MASSMLSSATMVASPAQATMVAPFNGLKSSAAFPATRKANNDITSITSNGGRVNCMQVWP
    PIGKKKFETLSYLPDLTDSELAKEVDYLIRNKWIPCVEFELEHGFVYREHGNSPGYYDGR
    YWTMWKLPLFGCTDSAQVLKEVEECKKEYPNAFIRIIGFDNTRQVQCISFIAYKPPSFT""" 
    |> BioArray.ofAminoAcidString

(*** include-value:largerSequence ***)

(** 
As you can see here, using the standard printing you are only able to see the first 100 amino acids in this sequence. 
Now lets take a look on the output when we use the pretty printer:
*)

open BioFSharp.IO.FSIPrinters
fsi.AddPrinter(prettyPrintBioCollection)

(**
<pre>
|val it : BioArray.BioArray<AminoAcids.AminoAcid> =
|  
|         1  MASSMLSSAT MVASPAQATM VAPFNGLKSS AAFPATRKAN NDITSITSNG GRVNCMQVWP
|        61  PIGKKKFETL SYLPDLTDSE LAKEVDYLIR NKWIPCVEFE LEHGFVYREH GNSPGYYDGR
|       121  YWTMWKLPLF GCTDSAQVLK EVEECKKEYP NAFIRIIGFD NTRQVQCISF IAYKPPSFTM
|       181  ASSMLSSATM VASPAQATMV APFNGLKSSA AFPATRKANN DITSITSNGG RVNCMQVWPP
|       241  IGKKKFETLS YLPDLTDSEL AKEVDYLIRN KWIPCVEFEL EHGFVYREHG NSPGYYDGRY
|       301  WTMWKLPLFG CTDSAQVLKE VEECKKEYPN AFIRIIGFDN TRQVQCISFI AYKPPSFTMA
|       361  SSMLSSATMV ASPAQATMVA PFNGLKSSAA FPATRKANND ITSITSNGGR VNCMQVWPPI
|       421  GKKKFETLSY LPDLTDSELA KEVDYLIRNK WIPCVEFELE HGFVYREHGN SPGYYDGRYW
|       481  TMWKLPLFGC TDSAQVLKEV EECKKEYPNA FIRIIGFDNT RQVQCISFIA YKPPSFT
</pre>
*)
