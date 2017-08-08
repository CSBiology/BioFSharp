(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Introduction
============
After you have downloaded and set up BioFSharp (as described [here](index.html#Setting-up-BioFSharp)), it is time to see it in action. Since we are programmers, let's say 'Hello World' !  

The focus of this brief tutorial lies on introducing you to the BioFSharp workflow. We will build a protein from a raw string and look at some of it's properties.
Detailed information about the functions used can be either found in the specific tutorials in the sidebar or the API reference. 

First, we reference and open BioFSharp.
*)

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

BioFSharp comes with various data structures for biological objects such as aminoacids. As you most likely know, you can abbreviate a
sequence of aminoacids with a three or oneletter code for each single aminoacid. We can convert a character to an aminoacid by using the
`charToOptionAminoAcid` function from the `BioItemsConverter` library. This will return us an option type, being either an aminoacid or `None` 
if the caracter is not coding for an aminoacid.

*)

open BioFSharp.BioItemsConverter

///valid character 
let valid = OptionConverter.charToOptionAminoAcid 'A'
///invalid character
let invalid = OptionConverter.charToOptionAminoAcid '?'

(**
Which results in 'A' being recognized as `Some Ala` and '?' as `None`.

To parse our entire string, we can use any of the BioCollections' `ofAminoAcidString` functions. For more information about BioSeq, BioList and BioArray go [here](Biosequences.html)
*)

///Protein represented as a Bioseq
let parsedProtein1 = rawString |> BioSeq.ofAminoAcidString 

///Protein represented as a BioList
let parsedProtein2 = rawString |> BioList.ofAminoAcidString 

///Protein represented as a BioArray
let parsedProtein3 = rawString |> BioArray.ofAminoAcidString 

(**
This yields us our Hello World protein. note that the '!' from the raw string is not contained in the sequence as it is not coding for an aminoacid.
*)


