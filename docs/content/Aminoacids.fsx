(*** hide ***)
#I "../../bin"
(**

# Amino Acids
<a id="SourceCode" href="https://github.com/CSBiology/BioFSharp/blob/master/src/BioFSharp/AminoAcids.fs">&lt;/&gt;view source code</a>
<a id="Author" href="https://github.com/HLWeil">&#128366;view author of this tutorial</a>
<br><br>

BioFsharp comes equipped with a broad range of features and functions for working with amino acids and proteins.  
For this there are two types of amino acids defined: `AminoAcidSymbol` is a very primitive type basically only consisting of a one letter code. 
It is located in the `AminoAcidSymbols` module. This type should be used for efficient pattern matching. 
On the other hand there is the `AminoAcid` type. This type uses the `IBioItem` interface and is packed with functionality. 
It's located in the 'AminoAcids' module. For more complex applications this type should be used.  
This tutorial covers the following topics:

* [Create Collections of amino acids](Aminoacids.html#Basics)

   In this chapter I'll talk about the different ways to create values of the `AminoAcid` type

* [Using the IBioItem-Interface](Aminoacids.html#IBioItem)

   The `IBioItem`-Interface adds a set of functionalities to the `AminoAcid` type
    
* [Modification](Aminoacids.html#Modification)

   This part is about how to modify `AminoAcid` using the built in functions

* [Pattern Matching](Aminoacids.html#Pattern-Matching)

   This part is about using 'AminoAcidSymbol' for quick pattern matching  

* [Example Workflow](Aminoacids.html#Example-Workflow) 
   
   Combines the knowlegde of the other chapters to show how it can be applied
*)
#r "BioFSharp.dll"
open BioFSharp
(**
<br>
##Basics

The type `AminoAcid` is in principle a union case of three letter codes. 
Besides the standard amino acids, there are also special cases like **Ter** which is a terminator and **Gap** which is a gap. (More on **Mod** [here](Aminoacids.html#Modification)
In principle you have two different ways of defining values of type `AminoAcid`. You can directly call the union case of the type:

*)
open AminoAcids

//one alanin
let myAlanine = AminoAcid.Ala

//list of amino acids
let myOligo = [AminoAcid.Glu;AminoAcid.Trp;AminoAcid.Lys;AminoAcid.Ala;AminoAcid.Cys]

(**
Of course this gets pretty redundant very quickly. For longer sequences you can just use the built in functions for the different collection types (BioSeq,BioArray,BioList):
*)

let myProtein = BioArray.ofAminoAcidString "ALGRELWYIILGNCGNLISREW"


(**

##IBioItem

Most of the functinality the `AminoAcid` type offers comes from the `IBioItem` interface. In F#, you have to explicitly implement interfaces in classes. 
Accessing those functionalities can be quite tricky at first. More on this [here](https://fsharpforfunandprofit.com/posts/interfaces/). 
In short: Sometimes you have to manually cast your instances of the class `AminoAcid` to the interface `IBioItem`. This is done using the `:>` operator.

*)

let myGlycine = AminoAcid.Gly
let myGlycine' = myGlycine :> IBioItem



(** 
Of course our goal is to make it as easy as possible for you.
Therefore many built-in functions of the `BioItem` can be found in the `AminoAcids` module. Those already do the casting for you.
This means that the manual casting is only necessary if you want to use functionalities of the IBioItem in special, self created functions (Or if you really really prefer using members over functions).  
To make the automatic casting a bit more clear:
The variable myGlycine' is of type `IBioItem`. The following three statements will print the same phrase:

*)

printfn "Hello my name is %s and I am an amino acid" (AminoAcids.name myGlycine)
printfn "Hello my name is %s and I am an amino acid" (BioItem.name myGlycine')
printfn "Hello my name is %s and I am an amino acid" myGlycine'.Name

(**
With the built in functions you can e.g. receive the chemical formula of the AA, check if it's a Terminator or get its mass.
For further functionalities check out the [API reference for IBioItem](https://csbiology.github.io/BioFSharp/reference/biofsharp-bioitem.html) or for [AminoAcids](https://csbiology.github.io/BioFSharp/reference/biofsharp-AminoAcids.html).
*)

(**
##Modification

The type `AminoAcid` also has a union case `mod`. This case is a tuple consisting of a Amino Acid and a list of `Modifications`.
Those modifications can be created using the functions of the `ModificationInfo` module and set using the functions of the `AminoAcids` module:
*)
open ModificationInfo

(**
coming soon

##Pattern Matching
coming soon
*)

(**
##Example Workflow
coming soon
*)