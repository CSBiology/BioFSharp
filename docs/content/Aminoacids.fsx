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

   This part is about using `AminoAcidSymbol` for quick pattern matching
   
* [Isoelectric Point](Aminoacids.html#Isoelectric-Point)

    This part is about how to calculate the theoretic isoelectric point of peptides

* [Example Workflow](Aminoacids.html#Example-Workflow) 
   
   Combines the knowlegde of the other chapters to show how it can be applied
*)
#r "BioFSharp.dll"
open BioFSharp
(**
<br>
##Basics

The type `AminoAcid` is in principle a discriminated union of three letter codes.  
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

The type `AminoAcid` also has a union case `mod`. This case is a tuple consisting of a Amino Acid and a list of `Modifications`. Therefore it's a recursive type and can be modified at will.
Those modifications can be created using the functions of the `ModificationInfo` module and set using the functions of the `AminoAcids` module:
*)
open ModificationInfo

(**
coming soon

##Pattern Matching

As stated above, for all matters where only the primary structure of the peptide is important, the `AminoAcidSymbol` should be the type of your choice. 
This type gets hashed as byte and is therefore very quick for tasks like alignment or pattern search. To create values of this type, you again have different possibilites:
First of all you can directly call the Enum case:
*)
open AminoAcidSymbols
let myAla = AminoAcidSymbol.Ala
(**
The easiest and most flexible way though is to use the `aminoAcidSymbol` mapping function. As input it takes either a `character` or an `AminoAcid`.
*)

//we defined "myOligo" above
let myAASOligo = myOligo |> Seq.map aminoAcidSymbol

let myArg = 'R' |> aminoAcidSymbol

let myAASProtein = "AMNTGILERVCMBPSSDT" |> Seq.map aminoAcidSymbol

(**
As you can see this function can be intuitively applied in different scenarios.  
Checking equality of AminoAcidSymbols can be done using the standard equals operator:
*)
myArg = myAla
(**
The comparison is based on comparing the according byte values. This makes using this type for pattern matching effective and easy. Some implementations of BioFSahrp are designed in a way which takes advantage of this. 
An example for this is the implementation for `pairwise alignment`, which even comes equipped with a set of amino acid scoring matrices.
A quick inroduction for aligning amino acid sequences using the implemented algorithm can be found [here](Alignment.html).
*)
(**
##Isoelectric Point

The isoelectric point (pI) of a protein is the point at which it carries as many positive as negative charges. 
Therefore the overall charge is zero. Knowing this value can e.g. be useful for isolation of single proteins in a voltage gradient.  
The implementation is based on: "[http://fields.scripps.edu/DTASelect/20010710-pI-Algorithm.pdf](http://fields.scripps.edu/DTASelect/20010710-pI-Algorithm.pdf)". 
In principle, the distinct amino acids in the protein are counted. 
By using the [Henderson-Hasselbalch equation](https://en.wikipedia.org/wiki/Henderson-Hasselbalch_equation) and the pKr values, the theoretic charge states of the amino acids for a specific pH can be calculated. 
Multiplying those charge states with the count of the associated amino acids and adding those products together then gives the overall charge of the protein. This is only done with the amino acids, which might be charged (basic, acidic). 
The isoelectric point is the pH value for which this function returns zero. It is found by [bisection](https://en.wikipedia.org/wiki/Bisection_method) (also called Binary Search).  
Disclaimer: Keep in mind, that this algorithm ignores post-translational modifications and interactions of the amino acids with each other. Therefore it is only intented to be a rough approximation and should be used as such.
<br>
The function for finding the isoelectric point is found in the `IsoelectricPoint` module. 

* Besides the peptide sequence in form of a `AminoAcidSymbol` Seq, it takes 
* a mapping function, which maps an `AminoAcidSymbol` to a float representing the pKr and
* an accuracy value. The found pI has to be at least as close to zero as this accuracy value

*)

//AA sequence
let myProteinForPI = 
    "ATPIIEMNYPWTMNIKLSSDACMTNWWPNCMTLKIIA"
    |> Seq.map AminoAcidSymbols.aminoAcidSymbol

//default function for pKr of charged aminoacids
let pKrFunction = IsoelectricPoint.getpKr

//accuracy in z
let acc = 0.5

let pI = IsoelectricPoint.tryFind pKrFunction acc myProteinForPI

(**
The result will be of type `Option<float*float>`. The first float is the pH, the second one is the according charge.
*)

(**
##Example Workflow
coming soon
*)