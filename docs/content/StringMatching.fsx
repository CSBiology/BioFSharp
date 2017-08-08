(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.IO.dll"
#r "../../bin/FSharp.Care.IO.dll"
open System
open BioFSharp
open BioFSharp.IO
open FSharp.Care.IO
open AminoAcids
open Nucleotides
open AminoAcidSymbols
open BioFSharp.Algorithm.StringMatching
(**

String matching algorithms
==========================
<a id="SourceCode" href="https://github.com/CSBiology/BioFSharp/blob/master/src/BioFSharp/Algorithm/PatternQuery.fs">&lt;/&gt;view source code</a>
<a id="Author" href="https://github.com/kMutagene">&#128366;view author of this tutorial</a>
<br><br>

String matching algorithms are concerned with finding a single or multiple matches of a query pattern within a source. The sub-modules of the `BioFSharp.Algorithm.StringMatching` module are organized as following:

 * AlgorithmName.**findAll** : returns the (zero-based) starting position of all matches of the query pattern in the source as an integer list
                             
 * AlgorithmName.**find** : returns the (zero-based) starting position of the first match of the query pattern in the source as an integer
                             
 * AlgorithmName.**findFrom** : returns the (zero-based) starting position of the first match of the query pattern in the source as an integer, starting from a specified (zero-based) position in the source

All algorithms contained in this library are implemented as generic as possible regarding the data type that the input source and query pattern can contain, being only restricted by implementing the `IEquatable` interface, as there must be a way to assure that two equal elements are indeed equal.
However, the nature of the specific algorithm may impose additional restrictions, for example concerning the size of a single comparable item. 

The type of query pattern and source has been constrained to be an `array<'a>` because many index accessions are made during the search.

Runtimes are provided using [Bachmann-Landau notations](https://en.wikipedia.org/wiki/Big_O_notation#Family_of_Bachmann.E2.80.93Landau_notations): 

 * n : length of the source

 * m : length of the query pattern

 * k : size of the Alphabet

The following query pattern and source will be used in all further examples demonstrating how to use the specific module. 

*)

///source containing the query pattern at positions 0,10 and 17 
let source = "AAABBBBBBBAAABBBBAAA" |> BioArray.ofAminoAcidString

///query pattern to find in the source
let queryPattern = "AAA"|> BioArray.ofAminoAcidString

(**
<hr></hr>
<br>
The Naive Algorithm
===================
The naive approach to the string matching problem is walking through the source starting from the beginning and checking at each position if the resulting substring equals the query pattern. 
While being inefficient, it may be beneficial to use it in cases where the speed advantage of another algorithm is neglegible or does not outhweigh the additional setup needed (for example if your source and query pattern are really short)

Usage
-----
No additional setup needed. Just use query pattern and source as input parameters for the respective function

*)

//returns [0;10;17]
Naive.findAll queryPattern source

//returns 0
Naive.find queryPattern source

//returns 17
Naive.findFrom 12 queryPattern source

(**

Runtime
-------

 * Preprocessing: _None_

 * Matching time: _O(nm)_

<hr></hr>
<br>
Knuth Morris Pratt
==================
The KMP algorithm makes use of previous match information to determine an amount of skips that can be made until the next position in the source gets examined as a possible match.
To achieve that, a prefix table (or failure function) of the pattern needs to be computed, which determines the amount of skippable elements depending on the previous (partial) match.


Usage
-----
*)
///prefix table computed from the query pattern
let prefixTable = KnuthMorrisPratt.createPrefixTable queryPattern

//returns [0;10;17]
KnuthMorrisPratt.findAll prefixTable queryPattern source

//returns 0
KnuthMorrisPratt.find prefixTable queryPattern source

//returns 17
KnuthMorrisPratt.findFrom prefixTable queryPattern 12 source

(**
Runtime
-------
 * Preprocessing: _Θ(m)_

 * Matching time: average: _Θ(n)_

 * Worst case(if the prefix table results in no possible skips): _Θ(mn)_ 

<hr></hr>
<br>
Rabin Karp
==========
The RK Algorithm saves time by not comparing every single element of pattern and a substring of the same length from the source. Instead, it compares hashed versions of them.
The RabinKarp module contains two submodules:

 * RKStandard: uses the build in .NET hash function `hash` 

 * CP: uses a cyclic polynomial hash, which is way faster than the built in hash function. Elements must be castable to `uint64` , as they get hashed as a 64 bit unsigned integer and updating the hash uses bitwise operators. 
 Using large query patterns is problematic, as the combined hash may exceed the size of an `uint64`.

Usage
-----

###RKStandard
*)
//returns [0;10;17]
RabinKarp.RKStandard.findAll queryPattern source

//returns 0
RabinKarp.RKStandard.find queryPattern source

//returns 17
RabinKarp.RKStandard.findFrom 12 queryPattern source


(**
###CP

As the AminoAcid type cannot be casted to an `uint64`, a conversion to their one character code is needed *)

///source converted to char array 
let source' = source |> Array.map (fun x -> (x :> IBioItem).Symbol)

///query pattern converted to char array
let queryPattern' = queryPattern |> Array.map (fun x -> (x :> IBioItem).Symbol)

//returns [0;10;17]
RabinKarp.CP.findAll queryPattern' source'

//returns 0
RabinKarp.CP.find queryPattern' source'

//returns 17
RabinKarp.CP.findFrom 12 queryPattern' source'

(**
###Using your own hash functions
The `RabinKarp` module provides generic `findAll`, `find` and `findFrom` functions that take the following additional parameters:

 * `blockHash`: a function that hashes an entire array (used to hash the pattern and the first substring of the source)

 * `updateHash`: a function that removes an element from an existing hash and adds a new one

Just use functional composition to build your own findX functions.
In fact the two provided algorithms are built the same way:
*)

///RabinKarp.CP.findAll built by functional composition
let inline findAll (pattern : array<'a>) (s : array<'a>) = 
    RabinKarp.findAllGeneric (RabinKarp.CP.updateHash pattern.Length) (RabinKarp.CP.blockHash) pattern s 

(**
Runtime
-------
 * Preprocessing: _Θ(m)_
 * Matching time: average _Θ(n + m)_
 * Worst case: Θ((n−m)m)

<hr></hr>
<br>
Boyer Moore
===========
<a id="Author" href="https://github.com/WieczorekE">&#128366;view author of this tutorial</a>

Usage
-----

This part of the documentation gives a short introduction into the Boyer Moore string search algorithm which is particulary desined to work on IBioItems.This algorithm searches a query within a longer source. In using two arrays that are created using the bad character rule and the good suffix heuristics the best shifting value is determined. This accounts for higher shifts which prevents unnecessary comparisons to save time.
*)


(**
The following code will return an array created out of all the proteins that are contained in the fasta file. First, the Symbols in the file get converted 
into amino acids and then they get concatenated and transformed into an array.
*)

let chlamyproteins = 
    let converter letters : BioList.BioList<AminoAcid> = 
        letters
            |> Seq.map ( fun x -> match (BioFSharp.AminoAcids.charToParsedAminoAcidChar x) with
                                  | ParsedAminoAcidChar.StandardCodes  v -> v
                                  | ParsedAminoAcidChar.AmbiguityCodes v -> v
                                  | ParsedAminoAcidChar.GapTer         v -> v
                                  | ParsedAminoAcidChar.NoAAChar       v -> failwith "No AA Char given")
            |> Seq.toList

    FileIO.readFile (__SOURCE_DIRECTORY__ + "/data/Proteins.fasta")
    |> FastA.fromFileEnumerator (converter)
    |> Seq.map (fun item -> item.Sequence |> Seq.toArray)
    |> Seq.concat
    |> Seq.toArray


(**The following code will return the first occurence of the searched query within the source.*)
BoyerMoore.findFirst chlamyproteins chlamyproteins.[16988..17978]

(**The following code will return the first occurence of the query after a given staring point.*)
BoyerMoore.findFrom 7 chlamyproteins chlamyproteins.[16988..17978]

(**The following code will return the all the occurence of the query within the given source.*)
BoyerMoore.findAll chlamyproteins chlamyproteins.[16988..17978]

(**
Runtime
-------
 * Preprocessing: _Θ(m+k)_
 * Matching time: best _Ω(n/m)_
 * Worst Case: _O(nm)_

Speed Comparison
================
Coming soon...
*)