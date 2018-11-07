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
#r "FSharpAux.IO.dll"
open System
open BioFSharp
open BioFSharp.IO
open FSharpAux.IO
open AminoAcids
open Nucleotides
open AminoAcidSymbols
open BioFSharp.Algorithm.StringMatching
(**
<table class="HeadAPI">
<td class="Head"><h1>String matching algorithms</h1></td>
<td class="API">
    <a id="APILink" href="https://csbiology.github.io/BioFSharp/reference/biofsharp-algorithm-patternquery.html" >&#128194;View module documentation</a>
</td>
</table>

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
coming soon...

Speed Comparison
================
Coming soon...
*)