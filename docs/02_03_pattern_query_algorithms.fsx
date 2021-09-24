(**
---
title: Pattern query algorithms
category: Algorithms
categoryindex: 1
index: 3
---
*)

(*** hide ***)

(*** condition: prepare ***)
#r "nuget: Plotly.NET, 2.0.0-preview.8"
#r "nuget: FSharpAux, 1.0.0"
#r "nuget: FSharpAux.IO, 1.0.0"
#r "nuget: FSharp.Stats, 0.4.0"
#r "../bin/BioFSharp/netstandard2.0/BioFSharp.dll"
#r "../bin/BioFSharp.IO/netstandard2.0/BioFSharp.IO.dll"
#r "../bin/BioFSharp.BioContainers/netstandard2.0/BioFSharp.BioContainers.dll"
#r "../bin/BioFSharp.ML/netstandard2.0/BioFSharp.ML.dll"
#r "../bin/BioFSharp.Stats/netstandard2.0/BioFSharp.Stats.dll"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-preview.8"
#r "nuget: FSharpAux, 1.0.0"
#r "nuget: FSharpAux.IO, 1.0.0"
#r "nuget: FSharp.Stats, 0.4.0"
#r "nuget: Plotly.NET.Interactive, 2.0.0-preview.8"
#r "nuget: BioFSharp, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.IO, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.BioContainers, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.ML, {{fsdocs-package-version}}"
#r "nuget: BioFSharp.Stats, {{fsdocs-package-version}}"
#endif // IPYNB

(**
# Pattern query algorithms

[![Binder]({{root}}img/badge-binder.svg)](https://mybinder.org/v2/gh/plotly/Plotly.NET/gh-pages?filepath={{fsdocs-source-basename}}.ipynb)&emsp;
[![Script]({{root}}img/badge-script.svg)]({{root}}{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

*Summary:* This example shows how to use different pattern query algorithms in BioFSharp

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
open BioFSharp

///source containing the query pattern at positions 0,10 and 17 
let source = "AAABBBBBBBAAABBBBAAA" |> BioArray.ofAminoAcidString

///query pattern to find in the source
let queryPattern = "AAA"|> BioArray.ofAminoAcidString

(**
## The Naive Algorithm

The naive approach to the string matching problem is walking through the source starting from the beginning and checking at each position if the resulting substring equals the query pattern. 
While being inefficient, it may be beneficial to use it in cases where the speed advantage of another algorithm is neglegible or does not outhweigh the additional setup needed (for example if your source and query pattern are really short)

### Usage

No additional setup needed. Just use query pattern and source as input parameters for the respective function

*)

open BioFSharp.Algorithm.StringMatching

Naive.findAll queryPattern source
(***include-it***)

Naive.find queryPattern source
(***include-it***)

Naive.findFrom 12 queryPattern source
(***include-it***)

(**

### Runtime

 * Preprocessing: _None_

 * Matching time: _O(nm)_


## Knuth Morris Pratt

The KMP algorithm makes use of previous match information to determine an amount of skips that can be made until the next position in the source gets examined as a possible match.
To achieve that, a prefix table (or failure function) of the pattern needs to be computed, which determines the amount of skippable elements depending on the previous (partial) match.

### Usage

*)

///prefix table computed from the query pattern
let prefixTable = KnuthMorrisPratt.createPrefixTable queryPattern

KnuthMorrisPratt.findAll prefixTable queryPattern source
(***include-it***)

KnuthMorrisPratt.find prefixTable queryPattern source
(***include-it***)

KnuthMorrisPratt.findFrom prefixTable queryPattern 12 source
(***include-it***)

(**
### Runtime

 * Preprocessing: _Θ(m)_

 * Matching time: average: _Θ(n)_

 * Worst case(if the prefix table results in no possible skips): _Θ(mn)_ 

## Rabin Karp

The RK Algorithm saves time by not comparing every single element of pattern and a substring of the same length from the source. Instead, it compares hashed versions of them.
The RabinKarp module contains two submodules:

 * RKStandard: uses the build in .NET hash function `hash` 

 * CP: uses a cyclic polynomial hash, which is way faster than the built in hash function. Elements must be castable to `uint64` , as they get hashed as a 64 bit unsigned integer and updating the hash uses bitwise operators. 
 Using large query patterns is problematic, as the combined hash may exceed the size of an `uint64`.

### Usage

#### RKStandard
*)

RabinKarp.RKStandard.findAll queryPattern source
(***include-it***)

RabinKarp.RKStandard.find queryPattern source
(***include-it***)

RabinKarp.RKStandard.findFrom 12 queryPattern source
(***include-it***)

(**
#### CP

As the AminoAcid type cannot be casted to an `uint64`, a conversion to their one character code is needed *)

///source converted to char array 
let source' = source |> Array.map (fun x -> (x :> IBioItem).Symbol)

///query pattern converted to char array
let queryPattern' = queryPattern |> Array.map (fun x -> (x :> IBioItem).Symbol)

RabinKarp.CP.findAll queryPattern' source'
(***include-it***)

RabinKarp.CP.find queryPattern' source'
(***include-it***)

RabinKarp.CP.findFrom 12 queryPattern' source'
(***include-it***)

(**
### Using your own hash functions
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
### Runtime

 * Preprocessing: _Θ(m)_
 * Matching time: average _Θ(n + m)_
 * Worst case: Θ((n−m)m)

Boyer Moore
===========
coming soon...

Speed Comparison
================
Coming soon...
*)