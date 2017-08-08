(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
BioFSharp
=========

BioFSharp aims to be a user-friendly library for Bioinformatics written in F#. It contains the basic data 
structures for common biological objects like amino acids and nucleotides based on chemical formulas and chemical elements. 

Furthermore, we provide a variety of parsers for many biological file formats, visualization tools for plots and networks and a variety of algorithms suited for bioinformatic workflows. 

F# being part of the .NET ecosystem provides seamless interoperability with other .NET languages and makes it easy to use in for example C#. 

BioFSharp consist of a core project and the following sub-projects, which are maintained and documented sperately:

 * [FSharp.Stats](https://github.com/CSBiology/FSharp.Stats) contains libraries for numerical computations including linear algebra, curve fitting and statistical measures.


 * [FSharp.Care](https://github.com/CSBiology/FSharp.Care) extends F# collections with auxilliary functions


<br></br>
<hr>

Setting up BioFSharp
====================
_Note: in the near future, we will provide a NuGet package_ 

 * Clone or download [FSharp.Stats](https://github.com/CSBiology/FSharp.Stats) and [FSharp.Care](https://github.com/CSBiology/FSharp.Care).

 * Clone or download [BioFSharp](https://github.com/CSBiology/BioFSharp) 

As long as the repositories are in the same folder, you can now build BioFSharp in Visual Studio. 
Alternatively, open the terminal application of your choice, set your current directory to the BioFSharp root folder and build it by 
using the `build` command. (this will execute build.cmd, which starts an automated build process orchestrated by [paket](https://github.com/fsprojects/Paket) and [FAKE](https://github.com/fsharp/FAKE))

<br></br>
<hr>

CSBlog
======
Tutorials that have a more general scope or cover several topics at once are posted at our blog [CSBlog](https://csbiology.github.io/CSBlog/). It is a tool for us to document
and share our workflows. It may be worth to check if your specific workflow is covered there to get an entry point for using BioFSharp for it.


<br></br>
<hr>

Samples & documentation
=======================

The library comes with comprehensible documentation. All samples and tutorial snippets are written in F#, it should however be no problem to adjust the syntax and use them in other .NET languages.
Tutorials are automatically generated from `*.fsx` files in the [content folder](https://github.com/CSBiology/BioFSharp/tree/master/docs/content). 
The API reference is automatically generated from Markdown comments in the library implementation.

You can find the tutorials and the API reference in the sidebar to the right. 

Documentation and tutorials for our sub-projects can be found here:
 
 * [FSharp.Stats](https://csbiology.github.io/FSharp.Stats/)

 * [FSharp.Care](http://csbiology.github.io/FSharp.Care/)

 
*)