BioFSharp
=========

An open source bioinformatics toolbox written in F#. <https://csbiology.github.io/BioFSharp/>

Build status:

|Branch|Ubuntu(latest)|Windows|
|---|---|---|
| master|[![Build Status](https://travis-ci.com/CSBiology/BioFSharp.svg?branch=master)](https://travis-ci.com/CSBiology/BioFSharp) | [![Build status](https://ci.appveyor.com/api/projects/status/9a5r4aklmmbykobk/branch/master?svg=true)](https://ci.appveyor.com/project/kMutagene/biofsharp/branch/master) |
| developer|[![Build Status](https://travis-ci.com/CSBiology/BioFSharp.svg?branch=developer)](https://travis-ci.com/CSBiology/BioFSharp) | [![Build status](https://ci.appveyor.com/api/projects/status/9a5r4aklmmbykobk/branch/developer?svg=true)](https://ci.appveyor.com/project/kMutagene/biofsharp/branch/developer) |

Core functionality
------------------

In its core namespace, BioFSharp contains the basic data structures for common biological objects and their modification. Our type modeling starts at chemical elements, abstracts those to form formulas, and finally molecules of high biological relevance such as amino acids and nucleotides. Sequences of these molecules are modelled by BioCollections, which provide extensive functionality for investigating their real life counterparts.

Additionally, core algorithms for biological sequences such as alignments and pattern matching algorithms are implemented.

IO functionality
----------------

The IO namespace aims to make data available and ease further processing. It contains read/write functions for a diverse set of biological file formats such as [Fasta](https://blast.ncbi.nlm.nih.gov/Blast.cgi?CMD=Web&PAGE_TYPE=BlastDocs&DOC_TYPE=BlastHelp), [FastQ](https://www.ncbi.nlm.nih.gov/sra/docs/submitformats/#fastq-files), [GeneBank](https://www.ncbi.nlm.nih.gov/Sitemap/samplerecord.html) or [GFF](https://github.com/The-Sequence-Ontology/Specifications/blob/master/gff3.md), as well as helper function for searching on or transforming the input data. Wrappers for commonly used command line tools like [NCBI's Blast](https://www.ncbi.nlm.nih.gov/books/NBK153387/) assure interoperability with an array of existing bioinformatic workflows

BioDB functionality
-------------------

The BioDB namespace offers API access to powerful popular databases like [GEO](https://www.ncbi.nlm.nih.gov/geo/) and [EBI(including SwissProt/Expasy)](https://www.ebi.ac.uk/). We additionally provide an API access for [FATool](http://iomiqsweb1.bio.uni-kl.de/), a webservice by our workgroup for querying functional annotations of? proteins.

Documentation
-------------

Functions, types and Classes contained in BioFSharp come with short explanatory description, which can be found in the [API Reference](https://csbiology.github.io/BioFSharp/reference/index.html).

More indepth explanations, tutorials and general information about the project can be found [here](http://csbiology.github.io/BioFSharp).

The documentation and tutorials for this library are automatically generated (using the F# Formatting) from *.fsx and *.md files in the docs folder. If you find a typo, please submit a pull request!