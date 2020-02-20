
![Data model](docsrc/files/img/Logo_large.png)




BioFSharp is an open source bioinformatics and computational biology toolbox written in F#. <https://csbiology.github.io/BioFSharp/>

[![Gitter](https://badges.gitter.im/CSBiology/BioFSharp.svg)](https://gitter.im/CSBiology/BioFSharp?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)
[![Generic badge](https://img.shields.io/badge/Made%20with-FSharp-rgb(1,143,204).svg)](https://shields.io/)
![GitHub contributors](https://img.shields.io/github/contributors/CSBiology/BioFSharp)


|Branch|Linux Mono (Xenial)|Linux .Net Core only (Bionic Beaver)|Windows|
|---|---|---|---|
| master | [![Build Status](https://travis-ci.com/CSBiology/BioFSharp.svg?branch=master)](https://travis-ci.com/CSBiology/BioFSharp) | [![Build status](https://ci.appveyor.com/api/projects/status/9a5r4aklmmbykobk/branch/master?svg=true)](https://ci.appveyor.com/project/kMutagene/biofsharp/branch/master) | [![Build status](https://ci.appveyor.com/api/projects/status/9a5r4aklmmbykobk/branch/master?svg=true)](https://ci.appveyor.com/project/kMutagene/biofsharp/branch/master) |
| developer | [![Build Status](https://travis-ci.com/CSBiology/BioFSharp.svg?branch=developer)](https://travis-ci.com/CSBiology/BioFSharp) | [![Build status](https://ci.appveyor.com/api/projects/status/9a5r4aklmmbykobk/branch/developer?svg=true)](https://ci.appveyor.com/project/kMutagene/biofsharp/branch/developer) |[![Build status](https://ci.appveyor.com/api/projects/status/9a5r4aklmmbykobk/branch/developer?svg=true)](https://ci.appveyor.com/project/kMutagene/biofsharp/branch/developer) |



Core functionality
------------------

In its core namespace, BioFSharp contains the basic data structures for common biological objects and their modification. Our type modeling starts at chemical elements, abstracts those to form formulas, and finally molecules of high biological relevance such as amino acids and nucleotides. Sequences of these molecules are modelled by BioCollections, which provide extensive functionality for investigating their real life counterparts.

![Data model](https://i.imgur.com/LXBvhmi.png)

Additionally, core algorithms for biological sequences such as alignments and pattern matching algorithms are implemented.

Besides the core functionality, BioFSharp has several namespaces as sub-projects with different scopes:

IO functionality
----------------

The IO namespace aims to make data available and ease further processing. It contains read/write functions for a diverse set of biological file formats such as [Fasta](https://blast.ncbi.nlm.nih.gov/Blast.cgi?CMD=Web&PAGE_TYPE=BlastDocs&DOC_TYPE=BlastHelp), [FastQ](https://www.ncbi.nlm.nih.gov/sra/docs/submitformats/#fastq-files), [GeneBank](https://www.ncbi.nlm.nih.gov/Sitemap/samplerecord.html) or [GFF](https://github.com/The-Sequence-Ontology/Specifications/blob/master/gff3.md), as well as helper function for searching on or transforming the input data. Wrappers for commonly used command line tools like [NCBI's Blast](https://www.ncbi.nlm.nih.gov/books/NBK153387/) assure interoperability with an array of existing bioinformatic workflows

BioDB functionality
-------------------

The BioDB namespace offers API access to powerful popular databases like [GEO](https://www.ncbi.nlm.nih.gov/geo/) and [EBI(including SwissProt/Expasy)](https://www.ebi.ac.uk/). We additionally provide an API access for [FATool](http://iomiqsweb1.bio.uni-kl.de/), a webservice by our workgroup for querying functional annotations of proteins.

BioContainers functionality
----------------------

The BioContainers namespace is our newest BioFSharp project and we are very excited about it! It is all about making common bioinformatics tools programmatically accessible from F#. 
This is realized by making the containerized tool accessible via the Docker daemon. We wrap some functionality from
[Docker.DotNet](https://github.com/microsoft/Docker.DotNet) to communicate with the docker API while providing extensive, type safe bindings for already 9 tools, including Blast, ClustalO, and TMHMM

ML functionality
----------------

Make your workflow ML ready with BioFSharp.ML. Currently contains helper functionf for [CNTK](https://docs.microsoft.com/en-us/cognitive-toolkit/) and a pre-trained model we used in our [publication about predicting peptide observability](https://www.frontiersin.org/articles/10.3389/fpls.2018.01559/full).

Stats functionality
----------------------

The Stats namespace contains statistical functions with a clear biological focus such as functions for calculating Gene Ontology Enrichments.


Documentation
-------------

Functions, types and Classes contained in BioFSharp come with short explanatory description, which can be found in the [API Reference](https://csbiology.github.io/BioFSharp/reference/index.html).

More indepth explanations, tutorials and general information about the project can be found [here](http://csbiology.github.io/BioFSharp).

The documentation and tutorials for this library are automatically generated (using the F# Formatting) from *.fsx and *.md files in the docs folder. If you find a typo, please submit a pull request!

Contributing
------------

Please refer to the [Contribution guidelines](.github/CONTRIBUTING.md)

Community/Social
----------------
Want to get in touch with us? We recently joined the twitter crowd:

[![Twitter Follow](https://img.shields.io/twitter/follow/BioFSharp.svg?style=social)](https://twitter.com/biofsharp)

[![Twitter Follow](https://img.shields.io/twitter/follow/cs_biology.svg?style=social)](https://twitter.com/cs_biology)