#### 0.1.0 - Thursday, May 30, 2019
Several bugfixes and additions to multiple sub projects:

* **BioFSharp** (core):
    * Additional functionality:
      * [GravyScore](https://github.com/CSBiology/BioFSharp/commit/209c3497e3cdb1db56a0675e0f2a76634a6dbe7a) (Grand Average of Hydropathy) as additional amino acid property

* **BioFSharp.IO**:
    * Additional functionality:
      * [GAF Parser](https://github.com/CSBiology/BioFSharp/commit/cbba6a41a9b239e26467f32048aaec3335373faf) for GO Annotation file format: [Gene Association File](http://geneontology.org/docs/go-annotation-file-gaf-format-2.0/)
      * [Uniprot style Fasta header parser](https://github.com/CSBiology/BioFSharp/commit/f2a16aaa2456b0c431f6d50d0f78a12834671e97)
      * [FastA to GFF3 converter functions](https://github.com/CSBiology/BioFSharp/commit/2cdd3537398040e1508736bd734b22a67a7c46e7)
      * [GFF3 Pretty Printer](https://github.com/CSBiology/BioFSharp/commit/eaaa59fbd382721e75fbb9c6558b0ba2ff6afb00) 

    * BugFixes:
      * [Fix OboParser](https://github.com/CSBiology/BioFSharp/commit/0354c9c13e7a4692f2ab61b80ef86ac8f5bd83c3) just taking the last occurence of the alt_id keyword and discards previous ones.
      * Fix [Fasta](https://github.com/CSBiology/BioFSharp/commit/aff8eff849deb1cca411faf3c640d53f6e410497) and [GFF3](https://github.com/CSBiology/BioFSharp/commit/d0f059ab899c715a37b7f50318292c8a81f18dd9) writers appending to files instead of recreating

* **BioFSharp.BioTools**:
    * Additional functionality:
      * [TMHMM biocontainer API wrapper](https://github.com/CSBiology/BioFSharp/commit/f11cb122df29ccaa0809d3c3c951294a1b645e0f) ([TMHMM](http://www.cbs.dtu.dk/services/TMHMM/) 2.0c predicts transmembrane helices in proteins)
      * [FastP and Hera biocontainer API wrapper](https://github.com/CSBiology/BioFSharp/commit/28b7654d57824bcdfdb8bae16af2f0f706ed60ad)
      * [IntaRNA biocontainer API wrapper](https://github.com/CSBiology/BioFSharp/commit/a659496179cd754fbea2fe9ef4030544a35eb68b) ([IntaRNA](https://github.com/BackofenLab/IntaRNA) is a tool for prediction of various nucleotide interactions)

    * BugFixes:
      * Fix Stream entry closed error in [BioContainer.tarOfStream()](https://github.com/CSBiology/BioFSharp/commit/20f8973ea717208627ef5a7ea0b72cbaecb4103c)

* **BioFSharp.ImgP**:
    * Additional functionality:
      * [update 3DCWT correlation calculation to loop](https://github.com/CSBiology/BioFSharp/commit/0b4ffe93755d915da64f4231199b0ec54d4d6c4d)

    * Bugfixes:
      * [fix height adjustment in ricker](https://github.com/CSBiology/BioFSharp/commit/abab82be1ac9fa0c540acfb5f3ccc6bd5143df1f)
      * [fix Ricker values for discrete time points](https://github.com/CSBiology/BioFSharp/commit/2bb6bb2b67ea43df2d9fe970bd1445e568df53d4)

#### 0.0.16 - Thursday, March 7, 2019
 * Fix template files for all nuget packages: 
   * use correct paths for binaries
   * update project descriptions
   * use type project to infer dependencies

#### 0.0.15 - Thursday, March 7, 2019
 * BioFSharp.ML - project introduction:
   * Usage of Microsoft's CNTK library with a biological focus:
   * This project comes with working CNTK integration:
      * necessary dependencies to use CNTK and its unmanaged libraries
      * CNTK loadscript: an easy way to load all dependencies of CNTK. load the script and use the resolveCNTKDependencies() function.
   * Built-in deep neural network 'd::ppop' ([publication](https://www.frontiersin.org/articles/10.3389/fpls.2018.01559/full))
     * predict peptide observability in mass spectrometry
     * Classification: functions to determine peptide feature vectors as input for dppop
     * Prediction: functions to predict peptide observability using the feature vectors prepared in Classification.
     * NonPlant and Plant models as embedded resources: the two models used in the original Web API. Additionally load custom models.
 * Othert additions:
   * BioFSharp.BioTools:
     * Integration of new tools as biocontainer APIs:
       * Hera
       * FastP
       * ClustalO
       * HMMER (experimental):
         * hmmbuild
         * hmmalign
         * hmmsearch
         * hmmscan
         * hmmemit
         * hmmpress
         * hmmconvert

#### 0.0.14 - Tuesday, February 12, 2019
 * Addition of blast biocontainer support for makeblastdb, blastp, blastn with full parameter wrapping
 * Extension of BioContainer functionality:
   * Add windows/unix path conversions and subpath matching to MounInfo
   * Add execReturnAsync (returns stdout of docker container) and execAsync (redirects stdout/stderr of container)

#### 0.0.13 - Friday, February 8, 2019
* Addition of the BioTools project, which uses docker.dotnet to use docker images and stream their output to fsi
* Low level wrapping of multiple docker.dotnet functions for F#
* Basic functionality for using docker images from fsi

#### 0.0.12 - Friday, December 28, 2018
* Addition of Pretty Printers, SOFT Parser, GEOFTP functions
* Improvement and refactoring of Modification functionality

#### 0.0.11 - Tuesday, November 2, 2018
* ImgP - project introduction

#### 0.0.1 - Thursday, August 9, 2018
* Initial release
