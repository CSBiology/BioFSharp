#### 1.0.02 - Wednesday, February 19, 2020
 * **BioFSharp.BioDB:**
    * Fix FaTool OData model URL
 * **BioFSharp.Vis**
   * Add Function to save chord plots as html files

#### 1.0.01 - Thursday, October 24, 2019
 * **BioFSharp.Stats:**
    * Major speed improvements for Sailent

#### 1.0.0 - Wednesday, October 23, 2019
Renaming of BioFSharp.Biotools makes this a major version increase, as it is not backwards compatible. Several bugfixes and additions to multiple sub projects:

 * We now have a cool new logo! (See readme)

 * **BioFSharp.BioTools has been renamed to BioFSharp.BioContainers**. This reflects the purpose of the library better than the previous name.
 
 * **BioFSharp.BioContainers**
   * Add thin [LastAlign biocontainer API wrapper](https://github.com/CSBiology/BioFSharp/commit/3304e612ccc0b97aa1625c29619968173168b545)

 * **BioFSharp.IO**
   * Bugfixes:
     * [Fix SOFT parser skipping entities](https://github.com/CSBiology/BioFSharp/commit/cc3a3f9efe898395c59a97e2b66792c4b3970749) (fixes [#72](https://github.com/CSBiology/BioFSharp/issues/72))

 * **BioFSharp.Stats**
   * Sailent is now [faster](https://github.com/CSBiology/BioFSharp/commit/844d2efcdd4b85c9e1c80f0160fb74e72f514a90) and has a [verbose option](https://github.com/CSBiology/BioFSharp/commit/97cd33978142f3cc669c4d1e527d8583456f5bc6) (fixes [#74](https://github.com/CSBiology/BioFSharp/issues/74),[#75](https://github.com/CSBiology/BioFSharp/issues/75))

 * **BioFSharp.ImgP** 
   * Add functionality to [isolate local maxima from frames](https://github.com/CSBiology/BioFSharp/commit/68073759265d086ea6d9b47f30bc3e47c8d3fb60)
   * Add functionality to [get correlation at specified coordinates](https://github.com/CSBiology/BioFSharp/commit/57efb6003254b234fdad2837e5ee666933a4355f)


#### 0.1.02 - Wednesday, September 04, 2019
Several bugfixes and additions to multiple sub projects:
 
 * **BioFSharp** (core):
   * Addition of various Unit tests (see the [issue](https://github.com/CSBiology/BioFSharp/issues/30) to track what has been added since the last version)
   * Bugfixes:
     * [Naming clarification and bugfixes regarding reverse and reverse complements for DNA](https://github.com/CSBiology/BioFSharp/commit/27ab68f84aacd6de01a194dc5542da90a85e59d4) (fixes [#66](https://github.com/CSBiology/BioFSharp/issues/66) and [#65](https://github.com/CSBiology/BioFSharp/issues/65))

 * **BioFSharp.Stats**
   * Bugfixes
     * [Ontology enrichment is now more conservative](https://github.com/CSBiology/BioFSharp/commit/0ed0161d8a203f38ac7171b3213eb7b386adcacf)

#### 0.1.01 - Wednesday, July 31, 2019
Several bugfixes and additions to multiple sub projects:
	
 * **BioFSharp** (core):
	* Add Unit testing for modules:
		Formula
		Mass
		BioArray
	* Changed functionality:
		 * Formula.substract now also returns negative atom counts
		  
 * **BioFSharp.ML**:
	* Additional functionality:
     	* add thin CNTK API

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
