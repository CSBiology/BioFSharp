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
