namespace BioFSharp.BioTools

// adaped from the hmmer usage guide at http://eddylab.org/software/hmmer/Userguide.pdf
// HMMER: biosequence analysis using profile hidden Markov models (http://hmmer.org/)

module HMMER =

    open BioContainer

    module HMMbuild =
        //Usage: hmmbuild [-options] <hmmfile_out> <msafile>

        ///Options for selecting alphabet rather than guessing it:
        //  --amino : input alignment is protein sequence data
        //  --dna   : input alignment is DNA sequence data
        //  --rna   : input alignment is RNA sequence data

        type AlphabetType =
            ///input alignment is protein sequence data
            | AminoAcids
            ///input alignment is DNA sequence data
            | DNA
            ///input alignment is RNA sequence data
            | RNA

            static member make = function
                | AminoAcids->  ["--amino"]
                | DNA       ->  ["--dna"]
                | RNA       ->  ["--rna"]

        ///Alternative model construction strategies:
        //  --fast           : assign cols w/ >= symfrac residues as consensus  [default]
        //  --hand           : manual construction (requires reference annotation)
        //  --symfrac <x>    : sets sym fraction controlling --fast construction  [0.5]
        //  --fragthresh <x> : if L <= x*alen, tag sequence as a fragment  [0.5]

        type ModelConstructionCustom =
            | Fast
            | Manual
            | SymFraction       of float
            | FragmentThreshold of float

            static member make = function
                | Fast                  -> ["--fast"]
                | Manual                -> ["--hand"]
                | SymFraction sf        -> ["--symfrac"; string sf]
                | FragmentThreshold ft  -> ["--fragthresh"; string ft]

        ///Alternative relative sequence weighting strategies:
        //  --wpb     : Henikoff position-based weights  [default]
        //  --wgsc    : Gerstein/Sonnhammer/Chothia tree weights
        //  --wblosum : Henikoff simple filter weights
        //  --wnone   : don't do any relative weighting; set all to 1
        //  --wgiven  : use weights as given in MSA file
        //  --wid <x> : for --wblosum: set identity cutoff  [0.62]  (0<=x<=1)

        type RelativeSequenceWeightingCustom =
            ///Henikoff position-based weights
            | PositionBased
            ///Gerstein/Sonnhammer/Chothia tree weights
            | GSC
            ///Henikoff simple filter weights
            | Blosum
            ///for Blosum: set identity cutoff (0<=x<=1)
            | BlosumIdentityCutoff of float
            ///don't do any relative weighting; set all to 1
            | NoRelativeWeighting
            ///use weights as given in MSA file
            | Given 
            
            static member make = function
                | PositionBased         -> ["--wpb"]
                | GSC                   -> ["--wgsc"]
                | Blosum                -> ["--wblosum"]
                | BlosumIdentityCutoff c-> ["--wid"; string c]
                | NoRelativeWeighting   -> ["--wnone"]
                | Given                 -> ["--wgiven"]

        ///Alternative effective sequence weighting strategies:
        //  --eent       : adjust eff seq # to achieve relative entropy target  [default]
        //  --eclust     : eff seq # is # of single linkage clusters
        //  --enone      : no effective seq # weighting: just use nseq
        //  --eset <x>   : set eff seq # for all models to <x>
        //  --ere <x>    : for --eent: set minimum rel entropy/position to <x>
        //  --esigma <x> : for --eent: set sigma param to <x>  [45.0]
        //  --eid <x>    : for --eclust: set fractional identity cutoff to <x>  [0.62]

        type EffectiveSequenceWeightingCustom =
            ///adjust eff seq # to achieve relative entropy target
            | EntropyTargeted
            ///for EntropyTargeted: set minimum rel entropy/position to <x>
            | MinimalEntropy        of float
            ///for EntropyTargeted: set sigma param to <x>  [45.0]
            | EntropySigma          of float
            ///eff seq # is # of single linkage clusters
            | SingleLinkageClusters
            ///for SingleLinkageClusters: set fractional identity cutoff to <x>  [0.62]
            | ClusterIdentityCutoff of float
            ///no effective seq # weighting: just use nseq
            | NoEffectiveWeighting
            ///set eff seq # for all models to <x>
            | Set                   of float

            static member make = function 
                | EntropyTargeted           -> ["--eent"]
                | SingleLinkageClusters     -> ["--eclust"]
                | NoEffectiveWeighting      -> ["--enone"]
                | Set s                     -> ["--eset";   string s]
                | MinimalEntropy me         -> ["--ere";    string me]
                | EntropySigma es           -> ["--esigma"; string es]
                | ClusterIdentityCutoff c   -> ["--eid";    string c]


        ///Alternative prior strategies:
        //  --pnone       : don't use any prior; parameters are frequencies
        //  --plaplace    : use a Laplace +1 prior
        //  --popen <x>   : force gap open prob. (w/ --singlemx, aa default 0.02, nt 0.031)
        //  --pextend <x> : force gap extend prob. (w/ --singlemx, aa default 0.4, nt 0.75)

        type PriorCustom =
            ///don't use any prior; parameters are frequencies
            | NoPrior
            ///use a Laplace +1 prio
            | Laplace
            ///force gap open prob.
            | ForcePGapOpen     of float
            ///force gap extend prob
            | ForcePGapExtend   of float

            static member make = function 
                |NoPrior            -> ["--pnone"]
                |Laplace            -> ["--plaplace"]
                |ForcePGapOpen f    -> ["--popen"; string f]
                |ForcePGapExtend f  -> ["--pextend"; string f]

        type SubstitutionScoreMatrix =
            |PAM30      
            |PAM70 
            |PAM120 
            |PAM240 
            |BLOSUM45 
            |BLOSUM50 
            |BLOSUM62 
            |BLOSUM80 
            |BLOSUM90
            |DNA1

            static member make = function
                |PAM30   -> "PAM30"
                |PAM70   -> "PAM70"
                |PAM120  -> "PAM120"
                |PAM240  -> "PAM240"
                |BLOSUM45-> "BLOSUM45"
                |BLOSUM50-> "BLOSUM50"
                |BLOSUM62-> "BLOSUM62"
                |BLOSUM80-> "BLOSUM80"
                |BLOSUM90-> "BLOSUM90"
                |DNA1    -> "DNA1"

        ///Handling single sequence inputs:
        //  --singlemx   : use substitution score matrix for single-sequence inputs
        //  --mx <s>     : substitution score matrix (built-in matrices, with --singlemx)
        //  --mxfile <f> : read substitution score matrix from file <f> (with --singlemx)
        type SingleSequenceHandlingCustom =
            ///use substitution score matrix for single-sequence inputs
            | UseSubstitutionMatrix
            ///substitution score matrix (built-in matrices, use with UseSubstitutionMatrix)
            | SMatrix       of SubstitutionScoreMatrix
            ///read substitution score matrix from file (use with UseSubstitutionMatrix)
            | SMatrixFile   of string

            static member make = function
                | UseSubstitutionMatrix -> ["--singlemx"]
                | SMatrix s             -> ["--mx"; (SubstitutionScoreMatrix.make s)]
                | SMatrixFile path      -> ["--mxfile"; path]

            static member makeWith (m:MountInfo) =
                let cPath p = (MountInfo.containerPathOf m p)
                function
                | UseSubstitutionMatrix -> ["--singlemx"]
                | SMatrix s             -> ["--mx"; (SubstitutionScoreMatrix.make s)]
                | SMatrixFile path      -> ["--mxfile"; cPath path]

        //Control of E-value calibration:
        //  --EmL <n> : length of sequences for MSV Gumbel mu fit  [200]  (n>0)
        //  --EmN <n> : number of sequences for MSV Gumbel mu fit  [200]  (n>0)
        //  --EvL <n> : length of sequences for Viterbi Gumbel mu fit  [200]  (n>0)
        //  --EvN <n> : number of sequences for Viterbi Gumbel mu fit  [200]  (n>0)
        //  --EfL <n> : length of sequences for Forward exp tail tau fit  [100]  (n>0)
        //  --EfN <n> : number of sequences for Forward exp tail tau fit  [200]  (n>0)
        //  --Eft <x> : tail mass for Forward exponential tail tau fit  [0.04]  (0<x<1)

        type EValueControlCustom = 
            ///length of sequences for MSV Gumbel mu fit
            | MSVLength         of int
            ///number of sequences for MSV Gumbel mu fit
            | MSVNumber         of int
            ///length of sequences for Viterbi Gumbel mu fit
            | ViterbiLength     of int
            ///number of sequences for Viterbi Gumbel mu fit
            | ViterbiNumber     of int
            ///length of sequences for Forward exp tail tau fit
            | ForwardLength     of int
            ///number of sequences for Forward exp tail tau fit
            | ForwardNumber     of int
            /// tail mass for Forward exponential tail tau fit
            | ForwardTailMass   of float

            static member make = function
                | MSVLength         l   -> ["--EmL";string l]
                | MSVNumber         n   -> ["--EmN";string n]
                | ViterbiLength     l   -> ["--EvL";string l]
                | ViterbiNumber     n   -> ["--EvN";string n]
                | ForwardLength     l   -> ["--EfL";string l]
                | ForwardNumber     n   -> ["--EfN";string n]
                | ForwardTailMass   m   -> ["--Eft";string m]

        ///Other options:
        //  --cpu <n>          : number of parallel CPU workers for multithreads
        //  --stall            : arrest after start: for attaching debugger to process
        //  --informat <s>     : assert input alifile is in format <s> (no autodetect)
        //  --seed <n>         : set RNG seed to <n> (if 0: one-time arbitrary seed)  [42]
        //  --w_beta <x>       : tail mass at which window length is determined
        //  --w_length <n>     : window length
        //  --maxinsertlen <n> : pretend all inserts are length <= <n>

        type MiscellaneousCustom =
            ///number of parallel CPU workers for multithreads
            | Threads               of int
            ///arrest after start: for attaching debugger to process
            | Stall
            ///assert input alifile is in format <s> (no autodetect)
            | AssertInFormat        of string
            ///set RNG seed to <n> (if 0: one-time arbitrary seed)
            | Seed                  of int
            ///tail mass at which window length is determined
            | TailMass              of float
            ///window length
            | WindowLength          of int
            ///pretend all inserts are length <= <n>
            | MaxInsertionLength    of int

            static member make = function
                | Threads t             -> ["--cpu"; string t]
                | Stall                 -> ["--stall"]
                | AssertInFormat f      -> ["--informat"; f]
                | Seed s                -> ["--seed"; string s]
                | TailMass m            -> ["--w_beta"; string m]
                | WindowLength wl       -> ["--w_length"; string wl]
                | MaxInsertionLength l  -> ["--maxinsertlen"; string l]

        //Basic options:
        //  -h     : show brief help on version and usage
        //  -n <s> : name the HMM <s>
        //  -o <f> : direct summary output to file <f>, not stdout
        //  -O <f> : resave annotated, possibly modified MSA to file <f>

        type HMMbuildParams =
            ///Multiple sequence alignment input file
            | InputMSAFile                   of string
            ///HMM file to save the generated HMMs to
            | OutputHMMFile                  of string
            ///name the HMM
            | Name                          of string
            ///direct summary output to file <f>, not stdout
            | SummaryToFile                 of string
            ///resave annotated, possibly modified MSA to file
            | ResaveFile                    of string
            ///Specify the alphabet used in the alignment file
            | Alphabet                      of AlphabetType list
            ///Alternative model construction strategies:
            | ModelConstruction             of ModelConstructionCustom list
            ///Alternative relative sequence weighting strategies
            | RelativeSequenceWeighting     of RelativeSequenceWeightingCustom list
            ///Alternative effective sequence weighting strategies
            | EffectiveSequenceWeighting    of EffectiveSequenceWeightingCustom list
            ///Alternative prior strategies
            | Prior                         of PriorCustom list
            ///Handling single sequence inputs
            | SingleSequenceHandling        of SingleSequenceHandlingCustom list
            ///Control of E-value calibration
            |EValueControl                  of EValueControlCustom list
            ///Other options
            |Miscellaneous                  of MiscellaneousCustom list

            static member makeCmd = ()

            static member makeCmdWith (m:MountInfo) =
                let cPath p = (MountInfo.containerPathOf m p)
                function 
                    | InputMSAFile path                     -> [cPath path]
                    | OutputHMMFile path                    -> [cPath path]
                    | Name n                                -> ["-n";n]
                    | SummaryToFile path                    -> ["-o";cPath path]
                    | ResaveFile path                       -> ["-O";cPath path]
                    | Alphabet aList                        -> aList    |> List.map AlphabetType.make |> List.concat
                    | ModelConstruction mcList              -> mcList   |> List.map ModelConstructionCustom.make                |> List.concat
                    | RelativeSequenceWeighting rswList     -> rswList  |> List.map RelativeSequenceWeightingCustom.make        |> List.concat
                    | EffectiveSequenceWeighting eswList    -> eswList  |> List.map EffectiveSequenceWeightingCustom.make       |> List.concat
                    | Prior pList                           -> pList    |> List.map PriorCustom.make                            |> List.concat
                    | SingleSequenceHandling sshList        -> sshList  |> List.map (SingleSequenceHandlingCustom.makeWith m)   |> List.concat
                    | EValueControl evcList                 -> evcList  |> List.map EValueControlCustom.make                    |> List.concat
                    | Miscellaneous mList                   -> mList    |> List.map MiscellaneousCustom.make                    |> List.concat

        let runHMMbuildAsync (bcContext:BioContainer.BcContext) (opt:HMMbuildParams list) = 
            let cmds = (opt |> List.map (HMMbuildParams.makeCmdWith bcContext.Mount))
            let tp = "hmmbuild"::(cmds |> List.concat)

            printfn "Starting process blastn\r\nparameters:"
            cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

            async {
                    let! res = BioContainer.execAsync bcContext tp           
                    return res
 
            }
        
        let runHMMbuild (bcContext:BioContainer.BcContext) (opt:HMMbuildParams list) = 
            runHMMbuildAsync bcContext opt
            |> Async.RunSynchronously

