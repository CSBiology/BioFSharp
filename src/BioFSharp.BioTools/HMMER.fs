namespace BioFSharp.BioTools

// adaped from the hmmer usage guide at http://eddylab.org/software/hmmer/Userguide.pdf
// HMMER: biosequence analysis using profile hidden Markov models (http://hmmer.org/)

module HMMER =

    open BioContainer

    //Common parameter types for multiple CLI tools
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

    ///hmmbuild - construct profiles from multiple sequence alignments
    module HMMbuild =
        //Usage: hmmbuild [-options] <hmmfile_out> <msafile>

        ///Options for selecting alphabet rather than guessing it:
        //  --amino : input alignment is protein sequence data
        //  --dna   : input alignment is DNA sequence data
        //  --rna   : input alignment is RNA sequence data


        ///Alternative model construction strategies:
        //  --fast           : assign cols w/ >= symfrac residues as consensus  [default]
        //  --hand           : manual construction (requires reference annotation)
        //  --symfrac <x>    : sets sym fraction controlling --fast construction  [0.5]
        //  --fragthresh <x> : if L <= x*alen, tag sequence as a fragment  [0.5]

        type ModelConstructionOptions =
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

        type RelativeSequenceWeightingOptions =
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

        type EffectiveSequenceWeightingOptions =
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

        type PriorOptions =
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
        type SingleSequenceHandlingOptions =
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

        type EValueControlOptions = 
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

        type MiscellaneousOptions =
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
            | ModelConstruction             of ModelConstructionOptions list
            ///Alternative relative sequence weighting strategies
            | RelativeSequenceWeighting     of RelativeSequenceWeightingOptions list
            ///Alternative effective sequence weighting strategies
            | EffectiveSequenceWeighting    of EffectiveSequenceWeightingOptions list
            ///Alternative prior strategies
            | Prior                         of PriorOptions list
            ///Handling single sequence inputs
            | SingleSequenceHandling        of SingleSequenceHandlingOptions list
            ///Control of E-value calibration
            | EValueControl                 of EValueControlOptions list
            ///Other options
            | Miscellaneous                 of MiscellaneousOptions list

            static member makeCmd =
                function 
                    | InputMSAFile path                     -> [path]
                    | OutputHMMFile path                    -> [path]
                    | Name n                                -> ["-n";n]
                    | SummaryToFile path                    -> ["-o";path]
                    | ResaveFile path                       -> ["-O";path]
                    | Alphabet aList                        -> aList    |> List.map AlphabetType.make |> List.concat
                    | ModelConstruction mcList              -> mcList   |> List.map ModelConstructionOptions.make            |> List.concat
                    | RelativeSequenceWeighting rswList     -> rswList  |> List.map RelativeSequenceWeightingOptions.make    |> List.concat
                    | EffectiveSequenceWeighting eswList    -> eswList  |> List.map EffectiveSequenceWeightingOptions.make   |> List.concat
                    | Prior pList                           -> pList    |> List.map PriorOptions.make                        |> List.concat
                    | SingleSequenceHandling sshList        -> sshList  |> List.map SingleSequenceHandlingOptions.make       |> List.concat
                    | EValueControl evcList                 -> evcList  |> List.map EValueControlOptions.make                |> List.concat
                    | Miscellaneous mList                   -> mList    |> List.map MiscellaneousOptions.make                |> List.concat

            static member makeCmdWith (m:MountInfo) =
                let cPath p = (MountInfo.containerPathOf m p)
                function 
                    | InputMSAFile path                     -> [cPath path]
                    | OutputHMMFile path                    -> [cPath path]
                    | Name n                                -> ["-n";n]
                    | SummaryToFile path                    -> ["-o";cPath path]
                    | ResaveFile path                       -> ["-O";cPath path]
                    | Alphabet aList                        -> aList    |> List.map AlphabetType.make |> List.concat
                    | ModelConstruction mcList              -> mcList   |> List.map ModelConstructionOptions.make                |> List.concat
                    | RelativeSequenceWeighting rswList     -> rswList  |> List.map RelativeSequenceWeightingOptions.make        |> List.concat
                    | EffectiveSequenceWeighting eswList    -> eswList  |> List.map EffectiveSequenceWeightingOptions.make       |> List.concat
                    | Prior pList                           -> pList    |> List.map PriorOptions.make                            |> List.concat
                    | SingleSequenceHandling sshList        -> sshList  |> List.map (SingleSequenceHandlingOptions.makeWith m)   |> List.concat
                    | EValueControl evcList                 -> evcList  |> List.map EValueControlOptions.make                    |> List.concat
                    | Miscellaneous mList                   -> mList    |> List.map MiscellaneousOptions.make                    |> List.concat

        let runHMMbuildAsync (bcContext:BioContainer.BcContext) (opt:HMMbuildParams list) = 
            //Usage: hmmbuild [-options] <hmmfile_out> <msafile> -> filter for in/out and move them to the end
            let msa     = 
                opt 
                |> List.filter (fun p -> match p with |InputMSAFile _ -> true |_ -> false)
                |> fun x -> if List.isEmpty x then
                                failwith "no input sequence given"
                            else 
                                HMMbuildParams.makeCmdWith bcContext.Mount x.[0]

            let hmm     = 
                opt 
                |> List.filter (fun p -> match p with |OutputHMMFile _ -> true |_ -> false)
                |> fun x -> if List.isEmpty x then
                                failwith "no output sequence given"
                            else 
                                HMMbuildParams.makeCmdWith bcContext.Mount x.[0]

            let options = opt |> List.filter (fun p -> match p with |InputMSAFile _ |OutputHMMFile _ -> false |_ -> true)
            let cmds = (options |> List.map (HMMbuildParams.makeCmdWith bcContext.Mount))
            let tp = ("hmmbuild"::(cmds |> List.concat))@hmm@msa

            printfn "Starting process hmmbuildn\r\nparameters:"
            printfn "%s" msa.[0]
            printfn "%s" hmm.[0]
            cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

            async {
                    let! res = BioContainer.execAsync bcContext tp           
                    return res
 
            }
        
        let runHMMbuild (bcContext:BioContainer.BcContext) (opt:HMMbuildParams list) = 
            runHMMbuildAsync bcContext opt
            |> Async.RunSynchronously

    ///hmmalign - align sequences to a profile
    module HMMalign =
        //Usage: hmmalign [-options] <hmmfile> <seqfile>

        //Basic options:
        //  -h     : show brief help on version and usage
        //  -o <f> : output alignment to file <f>, not stdout

        //Less common options:
        //  --mapali <f>    : include alignment in file <f> (same ali that HMM came from)
        //  --trim          : trim terminal tails of nonaligned residues from alignment
        //  --amino         : assert <seqfile>, <hmmfile> both protein: no autodetection
        //  --dna           : assert <seqfile>, <hmmfile> both DNA: no autodetection
        //  --rna           : assert <seqfile>, <hmmfile> both RNA: no autodetection
        //  --informat <s>  : assert <seqfile> is in format <s>: no autodetection
        //  --outformat <s> : output alignment in format <s>  [Stockholm]

        type HMMalignParams =
            ///HMM input file
            | InputHMMFile          of string
            ///Input sequence file to align.
            ///Sequence input formats include: FASTA, EMBL, GenBank, UniProt
            | InputSequenceFile     of string
            ///output alignment to file not stdout
            ///Alignment output formats include: Stockholm, Pfam, A2M, PSIBLAST
            | SummaryToFile         of string
            ///assert alphabet to both input hmm and seqfile
            | Alphabet              of AlphabetType list
            ///assert <seqfile> is in format <s>: no autodetection
            | SequenceFileFormat    of string
            ///output alignment in format <s>
            | OutFormat             of string
            ///trim terminal tails of nonaligned residues from alignment
            | Trim
            ///include alignment in file <f> (same ali that HMM came from)
            | MapAlignmentToFile    of string

            static member makeCmd = function
                | InputHMMFile path         -> [path]
                | InputSequenceFile path    -> [path]
                | SummaryToFile path        -> ["-o";path]
                | Alphabet aList            -> aList |> List.map AlphabetType.make |> List.concat
                | SequenceFileFormat f      -> ["--informat" ;f]
                | OutFormat f               -> ["--outformat" ;f]
                | Trim                      -> ["--trim"]
                | MapAlignmentToFile path   -> ["--mapali"; path]

            static member makeCmdWith (m:MountInfo) =
                let cPath p = (MountInfo.containerPathOf m p)
                function
                | InputHMMFile path         -> [cPath path]
                | InputSequenceFile path    -> [cPath path]
                | SummaryToFile path        -> ["-o";cPath path]
                | Alphabet aList            -> aList |> List.map AlphabetType.make |> List.concat
                | SequenceFileFormat f      -> ["--informat" ;f]
                | OutFormat f               -> ["--outformat" ;f]
                | Trim                      -> ["--trim"]
                | MapAlignmentToFile path   -> ["--mapali"; cPath path]



        let runHMMalignAsync (bcContext:BioContainer.BcContext) (opt:HMMalignParams list) = 
            //Usage: hmmbuild [-options] <hmmfile_out> <msafile> -> filter for in/out and move them to the end
            let hmm     = 
                opt 
                |> List.filter (fun p -> match p with |InputHMMFile _ -> true |_ -> false)
                |> fun x -> if List.isEmpty x then
                                failwith "no input hmm given"
                            else 
                                HMMalignParams.makeCmdWith bcContext.Mount x.[0]

            let seqFile     = 
                opt 
                |> List.filter (fun p -> match p with |InputSequenceFile _ -> true |_ -> false)
                |> fun x -> if List.isEmpty x then
                                failwith "no input sequence file given"
                            else 
                                HMMalignParams.makeCmdWith bcContext.Mount x.[0]

            let options = opt |> List.filter (fun p -> match p with |InputHMMFile _ |InputSequenceFile _ -> false |_ -> true)
            let cmds = (options |> List.map (HMMalignParams.makeCmdWith bcContext.Mount))
            let tp = ("hmmalign"::(cmds |> List.concat))@hmm@seqFile

            printfn "Starting process hmmalignn\r\nparameters:"
            printfn "%s" hmm.[0]
            printfn "%s" seqFile.[0]
            cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

            async {
                    let! res = BioContainer.execAsync bcContext tp           
                    return res
 
            }
        ///Perform a multiple sequence alignment of all the sequences in seqfile by aligning
        ///them individually to the profile HMM in hmmfile. The new alignment is output to
        ///stdout.
        let runHMMalign (bcContext:BioContainer.BcContext) (opt:HMMalignParams list) =
            runHMMalignAsync bcContext opt
            |> Async.RunSynchronously

    ///hmmsearch - search profile(s) against a sequence database
    module HMMsearch =
        
        ///Options directing output:
        //  -o <f>           : direct output to file <f>, not stdout
        //  -A <f>           : save multiple alignment of all hits to file <f>
        //  --tblout <f>     : save parseable table of per-sequence hits to file <f>
        //  --domtblout <f>  : save parseable table of per-domain hits to file <f>
        //  --pfamtblout <f> : save table of hits and domains to file, in Pfam format <f>
        //  --acc            : prefer accessions over names in output
        //  --noali          : don't output alignments, so output is smaller
        //  --notextw        : unlimit ASCII text output line width
        //  --textw <n>      : set max width of ASCII text output lines  [120]  (n>=120)

        type OutputDirectionsOptions =
            ///direct output to file <f>, not stdout
            | OutputToFile              of string
            ///save multiple alignment of all hits to file <f>
            | AllHitsToFile             of string
            ///save parseable table of per-sequence hits to file <f>
            | HitsToPerSequenceTable    of string
            ///save parseable table of per-domain hits to file <f>
            | HitsToPerDomainTable      of string
            ///save table of hits and domains to file, in Pfam format <f>
            | HitsToPfam                of string
            ///prefer accessions over names in output
            | PreferAccessionsOverNames
            ///don't output alignments, so output is smaller
            | NoAlignments
            ///unlimit ASCII text output line width
            | UnlimitedTextLineWidth
            ///set max width of ASCII text output lines
            | MaxTextLineWidth          of int

            static member make = 
                function
                | OutputToFile path             -> ["-o"; path]
                | AllHitsToFile path            -> ["-A"; path]
                | HitsToPerSequenceTable path   -> ["--tblout"; path]
                | HitsToPerDomainTable path     -> ["--domtblout"; path]
                | HitsToPfam path               -> ["--pfamtblout"; path]
                | PreferAccessionsOverNames     -> ["--acc"]
                | NoAlignments                  -> ["--noali"]
                | UnlimitedTextLineWidth        -> ["--notextw "]
                | MaxTextLineWidth lw           -> ["--textw"]

            static member makeWith (m:MountInfo) =
                let cPath p = (MountInfo.containerPathOf m p)
                function
                | OutputToFile path             -> ["-o"; cPath path]
                | AllHitsToFile path            -> ["-A"; cPath path]
                | HitsToPerSequenceTable path   -> ["--tblout"; cPath path]
                | HitsToPerDomainTable path     -> ["--domtblout"; cPath path]
                | HitsToPfam path               -> ["--pfamtblout"; cPath path]
                | PreferAccessionsOverNames     -> ["--acc"]
                | NoAlignments                  -> ["--noali"]
                | UnlimitedTextLineWidth        -> ["--notextw "]
                | MaxTextLineWidth lw           -> ["--textw"]

        ///Options controlling reporting thresholds:
        //  -E <x>     : report sequences <= this E-value threshold in output  [10.0]  (x>0)
        //  -T <x>     : report sequences >= this score threshold in output
        //  --domE <x> : report domains <= this E-value threshold in output  [10.0]  (x>0)
        //  --domT <x> : report domains >= this score cutoff in output

        type ReportingThresholdOptions =
            ///report sequences <= this E-value threshold in output
            | MaxSequenceEvalue of float
            ///report sequences >= this score threshold in output
            | MinSequenceScore  of float
            ///report domains <= this E-value threshold in output
            | MaxDomainEvalue   of float
            ///report domains >= this score cutoff in output
            | MinDomainScore    of float

            static member make = 
                function
                | MaxSequenceEvalue t   -> ["-E"; string t]
                | MinSequenceScore  t   -> ["-T"; string t]
                | MaxDomainEvalue   t   -> ["--domE"; string t]
                | MinDomainScore    t   -> ["--domT"; string t]

        ///Options controlling inclusion (significance) thresholds:
        //  --incE <x>    : consider sequences <= this E-value threshold as significant
        //  --incT <x>    : consider sequences >= this score threshold as significant
        //  --incdomE <x> : consider domains <= this E-value threshold as significant
        //  --incdomT <x> : consider domains >= this score threshold as significant

        type InclusionThresholdOptions =
            ///consider sequences <= this E-value threshold as significant
            | MaxSequenceEvalue of float
            ///consider sequences >= this score threshold as significant 
            | MinSequenceScore  of float
            ///consider domains <= this E-value threshold as significant
            | MaxDomainEvalue   of float
            ///consider domains >= this score threshold as significant
            | MinDomainScore    of float

            static member make = 
                function
                | MaxSequenceEvalue t   -> ["-incE"; string t]
                | MinSequenceScore  t   -> ["-incT"; string t]
                | MaxDomainEvalue   t   -> ["--incdomE"; string t]
                | MinDomainScore    t   -> ["--incdomT"; string t]
        
        ///Options controlling model-specific thresholding:
        //  --cut_ga : use profile's GA gathering cutoffs to set all thresholding
        //  --cut_nc : use profile's NC noise cutoffs to set all thresholding
        //  --cut_tc : use profile's TC trusted cutoffs to set all thresholding

        type ModelSpecificThresholdOptions =
            ///use profile's GA gathering cutoffs to set all thresholding
            | GatheringCutoff
            ///use profile's NC noise cutoffs to set all thresholding
            | NoiseCutoff
            ///use profile's TC trusted cutoffs to set all thresholding
            | TrustedCutoff

            static member make =
                function
                | GatheringCutoff   -> ["--cut_ga"]
                | NoiseCutoff       -> ["--cut_nc"]
                | TrustedCutoff     -> ["--cut_tc"]

        ///Options controlling acceleration heuristics:
        //  --max    : Turn all heuristic filters off (less speed, more power)
        //  --F1 <x> : Stage 1 (MSV) threshold: promote hits w/ P <= F1  [0.02]
        //  --F2 <x> : Stage 2 (Vit) threshold: promote hits w/ P <= F2  [1e-3]
        //  --F3 <x> : Stage 3 (Fwd) threshold: promote hits w/ P <= F3  [1e-5]
        //  --nobias : turn off composition bias filter

        type AccelerationHeuristicsOptions =
            ///Turn all heuristic filters off (less speed, more power)
            | NoFilters
            ///Stage 1 (MSV) threshold: promote hits w/ P <= F1
            | MSVThreshold  of float
            ///Stage 2 (Vit) threshold: promote hits w/ P <= F2
            | VitThreshold  of float
            ///Stage 3 (Fwd) threshold: promote hits w/ P <= F3
            | FwdThreshold  of float 
            ///turn off composition bias filter
            | NoBiasFilter

            static member make =
                function
                | NoFilters     -> ["max"]
                | MSVThreshold t-> ["F1"; string t]
                | VitThreshold t-> ["F2"; string t]
                | FwdThreshold t-> ["F3"; string t]
                | NoBiasFilter  -> ["nobias"]

        ///Other expert options:
        //  --nonull2     : turn off biased composition score corrections
        //  -Z <x>        : set # of comparisons done, for E-value calculation
        //  --domZ <x>    : set # of significant seqs, for domain E-value calculation
        //  --seed <n>    : set RNG seed to <n> (if 0: one-time arbitrary seed)  [42]
        //  --tformat <s> : assert target <seqfile> is in format <s>: no autodetection
        //  --cpu <n>     : number of parallel CPU workers to use for multithreads

        type MiscellaneousOptions =
            ///turn off biased composition score corrections
            | TurnOffBiasedScoreCorrections
            ///set # of comparisons done, for E-value calculation
            | EValueComparisons     of int
            ///set # of significant seqs, for domain E-value calculation
            | NumberOfSigSeqs       of int
            ///set RNG seed to <n> (if 0: one-time arbitrary seed)
            | RNGSeed               of int
            ///assert target <seqfile> is in format <s>: no autodetection
            | SequenceFileFormat    of string
            ///number of parallel CPU workers to use for multithreads
            | Threads               of int   
            
            static member make =
                function
                | TurnOffBiasedScoreCorrections -> ["--nonull2"]
                | EValueComparisons n           -> ["-Z"; string n]
                | NumberOfSigSeqs n             -> ["--domZ"; string n]
                | RNGSeed s                     -> ["--seed"; string s]
                | SequenceFileFormat f          -> ["--tformat"; f]
                | Threads t                     -> ["--cpu"; string t]

        type HMMsearchParams =
            /// Input HMM file containing query profiles to search for
            | InputHMMFile              of string
            /// Input sequence database to search query profiles in
            | SequenceDB                of string
            ///Options directing output
            | OutputDirections          of OutputDirectionsOptions list
            ///Options controlling reporting thresholds
            | ReportingThreshold        of ReportingThresholdOptions list
            ///Options controlling inclusion (significance) thresholds
            | InclusionThreshold        of InclusionThresholdOptions list
            ///Options controlling model-specific thresholding
            | ModelSpecificThreshold    of ModelSpecificThresholdOptions list
            ///Options controlling acceleration heuristics
            | AccelerationHeuristics    of AccelerationHeuristicsOptions list
            ///Other expert options
            | Miscellaneous             of MiscellaneousOptions list

            static member makeCMD = 
                function
                | InputHMMFile path                 -> [path]
                | SequenceDB path                   -> [path]
                | OutputDirections odList           -> odList |> List.map OutputDirectionsOptions.make          |> List.concat
                | ReportingThreshold rtList         -> rtList |> List.map ReportingThresholdOptions.make        |> List.concat
                | InclusionThreshold itList         -> itList |> List.map InclusionThresholdOptions.make        |> List.concat
                | ModelSpecificThreshold mstList    -> mstList|> List.map ModelSpecificThresholdOptions.make    |> List.concat
                | AccelerationHeuristics ahList     -> ahList |> List.map AccelerationHeuristicsOptions.make    |> List.concat
                | Miscellaneous mList               -> mList  |> List.map MiscellaneousOptions.make             |> List.concat

            static member makeCmdWith (m:MountInfo) =
                let cPath p = (MountInfo.containerPathOf m p)
                function
                | InputHMMFile path                 -> [cPath path]
                | SequenceDB path                   -> [cPath path]
                | OutputDirections odList           -> odList |> List.map (OutputDirectionsOptions.makeWith m)  |> List.concat
                | ReportingThreshold rtList         -> rtList |> List.map ReportingThresholdOptions.make        |> List.concat
                | InclusionThreshold itList         -> itList |> List.map InclusionThresholdOptions.make        |> List.concat
                | ModelSpecificThreshold mstList    -> mstList|> List.map ModelSpecificThresholdOptions.make    |> List.concat
                | AccelerationHeuristics ahList     -> ahList |> List.map AccelerationHeuristicsOptions.make    |> List.concat
                | Miscellaneous mList               -> mList  |> List.map MiscellaneousOptions.make             |> List.concat
            //Usage: hmmsearch [options] <hmmfile> <seqdb>

        let runHMMsearchAsync (bcContext:BioContainer.BcContext) (opt:HMMsearchParams list) = 
            //Usage: hmmbuild [-options] <hmmfile_out> <msafile> -> filter for in/out and move them to the end
            let hmm     = 
                opt 
                |> List.filter (fun p -> match p with |InputHMMFile _ -> true |_ -> false)
                |> fun x -> if List.isEmpty x then
                                failwith "no input hmm given"
                            else 
                                HMMsearchParams.makeCmdWith bcContext.Mount x.[0]

            let seqDB     = 
                opt 
                |> List.filter (fun p -> match p with |SequenceDB _ -> true |_ -> false)
                |> fun x -> if List.isEmpty x then
                                failwith "no input sequence file given"
                            else 
                                HMMsearchParams.makeCmdWith bcContext.Mount x.[0]

            let options = opt |> List.filter (fun p -> match p with |InputHMMFile _ |SequenceDB _ -> false |_ -> true)
            let cmds = (options |> List.map (HMMsearchParams.makeCmdWith bcContext.Mount))
            let tp = ("hmmalign"::(cmds |> List.concat))@hmm@seqDB

            printfn "Starting process hmmalignn\r\nparameters:"
            printfn "%s" hmm.[0]
            printfn "%s" seqDB.[0]
            cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

            async {
                    let! res = BioContainer.execAsync bcContext tp           
                    return res
 
            }
        ///Perform a multiple sequence alignment of all the sequences in seqfile by aligning
        ///them individually to the profile HMM in hmmfile. The new alignment is output to
        ///stdout.
        let runHMMalign (bcContext:BioContainer.BcContext) (opt:HMMsearchParams list) =
            runHMMsearchAsync bcContext opt
            |> Async.RunSynchronously