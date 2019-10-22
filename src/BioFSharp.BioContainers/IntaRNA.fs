namespace BioFSharp.BioContainers

module IntaRNA =
    
    //TO-DO: add full argument support. Currently only basic arguments are wrapped.

    open BioContainer
    
    
        ///  -q [ --query ] arg           
        ///
        ///either an RNA sequence or the stream/file name from where to read the query sequences (shouldbe the shorter sequences to increaseefficiency); 
        ///sequences have to use IUPACnucleotide encoding
        type QueryInputOptions =
            ///RNA sequence string
            |RNASequence of string
            ///stream/file name from where to read the query sequences
            |File of string

            static member make = function
                |RNASequence s  -> ["-q"; s]
                |File f         -> ["-q"; f]

            static member makeWith (m:MountInfo) = 
                let cPath p = (MountInfo.containerPathOf m p)
                function
                |RNASequence s  -> ["-q"; s]        
                |File f         -> ["-q"; cPath f]  

        ///  --qAcc arg (=C)              
        ///
        /// accessibility computation :
        ///    'N' no accessibility contributions
        ///    'C' computation of accessibilities
        ///    'P' unpaired probabilities in RNAplfold format
        ///   from --qAccFile
        ///    'E' ED values in RNAplfold Pu-like format from
        ///   --qAccFile
        type QueryAcessibilityComputationTypeOptions =
            ///'N' no accessibility contributions
            |NoContributions
            ///'C' computation of accessibilities
            |Compute
            ///'P' unpaired probabilities in RNAplfold format
            |UnpairedFromFile
            ///'E' ED values in RNAplfold Pu-like format from
            |EDValuesFromFile

            static member make = function
                |NoContributions    -> ["--qAcc=N"]
                |Compute            -> ["--qAcc=C"]
                |UnpairedFromFile   -> ["--qAcc=P"]
                |EDValuesFromFile   -> ["--qAcc=E"]


        type QueryAcessibilityComputationOptions =

            |QueryAcessibilityComputationType of QueryAcessibilityComputationTypeOptions

            ///  --qAccW arg (=150)           accessibility computation : sliding window size
            ///                               for query accessibility computation (arg in
            ///                               range [0,99999]; 0 will use to the full sequence
            ///                               length). Note, this also restricts the maximal
            ///                               interaction length (see --qIntLenMax).

            |SlidingWindowSize of int
            ///  --qAccL arg (=100)           accessibility computation : maximal loop length
            ///                               (base pair span) for query accessibility
            ///                               computation (arg in range [0,99999]; 0 will use
            ///                               to sliding window size 'qAccW')

            |MaximalLoopLength of int

            static member make = function
                |QueryAcessibilityComputationType t ->  QueryAcessibilityComputationTypeOptions.make t
                |SlidingWindowSize i                ->  [sprintf "--qAccW=%i" i]
                |MaximalLoopLength i                ->  [sprintf "--qAccL=%i" i]
    


        type QueryOptions = 
            |QueryInput of QueryInputOptions
            |QueryAcessibilityComputation of QueryAcessibilityComputationOptions list

            static member make = function
                |QueryInput qi                      -> QueryInputOptions.make qi
                |QueryAcessibilityComputation cList -> cList |> List.map QueryAcessibilityComputationOptions.make |> List.concat

            static member makeWith (m:MountInfo) = 
                let cPath p = (MountInfo.containerPathOf m p)
                function        
                |QueryInput qi                      -> (QueryInputOptions.makeWith m) qi
                |QueryAcessibilityComputation cList -> cList |> List.map QueryAcessibilityComputationOptions.make |> List.concat



        //Target:
        //  -t [ --target ] arg          either an RNA sequence or the stream/file name
        //                               from where to read the target sequences (should
        //                               be the longer sequences to increase efficiency);
        //                               use 'STDIN' to read from standard input stream;
        //                               sequences have to use IUPAC nucleotide encoding

        type TargetInputOptions =
            |RNASequence of string
            |File of string

            static member make = function
                |RNASequence s  -> ["-t"; s] 
                |File f         -> ["-t"; f] 

            static member makeWith (m:MountInfo) = 
                let cPath p = (MountInfo.containerPathOf m p)
                function
                |RNASequence s  -> ["-t"; s]        
                |File f         -> ["-t"; cPath f]  

        //  --tAcc arg (=C)              accessibility computation :
        //                                'N' no accessibility contributions
        //                                'C' computation of accessibilities
        //                                'P' unpaired probabilities in RNAplfold format
        //                               from --tAccFile
        //                                'E' ED values in RNAplfold Pu-like format from
        //                               --tAccFile

        type TargetAcessibilityComputationTypeOptions =
            |NoContributions
            |Compute
            |UnpairedFromFile
            |EDValuesFromFile

            static member make = function
                |NoContributions    -> ["--tAcc=N"]
                |Compute            -> ["--tAcc=C"]
                |UnpairedFromFile   -> ["--tAcc=P"]
                |EDValuesFromFile   -> ["--tAcc=E"]

        type TargetAcessibilityComputationOptions =
            |TargetAcessibilityComputationType of TargetAcessibilityComputationTypeOptions

            //  --tAccW arg (=150)           accessibility computation : sliding window size
            //                               for query accessibility computation (arg in
            //                               range [0,99999]; 0 will use the full sequence
            //                               length) Note, this also restricts the maximal
            //                               interaction length (see --tIntLenMax).
            |SlidingWindowSize of int

            //  --tAccL arg (=100)           accessibility computation : maximal loop size
            //                               (base pair span) for query accessibility
            //                               computation (arg in range [0,99999]; 0 will use
            //                               the sliding window size 'tAccW')
            |MaximalLoopLength of int

            static member make = function
                |TargetAcessibilityComputationType t ->  TargetAcessibilityComputationTypeOptions.make t
                |SlidingWindowSize i                ->  [sprintf "--tAccW=%i" i]
                |MaximalLoopLength i                ->  [sprintf "--tAccL=%i" i]



        type TargetOptions = 
            |TargetInput of TargetInputOptions
            |TargetAcessibilityComputation of TargetAcessibilityComputationOptions list

            static member make = function
                |TargetInput ti                      -> TargetInputOptions.make ti
                |TargetAcessibilityComputation cList -> cList |> List.map TargetAcessibilityComputationOptions.make |> List.concat

            static member makeWith (m:MountInfo) = 
                let cPath p = (MountInfo.containerPathOf m p)
                function        
                |TargetInput ti                      -> (TargetInputOptions.makeWith m) ti
                |TargetAcessibilityComputation cList -> cList |> List.map TargetAcessibilityComputationOptions.make |> List.concat 


        //Helix (only if --model=H):
        type HelixOptions =
            |Default
            ///  --helixMinBP arg (=2)        minimal number of base pairs inside a helix (arg
            ///                               in range [2,4])
            |MinBP              of int
            ///  --helixMaxBP arg (=10)       maximal number of base pairs inside a helix (arg
            ///                               in range [2,20])
            |MaxBP              of int
            ///  --helixMaxIL arg (=0)        maximal size for each internal loop size in a
            ///                               helix (arg in range [0,2]).
            |MaxInternalLoop    of float
            ///  --helixMaxED arg (=999)      maximal ED-value allowed (per sequence) during
            ///                               helix computation (arg in range [-999,999]).
            |MaxEDValue         of float
            ///  --helixMaxE arg (=0)         maximal energy considered during helix
            ///                               computation (arg in range [-999,999]).
            |MaxEnergy          of float
            ///  --helixWithED                if present, ED-values will be used within the
            ///                               energy evaluation of a helix
            |WithED             

            static member make = function
                |Default            ->[""]
                |MinBP            i ->[sprintf "--helixMinBP=%i" i]
                |MaxBP            i ->[sprintf "--helixMaxBP=%i" i]
                |MaxInternalLoop  f ->[sprintf "--helixMaxIL=%f" f]
                |MaxEDValue       f ->[sprintf "--helixMaxED=%f" f]
                |MaxEnergy        f ->[sprintf "--helixMaxE=%f"  f]
                |WithED             ->["--helixWithED"]

        ///Interaction:
        ///  -m [ --mode ] arg (=H)       prediction mode


        type PredictionModeOptions =
            ///'H' = heuristic (fast and low memory),
            |Heuristic of HelixOptions list
            ///'M' = exact and low memory
            |ExactLowMemory
            ///'E' = exact (high memory)
            |Exact

            static member make = function
                |Heuristic hList->  ("--mode=H":: (hList |> List.map HelixOptions.make |> List.concat))
                |ExactLowMemory ->  ["--mode=M"]
                |Exact          ->  ["--mode=E"]



        type SeedOptions = 
            ///  --noSeed                     if present, no seed is enforced within the
            ///                               predicted interactions
            |NoSeed
            ///  --seedTQ arg                 comma separated list of explicit seed base pair
            ///                               encoding(s) in the format startTbpsT&startQbpsQ,
            ///                               e.g. '3|||.|&7||.||', where startT/Q are the
            ///                               indices of the 5' seed ends in target/query
            ///                               sequence and 'bps' the dot-bar base pair
            ///                               encodings. This disables all other seed
            ///                               constraints and seed identification.
            |SeedList of string
            ///  --seedBP arg (=7)            number of inter-molecular base pairs within the
            ///                               seed region (arg in range [2,20])
            |BPAmount of int
            ///  --seedMaxUP arg (=0)         maximal overall number (query+target) of
            ///                               unpaired bases within the seed region (arg in
            ///                               range [0,20])
            |MaxUnpairedBases of int
    
            static member make = function
                |NoSeed             -> ["--noSeed"]
                |SeedList sL        -> [sprintf "--seedTQ=%s" sL    ]
                |BPAmount i         -> [sprintf "--seedBP=%i" i     ]
                |MaxUnpairedBases i -> [sprintf "--seedMaxUP=%i" i  ]

        /// --outMode arg (=N)   
        type OutputModeOptions = 
            ///'N' normal output (ASCII char + energy),
            |Normal
            ///'D' detailed output (ASCII char + energy/position details),
            |Detailed
            ///'C' CSV output (see --outCsvCols),
            |CSV

            static member make = function
                |Normal     -> ["--outMode=N"]
                |Detailed   -> ["--outMode=D"]
                |CSV        -> ["--outMode=C"]


        ///Top level type for modelling basic command line arguments for IntaRNA
        type IntaRNAParams =
            | Query of QueryOptions list
            | Target of TargetOptions list
            | PredictionMode of PredictionModeOptions list
            | Seed of SeedOptions list
            | OutputMode of OutputModeOptions

            static member makeCmd = function
                | Query             qList -> qList |> List.map QueryOptions.make            |> List.concat 
                | Target            tList -> tList |> List.map TargetOptions.make           |> List.concat 
                | PredictionMode    pList -> pList |> List.map PredictionModeOptions.make   |> List.concat 
                | Seed              sList -> sList |> List.map SeedOptions.make             |> List.concat 
                | OutputMode        o     -> OutputModeOptions.make o

            static member makeCmdWith (m:MountInfo) = 
                let cPath p = (MountInfo.containerPathOf m p)
                function
                | Query             qList -> qList |> List.map (QueryOptions.makeWith m)    |> List.concat 
                | Target            tList -> tList |> List.map (TargetOptions.makeWith m)   |> List.concat 
                | PredictionMode    pList -> pList |> List.map PredictionModeOptions.make   |> List.concat 
                | Seed              sList -> sList |> List.map SeedOptions.make             |> List.concat 
                | OutputMode        o     -> OutputModeOptions.make o

        ///
        let runIntaRNAAsync (bcContext:BioContainer.BcContext) (opt:IntaRNAParams list) = 
            let cmds = opt |> List.map (IntaRNAParams.makeCmdWith bcContext.Mount)
            let tp = "IntaRNA"::(cmds |> List.concat)
            printfn "starting process IntaRNA\r\nparameters:"
            cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

            async {
                let! res = BioContainer.execReturnAsync bcContext tp
                return res
            }

        ///Runs IntaRNA with the given the input parameters in a container specified by the bcContext
        let runIntaRNA (bcContext:BioContainer.BcContext) (opt:IntaRNAParams list) = 
            runIntaRNAAsync bcContext opt
            |> Async.RunSynchronously