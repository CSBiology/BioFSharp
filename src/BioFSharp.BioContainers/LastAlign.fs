namespace BioFSharp.BioContainers

///!! Currently many parameters are not implemented!
///
///From the Last docs at http://last.cbrc.jp:
///
///LAST finds similar regions between sequences.
///
///LAST can:
/// 
///Handle big sequence data, e.g:
///
///     - Compare two vertebrate genomes
///
///     - Align billions of DNA reads to a genome
///
///Indicate the reliability of each aligned column.
///
///Use sequence quality data properly.
///
///Compare DNA to proteins, with frameshifts.
///
///Compare PSSMs to sequences
///
///Calculate the likelihood of chance similarities between random sequences.
///
///Do split and spliced alignment.
///
///Train alignment parameters for unusual kinds of sequence (e.g. nanopore).

//TO-DO: Full Parameter support

module LastAlign =

    open BioContainer

    //Usage: lastdb [options] output-name fasta-sequence-file(s)
    //Prepare sequences for subsequent alignment with lastal.

    //Main Options:
    //-h, --help: show all options and their default settings, and exit
    //-p: interpret the sequences as proteins
    //-R: repeat-marking options (default=10)
    //-c: soft-mask lowercase letters (in reference *and* query sequences)
    //-u: seeding scheme (default: YASS for DNA, else exact-match seeds)

    //Advanced Options (default settings):
    //-w: use initial matches starting at every w-th position in each sequence (1)
    //-W: use "minimum" positions in sliding windows of W consecutive positions (1)
    //-S: strand: 0=reverse, 1=forward, 2=both (1)
    //-s: volume size (unlimited)
    //-Q: input format: 0=fasta or fastq-ignore,
    //                  1=fastq-sanger, 2=fastq-solexa, 3=fastq-illumina (fasta)
    //-P: number of parallel threads (1)
    //-m: seed pattern
    //-a: user-defined alphabet
    //-i: minimum limit on initial matches per query position (0)
    //-b: bucket depth
    //-C: child table type: 0=none, 1=byte-size, 2=short-size, 3=full (0)
    //-x: just count sequences and letters
    //-v: be verbose: write messages about what lastdb is doing
    //-V, --version: show version information, and exit

    type SeedingSchemeOptions =
        | YASS
        | ExactMatch

        static member make =
            function
            | YASS -> ["-u";"YASS"]
            | ExactMatch -> ["-u";"exact-match"]
    
    type InputOptions =
        |Single   of string
        |Multiple of string list

        static member make =
            function
            |Single   f        -> [f]
            |Multiple fl       -> fl

        static member makeWith (m:MountInfo) =
            let cPath p = (MountInfo.containerPathOf m p)
            function 
            |Single   f        -> [cPath f]
            |Multiple fl       -> fl |> List.map cPath

    type LastDBParams = 
        |Input of InputOptions
        |OutputName of string
        ///-p: interpret the sequences as proteins
        |InputAsProteins
        ///-R: repeat-marking options (default=10)
        |RepeatMarking of int
        ///-c: soft-mask lowercase letters (in reference *and* query sequence
        |LowercaseSoftMasking
        ///-u: seeding scheme (default: YASS for DNA, else exact-match seeds)
        |SeedingScheme of SeedingSchemeOptions

        static member make =
            function
            |Input         i        -> InputOptions.make i
            |OutputName    f        -> [f]
            |InputAsProteins        -> ["-p"]
            |RepeatMarking rm       -> ["-R";string rm]
            |LowercaseSoftMasking   -> ["-c"]
            |SeedingScheme sm       -> SeedingSchemeOptions.make sm

        static member makeCmdWith (m:MountInfo) =
            let cPath p = (MountInfo.containerPathOf m p)
            function 
            |Input         i        -> InputOptions.makeWith m i
            |OutputName    f        -> [cPath f]
            |InputAsProteins        -> ["-p"]   
            |RepeatMarking rm       -> ["-R";string rm]
            |LowercaseSoftMasking   -> ["-c"]
            |SeedingScheme sm       -> SeedingSchemeOptions.make sm

    let runLastDBAsync (bcContext:BioContainer.BcContext) (opt:LastDBParams list) = 
        //Usage: lastdb [options] output-name fasta-sequence-file(s)
        let input     = 
            opt 
            |> List.filter (fun p -> match p with |Input _ -> true |_ -> false)
            |> fun x -> if List.isEmpty x then
                            failwith "no input sequence given"
                        else 
                            LastDBParams.makeCmdWith bcContext.Mount x.[0]

        let output     = 
            opt 
            |> List.filter (fun p -> match p with |OutputName _ -> true |_ -> false)
            |> fun x -> if List.isEmpty x then
                            failwith "no output sequence given"
                        else 
                            LastDBParams.makeCmdWith bcContext.Mount x.[0]

        let options = opt |> List.filter (fun p -> match p with |Input _ |OutputName _ -> false |_ -> true)
        let cmds = (options |> List.map (LastDBParams.makeCmdWith bcContext.Mount))
        let tp = ("lastdb"::(cmds |> List.concat)@output@input)

        printfn "Starting process lastdb\r\nparameters:"
        cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

        async {
                let! res = BioContainer.execAsync bcContext tp           
                return res
 
        }


    //Usage: lastal [options] lastdb-name fasta-sequence-file(s)

    //Find and align similar sequences.
    //Cosmetic options:
    //-h, --help: show all options and their default settings, and exit
    //-V, --version: show version information, and exit
    //-v: be verbose: write messages about what lastal is doing
    //-f: output format: TAB, MAF, BlastTab, BlastTab+ (default=MAF)

    //E-value options (default settings):
    //-D: query letters per random alignment (1e+06)  
    //-E: maximum expected alignments per square giga (1e+18/D/refSize/numOfStrands)     
    //-r: match score   (2 if -M, else  6 if 0<Q<5, else 1 if DNA)                        
    //-q: mismatch cost (3 if -M, else 18 if 0<Q<5, else 1 if DNA)             
    //-p: match/mismatch score matrix (protein-protein: BL62, DNA-protein: BL80)   
    //-a: gap existence cost (DNA: 7, protein: 11, 0<Q<5: 21)                    
    //-b: gap extension cost (DNA: 1, protein:  2, 0<Q<5:  9)               
    //-A: insertion existence cost (a)                            
    //-B: insertion extension cost (b)                      
    //-c: unaligned residue pair cost (off)          
    //-F: frameshift cost (off)                     
    //-x: maximum score drop for preliminary gapped alignments (z)       
    //-y: maximum score drop for gapless alignments (min[t*10, x])     
    //-z: maximum score drop for final gapped alignments (e-1)  
    //-d: minimum score for gapless alignments (min[e, t*ln(1000*refSize/n)])     
    //-e: minimum score for gapped alignments      
    
    //Initial-match options (default settings):                                
    //-m: maximum initial matches per query position (10)          
    //-l: minimum length for initial matches (1)                 
    //-L: maximum length for initial matches (infinity)       
    //-k: use initial matches starting at every k-th position in each query (1) 
    //-W: use "minimum" positions in sliding windows of W consecutive positions  
    
    //Miscellaneous options (default settings):          
    //-s: strand: 0=reverse, 1=forward, 2=both (2 for DNA, 1 for protein)   
    //-S: score matrix applies to forward strand of: 0=reference, 1=query (0)     
    //-K: omit alignments whose query range lies in >= K others with > scor12e (off)   
    //-C: omit gapless alignments in >= C others with > score-per-length (off)       
    //-P: number of parallel threads (1)                                            
    //-i: query batch size (8 KiB, unless there is > 1 thread or lastdb volume)     
    //-M: find minimum-difference alignments (faster but cruder)                    
    //-T: type of alignment: 0=local, 1=overlap (0)                                 
    //-n: maximum gapless alignments per query position (infinity if m=0, else m)   
    //-N: stop after the first N alignments per query strand                        
    //-R: repeat-marking options (the same as was used for lastdb)            
    //-u: mask lowercase during extensions: 0=never, 1=gapless,2=gapless+postmask, 3=always (2 if lastdb -c and Q<5, else 0)        
    //-w: suppress repeats inside exact matches, offset by <= this distance (1000)     
    //-G: genetic code file                                                          
    //-t: 'temperature' for calculating probabilities (1/lambda)                
    //-g: 'gamma' parameter for gamma-centroid and LAMA (1)               
    //-j: output type: 0=match counts, 1=gapless, 2=redundant gapped, 3=gapped,4=column ambiguity estimates, 5=gamma-centroid, 6=LAMA, 7=expected counts (3)                                           
    //-Q: input format: 0=fasta or fastq-ignore, 1=fastq-sanger, 2=fastq-solexa,3=fastq-illumina, 4=prb, 5=PSSM (fasta)

    type OutputFormatOptions =
        |TAB
        |MAF
        |BlastTab
        |BlastTabPlus

        static member make =
            function
            |TAB            -> ["-f";"TAB"]
            |MAF            -> ["-f";"MAF"]
            |BlastTab       -> ["-f";"BlastTab"]
            |BlastTabPlus   -> ["-f";"BlastTab+"]

    type QueryOptions =
        |SingleFastaFile of string
        |MultipleFilePattern of string

        static member make =
            function
            |SingleFastaFile        s -> [s]
            |MultipleFilePattern    m -> [m]

        static member makeWith (m:MountInfo) =
            let cPath p = (MountInfo.containerPathOf m p)
            function
            |SingleFastaFile        s -> [cPath s]
            |MultipleFilePattern    m -> [cPath m]

    type LastAlignParameters =
        |DBName         of string
        |Query          of QueryOptions
        |OutputFileName of string
        |OutputFormat   of OutputFormatOptions
        |Verbose

        static member makeCmd =
            function
            |DBName             dbn     -> [dbn]
            |Query              q       -> QueryOptions.make q
            |OutputFileName     on      -> [">"; on]
            |OutputFormat       outfmt  -> OutputFormatOptions.make outfmt
            |Verbose                    -> ["-v"]

        static member makeCmdWith (m:MountInfo) =
            let cPath p = (MountInfo.containerPathOf m p)
            function
            |DBName             dbn     -> [cPath dbn]
            |Query              q       -> QueryOptions.makeWith m q
            |OutputFileName     on      -> [">" ;(cPath on)]
            |OutputFormat       outfmt  -> OutputFormatOptions.make outfmt
            |Verbose                    -> ["-v"]

    let runLastAlignAsync (bcContext:BioContainer.BcContext) (opt:LastAlignParameters list) = 
        //Usage: lastal [options] lastdb-name fasta-sequence-file(s)

        let db = 
            opt 
            |> List.filter (fun p -> match p with |DBName _ -> true |_ -> false)
            |> fun x -> if List.isEmpty x then
                            failwith "no search DB given"
                        else 
                            LastAlignParameters.makeCmdWith bcContext.Mount x.[0]
        let input     = 
            opt 
            |> List.filter (fun p -> match p with |Query _ -> true |_ -> false)
            |> fun x -> if List.isEmpty x then
                            failwith "no query given"
                        else 
                            LastAlignParameters.makeCmdWith bcContext.Mount x.[0]

        let output     = 
            opt 
            |> List.filter (fun p -> match p with |OutputFileName _ -> true |_ -> false)
            |> fun x -> if List.isEmpty x then
                            failwith "no output name given"
                        else 
                            LastAlignParameters.makeCmdWith bcContext.Mount x.[0]

        let options = opt |> List.filter (fun p -> match p with |DBName _ |Query _ |OutputFileName _-> false |_ -> true)
        let cmds = (options |> List.map (LastAlignParameters.makeCmdWith bcContext.Mount))
        let tp = ("lastal"::(cmds |> List.concat)@db@input@output)

        printfn "Starting process lastal\r\nparameters:"
        cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

        async {
                let! res = BioContainer.execReturnAsync bcContext tp           
                return res
 
        }