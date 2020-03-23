namespace BioFSharp.BioContainers
open BioContainer

module STAR =

    let inline private handleOptionalSecondParameter (handleFunction1: ('T -> string)) (handleFunction2: ('U -> string)) (parameters: ('T*'U option)) =
        let p1,p2 = parameters
        match p2 with
        |Some v ->
            sprintf "%s %s" (handleFunction1 p1) (handleFunction2 v)
        |None 
            -> handleFunction1 p1

    let inline private handleQuadruples (q: 'A*'B*'C*'D) =
        let a,b,c,d = q
        sprintf "%s %s %s %s" (string a) (string b) (string c) (string d)

//### Parameter Files

//### Variation parameters

//### Input Files


//Run Parameters

    ///type of the STAR run
    type RunMode =
        ///map reads
        |AlignReads
        ///generate genome files
        |GenomeGenerate
        ///input alignments from BAM. Presently only works with --outWigType and --bamRemoveDuplicates.
        |InputAlignmentsFromBAM
        ///lift-over of GTF files (--sjdbGTFfile) between genome assemblies using chain file(s) from --genomeChainFiles.
        |LiftOver

        static member make = function
            |AlignReads             -> "alignReads"
            |GenomeGenerate         -> "genomeGenerate"
            |InputAlignmentsFromBAM -> "inputAlignmentsFromBAM"
            |LiftOver               -> "liftOver"

    type RunPermission =
        |User_RWX
        |All_RWX

        static member make = function
            |User_RWX   -> "User_RWX"
            |All_RWX    -> "All_RWX"

    type RunParams =
        ///type of the STAR run
        |Mode of RunMode
        ///number of threads to run STAR
        |Threads of int
        ///permissions for the directories created at the run-time.
        |DirectoryPermissions of RunPermission
        ///random number generator seed. 
        |RNGSeed of int

        static member makeCmd = function 
            |Mode m                     -> ["--runMode"   ; RunMode.make m]
            |Threads t                  -> ["--runThreadN"; string t]
            |DirectoryPermissions p     -> ["--runDirPerm"; RunPermission.make p]
            |RNGSeed r                  -> ["--runRNGseed"; string r]

    ///mode of shared memory usage for the genome files. Only used with --runMode alignReads.
    type GenomeLoad =
        ///load genome into shared and keep it in memory after run
        |LoadAndKeep
        //load genome into shared but remove it after run
        |LoadAndRemove
        ///load genome into shared memory and exit, keeping the genome in memory for future runs
        |LoadAndExit
        ///do not map anything, just remove loaded genome from memory
        |Remove
        ///do not use shared memory, each job will have its own private copy of the genome
        |NoSharedMemory

        static member make = function
            |LoadAndKeep    -> "LoadAndKeep"
            |LoadAndRemove  -> "LoadAndRemove"
            |LoadAndExit    -> "LoadAndExit"
            |Remove         -> "Remove"
            |NoSharedMemory -> "NoSharedMemory"

    ///Genome Indexing Parameters - only used with --runMode genomeGenerate
    type GenomeIndexingOptions =
        ///=log2(chrBin), where chrBin is the size of the bins for genome storage: each chromosome will occupy an integer number of bins. For a genome with large number of contigs, it is recommended to scale this parameter as min(18, log2[max(GenomeLength/NumberOfReferences,ReadLength)]).
        |ChrBinNbits of int
        ///length (bases) of the SA pre-indexing string. Typically between 10 and 15. Longer strings will use much more memory, but allow faster searches. For small genomes, the parameter --genomeSAindexNbases must be scaled down to min(14, log2(GenomeLength)/2 - 1).
        |SAindexNbases of int
        ///suffux array sparsity, i.e. distance between indices: use bigger numbers to decrease needed RAM at the cost of mapping speed reduction
        |SAsparseD of int
        ///maximum length of the suffixes, has to be longer than read length. -1 = infinite.
        |SuffixLengthMax of int

        static member makeCmd = function
            //genomeChrBinNbits           18
            |ChrBinNbits i      -> ["--genomeChrBinNbits"; string i]
            //genomeSAindexNbases         14
            |SAindexNbases i    -> ["--genomeSAindexNbases"; string i]
            //genomeSAsparseD             1
            |SAsparseD i        -> ["--genomeSAsparseD"; string i]
            //genomeSuffixLengthMax       -1
            |SuffixLengthMax  i -> ["--genomeSuffixLengthMax"; string i]

    type GenomeParams =
        ///path to the directory where genome files are stored (for --runMode alignReads) or will be generated (for --runMode generateGenome)
        |GenomeDirectory of string
        ///path(s) to the fasta files with the genome sequences, separated by spaces. These files should be plain text FASTA files, they *cannot* be zipped. Required for the genome generation (--runMode genomeGenerate). Can also be used in the mapping (--runMode alignReads) to add extra (new) sequences to the genome (e.g. spike-ins).
        |GenomeFastaFiles of string list
        ///chain files for genomic liftover. Only used with --runMode liftOver .
        |GenomeChainFiles of string list
        ///genome files exact sizes in bytes. Typically, this should not be defined by the user.
        |GenomeFileSizes of int list
        ///VCF file with consensus SNPs (i.e. alternative allele is the major (AF>0.5) allele)
        |GenomeConsensusFile of string
        ///mode of shared memory usage for the genome files. Only used with --runMode alignReads.
        |Load of GenomeLoad
        ///Genome Indexing Parameters - only used with --runMode genomeGenerate
        |IndexingOptions of GenomeIndexingOptions list

        static member makeCmd = function
            |GenomeDirectory d      -> ["--genomeDir"; d]
            |GenomeFastaFiles f     -> ["--genomeFastaFiles"    ; f |> String.concat " " ]
            |GenomeChainFiles f     -> ["--genomeChainFiles"    ; f |> String.concat " "]
            |GenomeFileSizes l      -> ["--genomeFileSizes"     ; l |> List.map string |> String.concat " "]
            |GenomeConsensusFile f  -> ["--genomeConsensusFile" ; f]
            |Load l                 -> ["--genomeLoad"          ; l |> GenomeLoad.make]
            |IndexingOptions i      -> i |> List.map GenomeIndexingOptions.makeCmd |> List.concat

        static member makeCmdWith (m: MountInfo) = function
            |GenomeDirectory d      -> ["--genomeDir"           ; d |> MountInfo.containerPathOf m ]
            |GenomeFastaFiles f     -> ["--genomeFastaFiles"    ; f |> List.map (MountInfo.containerPathOf m) |> String.concat " " ]
            |GenomeChainFiles f     -> ["--genomeChainFiles"    ; f |> List.map (MountInfo.containerPathOf m) |> String.concat " " ]
            |GenomeFileSizes l      -> ["--genomeFileSizes"     ; l |> List.map string |> List.map (MountInfo.containerPathOf m) |> String.concat " "]
            |GenomeConsensusFile f  -> ["--genomeConsensusFile" ; f |> MountInfo.containerPathOf m]
            |Load l                 -> ["--genomeLoad"          ; l |> GenomeLoad.make]
            |IndexingOptions i      -> i |> List.map GenomeIndexingOptions.makeCmd |> List.concat

//### Splice Junctions Database

    ///which files to save when sjdb junctions are inserted on the fly at the mapping step
    type SpliceJunctionsDatabaseSaveOptions =
        ///only small junction / transcript files
        |Basic
        ///all files including big Genome, SA and SAindex - this will create a complete genome directory
        |All  

        static member make = function
            |Basic -> "Basic"
            |All   -> "All"

    type SpliceJunctionsDatabaseParams =
        ///path(s) to the files with genomic coordinates (chr tab start tab end tab strand) for the splice junction introns. Multiple files can be supplied wand will be concatenated.
        |FileChrStartEnd of string list
        ///string: path to the GTF file with annotations
        |GTFfile of string
        ///prefix for chromosome names in a GTF file (e.g. 'chr' for using ENSMEBL annotations with UCSC genomes)
        |GTFchrPrefix of string
        /// feature type in GTF file to be used as exons for building transcripts
        |GTFfeatureExon of string
        ///GTF attribute name for parent transcript ID (default "transcript_id" works for GTF files)
        |GTFtagExonParentTranscript of string
        ///GTF attribute name for parent gene ID (default "gene_id" works for GTF files)
        |GTFtagExonParentGene of string
        ///GTF attrbute name for parent gene name
        |GTFtagExonParentGeneName of string list
        ///GTF attrbute name for parent gene type
        |GTFtagExonParentGeneType of string list
        ///length of the donor/acceptor sequence on each side of the junctions, ideally = (mate_length - 1)
        |Overhang of int
        ///extra alignment score for alignmets that cross database junctions
        |Score of int
        ///which files to save when sjdb junctions are inserted on the fly at the mapping step
        |InsertSave of SpliceJunctionsDatabaseSaveOptions

        static member makeCmd = function
            |FileChrStartEnd l            -> ["--sjdbChrStartEnd"               ; l |> String.concat " " ]
            |GTFfile f                    -> ["--sjdbGTFfile"                   ; f ]
            |GTFchrPrefix f               -> ["--sjdbGTFchrPrefix"              ; f ]
            |GTFfeatureExon f             -> ["--sjdbGTFfeatureExon"            ; f ]
            |GTFtagExonParentTranscript f -> ["--sjdbGTFtagExonParentTranscript"; f ]
            |GTFtagExonParentGene f       -> ["--sjdbGTFtagExonParentGene"      ; f ]
            |GTFtagExonParentGeneName f   -> ["--sjdbGTFtagExonParentGeneName"  ; f |> String.concat " " ]
            |GTFtagExonParentGeneType f   -> ["--sjdbGTFtagExonParentGeneType"  ; f |> String.concat " " ]
            |Overhang o                   -> ["--sjdbOverhang"                  ; string o]
            |Score s                      -> ["--sjdbScore"                     ; string s]
            |InsertSave s                 -> ["--sjdbInsertSave"                ; s |> SpliceJunctionsDatabaseSaveOptions.make ]

        static member makeCmdWith (m: MountInfo) = function
            |FileChrStartEnd l            -> ["--sjdbChrStartEnd"               ; l |> String.concat " " ]
            |GTFfile f                    -> ["--sjdbGTFfile"                   ; f |> MountInfo.containerPathOf m]
            |GTFchrPrefix f               -> ["--sjdbGTFchrPrefix"              ; f ]
            |GTFfeatureExon f             -> ["--sjdbGTFfeatureExon"            ; f ]
            |GTFtagExonParentTranscript f -> ["--sjdbGTFtagExonParentTranscript"; f ]
            |GTFtagExonParentGene f       -> ["--sjdbGTFtagExonParentGene"      ; f ]
            |GTFtagExonParentGeneName f   -> ["--sjdbGTFtagExonParentGeneName"  ; f |> String.concat " " ]
            |GTFtagExonParentGeneType f   -> ["--sjdbGTFtagExonParentGeneType"  ; f |> String.concat " " ]
            |Overhang o                   -> ["--sjdbOverhang"                  ; string o]
            |Score s                      -> ["--sjdbScore"                     ; string s]
            |InsertSave s                 -> ["--sjdbInsertSave"                ; s |> SpliceJunctionsDatabaseSaveOptions.make ]

//### Read Parameters
    ///format of input read files
    type ReadFilesType =
        ///FASTA or FASTQ
        |FastX      
        ///SAM or BAM single-end reads; for BAM use --readFilesCommand samtools view
        |SAMSingleEnd
        ///SAM or BAM paired-end reads; for BAM use --readFilesCommand samtools view
        |SAMPairedEnd

        static member make = function
            |FastX          -> "Fastx"
            |SAMSingleEnd   -> "SAM SE"
            |SAMPairedEnd   -> "SAM PE"

    ///lengths of names,sequences,qualities for both mates are the same  / not the same. NotEqual is safe in all situations.
    type MatesLengths =
        ///lengths of names,sequences,qualities for both mates are the same
        |Equal
        ///lengths of names,sequences,qualities for both mates are not the same. NotEqual is safe in all situations.
        |NotEqual

        static member make = function
            |Equal      -> "Equal"
            |NotEqual   -> "NotEqual"

    ///clipping options from either end of the mates
    type MateClippingOptions =
        ///number(s) of bases to clip from 3p of each mate. If one value is given, it will be assumed the same for both mates.
        |Bases3p                    of int * int option 
        ///number(s) of bases to clip from 5p of each mate. If one value is given, it will be assumed the same for both mates.
        |Bases5p                    of int * int option 
        ///adapter sequences to clip from 3p of each mate.  If one value is given, it will be assumed the same for both mates.
        |AdapterSeq3p               of string * string option
        ///max proportion of mismatches for 3p adpater clipping for each mate.  If one value is given, it will be assumed the same for both mates.
        |AdapterMisMatchPortion3p   of float * float option
        ///number of bases to clip from 3p of each mate after the adapter clipping. If one value is given, it will be assumed the same for both mates.
        |BasesAfterAdapter3p        of int * int option

        static member makeCmd = function
            |Bases3p (p1,p2)                    -> ["--clip3pNbases"            ; (p1,p2) |> handleOptionalSecondParameter string string]
            |Bases5p (p1,p2)                    -> ["--clip5pNbases"            ; (p1,p2) |> handleOptionalSecondParameter string string]
            |AdapterSeq3p (p1,p2)               -> ["--clip3pAdapterSeq"        ; (p1,p2) |> handleOptionalSecondParameter string string]
            |AdapterMisMatchPortion3p (p1,p2)   -> ["--clip3pAdapterMMp"        ; (p1,p2) |> handleOptionalSecondParameter string string]
            |BasesAfterAdapter3p (p1,p2)        -> ["--clip3pAfterAdapterNbases"; (p1,p2) |> handleOptionalSecondParameter string string]

    type ReadParams =
        ///format of input read files
        |InputFormat of ReadFilesType
        ///paths to files that contain input read1 (and, if needed,  read2)
        |FilesIn of string * string option
        ///prefix for the read files names, i.e. it will be added in front of the strings in --readFilesIn
        |FilesPrefix of string
        ///command line to execute for each of the input file. This command should generate FASTA or FASTQ text and send it to stdout For example: zcat - to uncompress .gz files, bzcat - to uncompress .bz2 files, etc.
        |FilesCommand of string list
        ///number of reads to map from the beginning of the file
        |MapNumber of int
        ///Equal/NotEqual - lengths of names,sequences,qualities for both mates are the same  / not the same. NotEqual is safe in all situations.
        |MatesLengthsIn of MatesLengths
        ///character(s) separating the part of the read names that will be trimmed in output (read name after space is always trimmed)
        |NameSeparator of string
        ///number to be subtracted from the ASCII code to get Phred quality score
        |QualityScoreBase of int
        ///clipping options from either end of the mates
        |MateClipping of MateClippingOptions list

        static member makeCmd = function
            |MateClipping m     -> m |> List.map MateClippingOptions.makeCmd |> List.concat
            |NameSeparator s    -> ["--readNameSeparator"   ; s]
            |FilesPrefix p      -> ["--readFilesPrefix"     ; p]
            |InputFormat f      -> ["--readFilesType"       ; f         |> ReadFilesType.make]
            |FilesIn (f1,f2)    -> ["--readFilesIn"         ; (f1,f2)   |> handleOptionalSecondParameter string string]
            |FilesCommand c     -> ["--readFilesCommand"    ; c         |> String.concat " "]
            |MapNumber n        -> ["--readMapNumber"       ; n         |> string]
            |MatesLengthsIn m   -> ["--readMatesLengthsIn"  ; m         |> MatesLengths.make]
            |QualityScoreBase b -> ["--readQualityScoreBase"; b         |> string]
                                    
        static member makeCmdWith (m:MountInfo) = function
            |MateClipping m     -> m |> List.map MateClippingOptions.makeCmd |> List.concat
            |NameSeparator s    -> ["--readNameSeparator"   ; s]
            |FilesPrefix p      -> ["--readFilesPrefix"     ; p]
            |InputFormat f      -> ["--readFilesType"       ; f         |> ReadFilesType.make]
            |FilesIn (f1,f2)    -> ["--readFilesIn"         ; (f1,f2)   |> handleOptionalSecondParameter ((MountInfo.containerPathOf m) >> string)  ((MountInfo.containerPathOf m) >> string)]
            |FilesCommand c     -> ["--readFilesCommand"    ; c         |> String.concat " "]
            |MapNumber n        -> ["--readMapNumber"       ; n         |> string]
            |MatesLengthsIn m   -> ["--readMatesLengthsIn"  ; m         |> MatesLengths.make]
            |QualityScoreBase b -> ["--readQualityScoreBase"; b         |> string]
//### Limits

    type LimitParams =
        ///maximum available RAM (bytes) for genome generation
        |GenomeGenerateRAM  of int
        ///max available buffers size (bytes) for input/output, per thread
        |IObufferSize       of int
        ///max number of junctions for one read (including all multi-mappers)
        |OutSAMoneReadBytes of int
        ///max size of the SAM record (bytes) for one read. Recommended value: >(2*(LengthMate1+LengthMate2+100)*outFilterMultimapNmax
        |OutSJoneRead       of int
        ///max number of collapsed junctions
        |OutSJcollapsed     of int
        ///maximum available RAM (bytes) for sorting BAM. If =0, it will be set to the genome index size. 0 value can only be used with --genomeLoad NoSharedMemory option.
        |BAMsortRAM         of int
        ///maximum number of junction to be inserted to the genome on the fly at the mapping stage, including those from annotations and those detected in the 1st step of the 2-pass run
        |SjdbInsertNsj      of int
        ///soft limit on the number of reads
        |NreadsSoft         of int

        static member makeCmd = function
            |GenomeGenerateRAM  i   -> ["--limitGenomeGenerateRAM" ; i |> string]
            |IObufferSize       i   -> ["--limitIObufferSize"      ; i |> string]
            |OutSAMoneReadBytes i   -> ["--limitOutSAMoneReadBytes"; i |> string]
            |OutSJoneRead       i   -> ["--limitOutSJoneRead"      ; i |> string]
            |OutSJcollapsed     i   -> ["--limitOutSJcollapsed"    ; i |> string]
            |BAMsortRAM         i   -> ["--limitBAMsortRAM"        ; i |> string]
            |SjdbInsertNsj      i   -> ["--limitSjdbInsertNsj"     ; i |> string]
            |NreadsSoft         i   -> ["--limitNreadsSoft"        ; i |> string]


//### Output: general

    type OutputSTDOptions =
        ///log messages
        |Log                   
        ///alignments in SAM format (which normally are output to Aligned.out.sam file), normal standard output will go into Log.std.out
        |SAM                   
        ///alignments in BAM format, unsorted. Requires --outSAMtype BAM Unsorted
        |BAMUnsorted          
        ///alignments in BAM format, unsorted. Requires --outSAMtype BAM SortedByCoordinate
        |BAMSortedByCoordinate
        ///alignments to transcriptome in BAM format, unsorted. Requires --quantMode TranscriptomeSAM
        |BAMQuant             
        
        static member make = function
            |Log                    -> "Log"
            |SAM                    -> "SAM"
            |BAMUnsorted            -> "BAM_Unsorted"
            |BAMSortedByCoordinate  -> "BAM_SortedByCoordinate"
            |BAMQuant               -> "BAM_Quant"

    type AlignmentOrder =
        ///quasi-random order used before 2.5.0
        |Old24
        ///random order of alignments for each multi-mapper. Read mates (pairs) are always adjacent, all alignment for each read stay together. This option will become default in the future releases.
        |Random 

        static member make = function
            |Old24  -> "Old_2.4"
            |Random -> "Random"

    type SAMSortingOptions =
        ///standard unsorted
        |Unsorted
        ///sorted by coordinate. This option will allocate extra memory for sorting which can be specified by --limitBAMsortRAM.
        |SortedByCoordinate

        static member make = function
            |Unsorted           -> "Unsorted"
            |SortedByCoordinate -> "SortedByCoordinate"

    ///type of SAM/BAM output
    type OutputTypeOptions =
        ///no SAM/BAM output
        |NoOutput
        ///output BAM
        |BAM of SAMSortingOptions
        ///output SAM
        |SAM of SAMSortingOptions

        static member make = function
            |NoOutput   ->  sprintf "None"                              
            |BAM s      ->  sprintf "BAM %s" (SAMSortingOptions.make s) 
            |SAM s      ->  sprintf "SAM %s" (SAMSortingOptions.make s) 

    ///mode of SAM output
    type OutputModeOptions =
        ///no SAM output
        | NoOutput
        ///full SAM output
        | Full
        ///full SAM but without quality scores
        | NoQS

        static member make = function
            |NoOutput   -> "NoOutput"
            |Full       -> "Full"
            |NoQS       -> "NoQS"

    ///Cufflinks-like strand field flag
    type StrandFieldOptions =
        |NotUsed
        ///strand derived from the intron motif. Reads with inconsistent and/or non-canonical introns are filtered out.
        |IntronMotif

        static member make = function
            |NotUsed        -> "None"
            |IntronMotif    -> "intronMotif"

    ///desired SAM attributes, in the order desired for the output SAM
    type AttributeOptions =
        ///NH HI AS nM NM MD jM jI XS MC ch ... any combination in any order
        |Custom of string list
        ///no attributes
        |NoAttributes    
        ///NH HI AS nM
        |Standard
        ///NH HI AS nM NM MD jM jI MC ch
        |All     
        ///variant allele
        |VA      
        ///genomic coordiante of the variant overlapped by the read
        |VG      
        ///0/1 - alignment does not pass / passes WASP filtering. Requires --waspOutputMode SAMtag
        |VW      
        ///CR CY UR UY ... sequences and quality scores of cell barcodes and UMIs for the solo* demultiplexing
        ///
        ///CB UB       ... error-corrected cell barcodes and UMIs for solo* demultiplexing. Requires --outSAMtype BAM SortedByCoordinate.
        |SoloCustom of string list
        ///assessment of CB and UMI
        |SM
        ///sequence of the entire barcode (CB,UMI,adapter...)
        |SS
        ///quality of the entire barcode
        |SQ
        ///alignment block read/genomic coordinates
        |RB
        ///read coordinate of the variant
        |VR

        static member make = function
            |Custom c                   -> c |> String.concat " "
            |SoloCustom c               -> c |> String.concat " "
            |NoAttributes               -> "None"
            |Standard                   -> "Standard"
            |All                        -> "All"
            |VA                         -> "vA"
            |VG                         -> "vG"
            |VW                         -> "vW"
            |SM                         -> "sM"
            |SS                         -> "sS"
            |SQ                         -> "sQ"
            |RB                         -> "rB"
            |VR                         -> "vR"

    ///output of unmapped reads in the SAM format
    type UnmappedReadOptions =
    ///no output
        | Discard
    ///output unmapped reads within the main SAM file (i.e. Aligned.out.sam)
        | Within
    ///record unmapped mate for each alignment, and, in case of unsorted output, keep it adjacent to its mapped mate. Only affects multi-mapping reads.
        | WithinKeepPairs

        static member make = function 
            |Discard            -> "None"
            |Within             -> "Within"
            |WithinKeepPairs    -> "Within KeepPairs"

    ///type of sorting for the SAM output
    type OutputSortingOptions =
        ///one mate after the other for all paired alignments
        |Paired
        ///one mate after the other for all paired alignments, the order is kept the same as in the input FASTQ files
        |PairedKeepInputOrder

        static member make = function
            |Paired                 -> "Paired"
            |PairedKeepInputOrder   -> "PairedKeepInputOrder"

    ///which alignments are considered primary - all others will be marked with 0x100 bit in the FLAG
    type PrimaryFlagOptions =
        ///only one alignment with the best score is primary
        |OneBestScore
        ///all alignments with the best score are primary
        |AllBestScore

        static member make = function
            |OneBestScore -> "OneBestScore"
            |AllBestScore -> "AllBestScore"

    ///read ID record type
    type ReadIDOptions =
        ///first word (until space) from the FASTx read ID line, removing /1,/2 from the end
        |Standard
        ///read number (index) in the FASTx file
        |Number

        static member make = function
            |Standard   ->  "Standard"
            |Number     ->  "Number"

    ///filter the output into main SAM/BAM files
    type OutputFilterOptions =
        ///only keep the reads for which all alignments are to the extra reference sequences added with --genomeFastaFiles at the mapping stage.
        |KeepOnlyAddedReferences
        ///keep all alignments to the extra reference sequences added with --genomeFastaFiles at the mapping stage.
        |KeepAllAddedReferences

        static member make = function
            |KeepOnlyAddedReferences    -> "KeepOnlyAddedReferences"
            |KeepAllAddedReferences     -> "KeepAllAddedReferences"

    type OutputFormattingOptions =
        ///mode of SAM output
        |OutputMode             of OutputModeOptions
        ///Cufflinks-like strand field flag
        |StrandField            of StrandFieldOptions
        ///desired SAM attributes, in the order desired for the output SAM
        |Attributes             of AttributeOptions list
        ///start value for the IH attribute. 0 may be required by some downstream software, such as Cufflinks or StringTie.
        |AttributeIHSTart       of int 
        ///output of unmapped reads in the SAM format
        |UnmappedReads          of UnmappedReadOptions
        ///type of sorting for the SAM output
        |Sorting                of OutputSortingOptions
        ///which alignments are considered primary - all others will be marked with 0x100 bit in the FLAG
        |PrimaryFlag            of PrimaryFlagOptions
        ///read ID record type
        |ReadID                 of ReadIDOptions
        ///0 to 255: the MAPQ value for unique mappers
        |MAPQUinque             of int
        ///0 to 65535: sam FLAG will be bitwise OR'd with this value, i.e. FLAG=FLAG | outSAMflagOR. This is applied after all flags have been set by STAR, and after outSAMflagAND. Can be used to set specific bits that are not set otherwise.
        |FlagOR                 of int
        ///0 to 65535: sam FLAG will be bitwise AND'd with this value, i.e. FLAG=FLAG & outSAMflagOR. This is applied after all flags have been set by STAR, but before outSAMflagOR. Can be used to unset specific bits that are not set otherwise.
        |FlagAND                of int
        ///SAM/BAM read group line. The first word contains the read group identifier and must start with "ID:", e.g. --outSAMattrRGline ID:xxx CN:yy "DS:z z z".xxx will be added as RG tag to each output alignment. Any spaces in the tag values have to be double quoted. Comma separated RG lines correspons to different (comma separated) input files in --readFilesIn. Commas have to be surrounded by spaces, e.g. --outSAMattrRGline ID:xxx , ID:zzz "DS:z z" , ID:yyy DS:yyyy
        |ReadGroupLine          of string
        ///@HD (header) line of the SAM header
        |HeaderLine             of string 
        ///extra @PG (software) line of the SAM header (in addition to STAR)
        |PGLine                 of string
        ///path to the file with @CO (comment) lines of the SAM header
        |CommentLineFile        of string
        ///filter the output into main SAM/BAM files
        |Filter                 of OutputFilterOptions
        ///max number of multiple alignments for a read that will be output to the SAM/BAM files.
        |MaxMultipleAlignments  of int
        ///calculation method for the TLEN field in the SAM/BAM files. 1 ... leftmost base of the (+)strand mate to rightmost base of the (-)mate. (+)sign for the (+)strand mate 2 ... leftmost base of any mate to rightmost base of any mate. (+)sign for the mate with the leftmost base. This is different from 1 for overlapping mates with protruding ends  
        |TLenCalculation        of int

        static member makeCmd = function 
            |OutputMode                m ->["--outSAMmode"              ; m |> OutputModeOptions.make]
            |StrandField               s ->["--outSAMstrandField"       ; s |> StrandFieldOptions.make]
            |Attributes                a ->["--outSAMattributes"        ; a |> List.map AttributeOptions.make |> String.concat " "]
            |AttributeIHSTart          s ->["--outSAMattrIHstart"       ; s |> string]
            |UnmappedReads             u ->["--outSAMunmapped"          ; u |> UnmappedReadOptions.make]
            |Sorting                   s ->["--outSAMorder"             ; s |> OutputSortingOptions.make]
            |PrimaryFlag               p ->["--outSAMprimaryFlag"       ; p |> PrimaryFlagOptions.make]
            |ReadID                    r ->["--outSAMreadID"            ; r |> ReadIDOptions.make]
            |MAPQUinque                i ->["--outSAMmapqUnique"        ; i |> string]
            |FlagOR                    i ->["--outSAMflagOR"            ; i |> string]
            |FlagAND                   i ->["--outSAMflagAND"           ; i |> string]
            |ReadGroupLine             l ->["--outSAMattrRGline"        ; l]
            |HeaderLine                l ->["--outSAMheaderHD"          ; l]
            |PGLine                    l ->["--outSAMheaderPG"          ; l]
            |CommentLineFile           p ->["--outSAMheaderCommentFile" ; p]
            |Filter                    f ->["--outSAMfilter"            ; f |> OutputFilterOptions.make]
            |MaxMultipleAlignments     i ->["--outSAMmultNmax"          ; i |> string]
            |TLenCalculation           i ->["--outSAMtlen"              ; i |> string]

        static member makeCmdWith (m:MountInfo) = function 
            |OutputMode                m ->["--outSAMmode"              ; m |> OutputModeOptions.make]
            |StrandField               s ->["--outSAMstrandField"       ; s |> StrandFieldOptions.make]
            |Attributes                a ->["--outSAMattributes"        ; a |> List.map AttributeOptions.make |> String.concat " "]
            |AttributeIHSTart          s ->["--outSAMattrIHstart"       ; s |> string]
            |UnmappedReads             u ->["--outSAMunmapped"          ; u |> UnmappedReadOptions.make]
            |Sorting                   s ->["--outSAMorder"             ; s |> OutputSortingOptions.make]
            |PrimaryFlag               p ->["--outSAMprimaryFlag"       ; p |> PrimaryFlagOptions.make]
            |ReadID                    r ->["--outSAMreadID"            ; r |> ReadIDOptions.make]
            |MAPQUinque                i ->["--outSAMmapqUnique"        ; i |> string]
            |FlagOR                    i ->["--outSAMflagOR"            ; i |> string]
            |FlagAND                   i ->["--outSAMflagAND"           ; i |> string]
            |ReadGroupLine             l ->["--outSAMattrRGline"        ; l]
            |HeaderLine                l ->["--outSAMheaderHD"          ; l]
            |PGLine                    l ->["--outSAMheaderPG"          ; l]
            |CommentLineFile           p ->["--outSAMheaderCommentFile" ; p |> MountInfo.containerPathOf m]
            |Filter                    f ->["--outSAMfilter"            ; f |> OutputFilterOptions.make]
            |MaxMultipleAlignments     i ->["--outSAMmultNmax"          ; i |> string]
            |TLenCalculation           i ->["--outSAMtlen"              ; i |> string]

    ///mark duplicates in the BAM file, for now only works with (i) sorted BAM fed with inputBAMfile, and (ii) for paired-end alignments only
    type RemoveDuplicatesTypeOptions =
        ///no duplicate removal/marking
        |NoRemoval
        ///mark all multimappers, and duplicate unique mappers. The coordinates, FLAG, CIGAR must be identical
        |UniqueIdentical
        ///mark duplicate unique mappers but not multimappers
        |UniqueIdenticalNotMulti

        static member make = function
            |NoRemoval                  -> "-"
            |UniqueIdentical            -> "UniqueIdentical"
            |UniqueIdenticalNotMulti    -> "UniqueIdenticalNotMulti"
        
    type BAMHandlingOptions =
        ///BAM compression level, -1=default compression (6?), 0=no compression, 10=maximum compression
        |Compression            of int
        ///number of threads for BAM sorting. 0 will default to min(6,--runThreadN).
        |SortingThreadNumber    of int
        ///number of genome bins fo coordinate-sorting
        |SortingBinsNumber      of int
        ///mark duplicates in the BAM file, for now only works with (i) sorted BAM fed with inputBAMfile, and (ii) for paired-end alignments only
        |RemoveDuplicatesType   of RemoveDuplicatesTypeOptions
        ///number of bases from the 5' of mate 2 to use in collapsing (e.g. for RAMPAGE)
        |DuplicatesMate2basesN  of int

        static member makeCmd = function
            |Compression              i -> ["outBAMcompression"             ; i |> string]
            |SortingThreadNumber      i -> ["outBAMsortingThreadN"          ; i |> string]
            |SortingBinsNumber        i -> ["outBAMsortingBinsN"            ; i |> string]
            |RemoveDuplicatesType     r -> ["bamRemoveDuplicatesType"       ; r |> RemoveDuplicatesTypeOptions.make]
            |DuplicatesMate2basesN    i -> ["bamRemoveDuplicatesMate2basesN"; i |> string]

    type OutputParams =
        ///type of SAM/BAM output
        |Type               of OutputTypeOptions
        ///output files name prefix (including full or relative path). Can only be defined on the command line.
        |FileNamePrefix     of string
        ///path to a directory that will be used as temporary by STAR. All contents of this directory will be removed! the temp directory will default to outFileNamePrefix_STARtmp
        |TmpDir             of string
        ///whether to keep the temporary files after STAR runs is finished
        |TmpKeep            of bool (*true -> All false -> None*)
        ///which output will be directed to stdout (standard out)
        |Std                of OutputSTDOptions
        ///output of unmapped and partially mapped (i.e. mapped only one mate of a paired end read) reads in separate file(s). either no output or output in separate fasta/fastq files, Unmapped.out.mate1/2
        |KeepUnmappedReads  of bool (*true -> Fastx false -> None*)
        ///int: add this number to the quality score (e.g. to convert from Illumina to Sanger, use -31)
        |QSconversionAdd    of int
        ///string: order of multimapping alignments in the output files
        |MultimapperOrder   of AlignmentOrder
        ///various SAM/BAM formatting options
        |Formatting         of OutputFormattingOptions list
        ///various BAM specific options
        |BAMHandling        of BAMHandlingOptions list

            static member makeCmd = function
                |FileNamePrefix    p -> ["--outFileNamePrefix"   ; p]
                |TmpDir            p -> ["--outTmpDir"           ; p]
                |TmpKeep           b -> ["--outTmpKeep"          ; (if b then "All" else "None")]
                |Std               s -> ["--outStd"              ; s |> OutputSTDOptions.make]
                |KeepUnmappedReads b -> ["--outReadsUnmapped"    ; (if b then "Fastx" else "None")]
                |QSconversionAdd   i -> ["--outQSconversionAdd"  ; i |> string]
                |MultimapperOrder  m -> ["--outMultimapperOrder" ; m |> AlignmentOrder.make]
                |Type              s -> ["--outSAMtype"          ; s |> OutputTypeOptions.make]
                |Formatting        f -> f |> List.map OutputFormattingOptions.makeCmd |> List.concat
                |BAMHandling       f -> f |> List.map BAMHandlingOptions.makeCmd |> List.concat

            static member makeCmdWith (m:MountInfo) = function
                |FileNamePrefix    p -> ["--outFileNamePrefix"   ; p]
                |TmpDir            p -> ["--outTmpDir"           ; p |> MountInfo.containerPathOf m]
                |TmpKeep           b -> ["--outTmpKeep"          ; (if b then "All" else "None")]
                |Std               s -> ["--outStd"              ; s |> OutputSTDOptions.make]
                |KeepUnmappedReads b -> ["--outReadsUnmapped"    ; (if b then "Fastx" else "None")]
                |QSconversionAdd   i -> ["--outQSconversionAdd"  ; i |> string]
                |MultimapperOrder  m -> ["--outMultimapperOrder" ; m |> AlignmentOrder.make]
                |Type              s -> ["--outSAMtype"          ; s |> OutputTypeOptions.make]
                |Formatting        f -> f |> List.map (OutputFormattingOptions.makeCmdWith m) |> List.concat
                |BAMHandling       f -> f |> List.map BAMHandlingOptions.makeCmd |> List.concat

    type WiggleSignal =
        ///Do not specify Wiggle signal
        |All
        ///signal from only 5' of the 1st read, useful for CAGE/RAMPAGE etc
        |Read1_5p
        ///signal from only 2nd read
        |Read2   

        static member make = function
            |All
            |Read1_5p -> "read1_5p"
            |Read2    -> "read2"

    ///type of signal output, e.g. "bedGraph" OR "bedGraph read1_5p". Requires sorted BAM: --outSAMtype BAM SortedByCoordinate .
    type WiggleTypeOptions =
        ///no signal output
        |NoSignal
        ///bedGraph format
        |BedGraph   of WiggleSignal
        ///wiggle format
        |Wiggle     of WiggleSignal

        static member make = function
            |NoSignal   -> "None"
            |BedGraph w -> match w with | All -> "bedGraph" | Read1_5p | Read2 -> sprintf "bedGraph %s" (w |> WiggleSignal.make)
            |Wiggle   w -> match w with | All -> "wiggle"   | Read1_5p | Read2 -> sprintf "wiggle %s" (w |> WiggleSignal.make)

    type WiggleStrandOptions =
        ///separate strands, str1 and str2
        | Stranded  
        ///collapsed strands
        | Unstranded

        static member make = function
            | Stranded      -> "Stranded"
            | Unstranded    -> "Unstranded"

    type WiggleNormalizationOptions =
        ///no normalization, "raw" counts
        |NoNorm
        ///reads per million of mapped reads
        |RPM

        static member make = function
            |NoNorm -> "NoNorm"
            |RPM    -> "RPM"

    type OutputWiggleParams =
        ///type of signal output, e.g. "bedGraph" OR "bedGraph read1_5p". Requires sorted BAM: --outSAMtype BAM SortedByCoordinate .
        |WiggleType         of WiggleTypeOptions
        ///strandedness of wiggle/bedGraph output
        |WiggleStrand       of WiggleStrandOptions
        ///prefix matching reference names to include in the output wiggle file, e.g. "chr", default "-" - include all references
        |ReferencesPrefix   of string
        ///type of normalization for the signal
        |Normalization      of WiggleNormalizationOptions

        static member makeCmd = function
            |WiggleType       w -> ["--outWigType"               ; w |> WiggleTypeOptions.make]
            |WiggleStrand     w -> ["--outWigStrand"             ; w |> WiggleStrandOptions.make]
            |ReferencesPrefix w -> ["--outWigReferencesPrefix"   ; w ]
            |Normalization    w -> ["--outWigNorm"               ; w |> WiggleNormalizationOptions.make]

    type OutputFilterTypeOptions =
        ///standard filtering using only current alignment
        | Normal 
        ///BySJout ... keep only those reads that contain junctions that passed filtering into SJ.out.tab
        | BySJout

        static member make = function
            | Normal  -> "Normal"
            | BySJout -> "BySJout"

    ///filter alignment using their motifs
    type OutputFilterIntronMotifsOptions =
        ///no filtering
        |NoFilter                         
        ///filter out alignments that contain non-canonical junctions
        |RemoveNoncanonical           
        ///filter out alignments that contain non-canonical unannotated junctions when using annotated splice junctions database. The annotated non-canonical junctions will be kept.
        |RemoveNoncanonicalUnannotated

        static member make = function
            |NoFilter                      -> "None"
            |RemoveNoncanonical            -> "RemoveNoncanonical"
            |RemoveNoncanonicalUnannotated -> "RemoveNoncanonicalUnannotated"

    ///which reads to consider for collapsed splice junctions output
    type SpliceJunctionsFilterReadOptions =
        ///all reads, unique- and multi-mappers
        |All
        ///Unique: uniquely mapping reads only
        |Unique

        static member make = function
            |All    -> "All"
            |Unique -> "Unique"

    type OutputFilteringSpliceJunctionsOptions =
        ///which reads to consider for collapsed splice junctions output
        |FilterRead         of SpliceJunctionsFilterReadOptions
        ///minimum overhang length for splice junctions on both sides for: (1) non-canonical motifs, (2) GT/AG and CT/AC motif, (3) GC/AG and CT/GC motif, (4) AT/AC and GT/AT motif. -1 means no output for that motif does not apply to annotated junctions
        |OverhangMin        of (int*int*int*int)
        ///minimum uniquely mapping read count per junction for: (1) non-canonical motifs, (2) GT/AG and CT/AC motif, (3) GC/AG and CT/GC motif, (4) AT/AC and GT/AT motif. -1 means no output for that motif Junctions are output if one of outSJfilterCountUniqueMin OR outSJfilterCountTotalMin conditions are satisfied does not apply to annotated junctions
        |CountUniqueMin     of (int*int*int*int)
        ///minimum total (multi-mapping+unique) read count per junction for: (1) non-canonical motifs, (2) GT/AG and CT/AC motif, (3) GC/AG and CT/GC motif, (4) AT/AC and GT/AT motif. -1 means no output for that motif Junctions are output if one of outSJfilterCountUniqueMin OR outSJfilterCountTotalMin conditions are satisfied does not apply to annotated junctions
        |CountTotalMin      of (int*int*int*int)
        ///minimum allowed distance to other junctions' donor/acceptor does not apply to annotated junctions
        |DistToOtherSJmin   of (int*int*int*int)
        ///maximum gap allowed for junctions supported by 1,2,3,,,N reads i.e. by default junctions supported by 1 read can have gaps <=50000b, by 2 reads: <=100000b, by 3 reads: <=200000. by >=4 reads any gap <=alignIntronMax does not apply to annotated junctions
        |IntronMaxVsReadN   of int list

        static member makeCmd = function
            |FilterRead       f -> ["--outSJfilterReads"              ; f |> SpliceJunctionsFilterReadOptions.make]
            |OverhangMin      f -> ["--outSJfilterOverhangMin"        ; f |> handleQuadruples ]
            |CountUniqueMin   f -> ["--outSJfilterCountUniqueMin"     ; f |> handleQuadruples ]
            |CountTotalMin    f -> ["--outSJfilterCountTotalMin"      ; f |> handleQuadruples ]
            |DistToOtherSJmin f -> ["--outSJfilterDistToOtherSJmin"   ; f |> handleQuadruples ]
            |IntronMaxVsReadN f -> ["--outSJfilterIntronMaxVsReadN"   ; f |> List.map string |> String.concat " "]

    type OutputFilteringParams =
        ///type of filtering
        |FilterType                         of OutputFilterTypeOptions
        ///the score range below the maximum score for multimapping alignments
        |MultimapScoreRange                 of int
        ///maximum number of loci the read is allowed to map to. Alignments (all of them) will be output only if the read maps to no more loci than this value. Otherwise no alignments will be output, and the read will be counted as "mapped to too many loci" in the Log.final.out .
        |MultimapNmax                       of int
        ///alignment will be output only if it has no more mismatches than this value.
        |MismatchNmax                       of int
        ///alignment will be output only if its ratio of mismatches to *mapped* length is less than or equal to this value.
        |MismatchNoverLmax                  of float
        ///alignment will be output only if its ratio of mismatches to *read* length is less than or equal to this value.
        |MismatchNoverReadLmax              of float
        ///alignment will be output only if its score is higher than or equal to this value.
        |ScoreMin                           of int
        ///same as outFilterScoreMin, but  normalized to read length (sum of mates' lengths for paired-end reads)
        |ScoreMinOverLread                  of float
        ///alignment will be output only if the number of matched bases is higher than or equal to this value.
        |MatchNmin                          of int
        ///same as outFilterMatchNmin, but normalized to the read length (sum of mates' lengths for paired-end reads).
        |MatchNminOverLread                 of float
        ///filter alignment using their motifs
        |IntronMotifs                       of OutputFilterIntronMotifsOptions
        ///filter alignments: remove alignments that have junctions with inconsistent strands or no filtering
        |RemoveInconsistentIntronStrands    of bool //(true -> RemoveInconsistentStrands false -> None)
        ///various splice junction filtering options
        |SpliceJunctionsFiltering           of OutputFilteringSpliceJunctionsOptions list

        static member makeCmd = function
            |FilterType                       f -> ["--outFilterType"                  ; f |> OutputFilterTypeOptions.make ]
            |MultimapScoreRange               i -> ["--outFilterMultimapScoreRange"    ; i |> string]
            |MultimapNmax                     i -> ["--outFilterMultimapNmax"          ; i |> string]
            |MismatchNmax                     i -> ["--outFilterMismatchNmax"          ; i |> string]
            |MismatchNoverLmax                f -> ["--outFilterMismatchNoverLmax"     ; f |> string]
            |MismatchNoverReadLmax            f -> ["--outFilterMismatchNoverReadLmax" ; f |> string]
            |ScoreMin                         i -> ["--outFilterScoreMin"              ; i |> string]
            |ScoreMinOverLread                f -> ["--outFilterScoreMinOverLread"     ; f |> string]
            |MatchNmin                        i -> ["--outFilterMatchNmin"             ; i |> string]
            |MatchNminOverLread               f -> ["--outFilterMatchNminOverLread"    ; f |> string]
            |IntronMotifs                     m -> ["--outFilterIntronMotifs"          ; m |> OutputFilterIntronMotifsOptions.make]
            |RemoveInconsistentIntronStrands  b -> ["--outFilterIntronStrands"         ; (if b then "RemoveInconsistentStrands" else "None")]
            |SpliceJunctionsFiltering         s -> s |> List.map OutputFilteringSpliceJunctionsOptions.makeCmd |> List.concat
    type AlignmentScoringOptions =
        ///splice junction penalty (independent on intron motif)
        |Gap                    of int
        ///non-canonical junction penalty (in addition to scoreGap)
        |GapNoncan              of int
        ///GC/AG and CT/GC junction penalty (in addition to scoreGap)
        |GapGCAG                of int
        ///AT/AC  and GT/AT junction penalty  (in addition to scoreGap)
        |GapATAC                of int
        ///extra score logarithmically scaled with genomic length of the alignment: scoreGenomicLengthLog2scale*log2(genomicLength)
        |GenomicLengthLog2scal  of int
        ///deletion open penalty
        |DelOpen                of int
        ///deletion extension penalty per base (in addition to scoreDelOpen)
        |DelBase                of int
        ///insertion open penalty
        |InsOpen                of int
        ///insertion extension penalty per base (in addition to scoreInsOpen)
        |InsBase                of int
        ///maximum score reduction while searching for SJ boundaries inthe stitching step
        |StitchSJshift          of int

        static member makeCmd = function
            |Gap                   i    -> ["--scoreGap"                    ; i |> string ]
            |GapNoncan             i    -> ["--scoreGapNoncan"              ; i |> string ]
            |GapGCAG               i    -> ["--scoreGapGCAG"                ; i |> string ]
            |GapATAC               i    -> ["--scoreGapATAC"                ; i |> string ]
            |GenomicLengthLog2scal f    -> ["--scoreGenomicLengthLog2scale" ; f |> string ]
            |DelOpen               i    -> ["--scoreDelOpen"                ; i |> string ]
            |DelBase               i    -> ["--scoreDelBase"                ; i |> string ]
            |InsOpen               i    -> ["--scoreInsOpen"                ; i |> string ]
            |InsBase               i    -> ["--scoreInsBase"                ; i |> string ]
            |StitchSJshift         i    -> ["--scoreStitchSJshift"          ; i |> string ]

    type AlignmentSeedOptions =
        /// defines the search start point through the read - the read is split into pieces no longer than this value
        |SearchStartLmax            of int
        /// seedSearchStartLmax normalized to read length (sum of mates' lengths for paired-end reads)
        |SearchStartLmaxOverLread   of float
        /// defines the maximum length of the seeds, if =0 max seed lengthis infinite
        |SearchLmax                 of int
        /// only pieces that map fewer than this value are utilized in the stitching procedure
        |MultimapNmax               of int
        /// max number of seeds per read
        |PerReadNmax                of int
        /// max number of seeds per window
        |PerWindowNmax              of int
        /// max number of one seed loci per window
        |NoneLociPerWindow          of int
        /// min length of the seed sequences split by Ns or mate gap
        |SplitMin                   of int

        static member makeCmd = function
            |SearchStartLmax          i -> ["--seedSearchStartLmax"         ; i |> string]
            |SearchStartLmaxOverLread f -> ["--seedSearchStartLmaxOverLread"; f |> string]
            |SearchLmax               i -> ["--seedSearchLmax"              ; i |> string]
            |MultimapNmax             i -> ["--seedMultimapNmax"            ; i |> string]
            |PerReadNmax              i -> ["--seedPerReadNmax"             ; i |> string]
            |PerWindowNmax            i -> ["--seedPerWindowNmax"           ; i |> string]
            |NoneLociPerWindow        i -> ["--seedNoneLociPerWindow"       ; i |> string]
            |SplitMin                 i -> ["--seedSplitMin"                ; i |> string]

    ///type of read ends alignment
    type AlignmentEndsTypeOptions =
        ///standard local alignment with soft-clipping allowed
        |Local            
        ///force end-to-end read alignment, do not soft-clip
        |EndToEnd         
        ///fully extend only the 5p of the read1, all other ends: local alignment
        |Extend5pOfRead1  
        ///fully extend only the 5p of the both read1 and read2, all other ends: local alignment
        |Extend5pOfReads12

        static member make = function
            |Local              -> "Local"
            |EndToEnd           -> "EndToEnd"
            |Extend5pOfRead1    -> "Extend5pOfRead1"
            |Extend5pOfReads12  -> "Extend5pOfReads12"

    ///maximum and type of allowed allow protrusion of alignment ends, i.e. start (end) of the +strand mate downstream of the start (end) of the -strand mate
    type AlignmentEndsProtrusionOptions =
        ///report aligments with non-zero protrusion as concordant pairs
        |ConcordantPair of int
        ///report alignments with non-zero protrusion as discordant pairs
        |DiscordantPair of int

        static member make = function
            |ConcordantPair i -> sprintf "%i ConcordantPair" i
            |DiscordantPair i -> sprintf "%i DiscordantPair" i

    ///how to flush ambiguous insertion positions
    type AlignmentInsertionFlushOptions =
        |DoNotFlush
        |Right

        static member make = function   
            |DoNotFlush -> "None"
            |Right      -> "Right"

    type AligmentPairedEndReadOptions =
        ///minimum number of overlap bases to trigger mates merging and realignment
        |OverlapNbasesMin   of int
        ///maximum proportion of mismatched bases in the overlap area
        |OverlapMMp         of float

        static member makeCmd = function
            |OverlapNbasesMin i -> ["--peOverlapNbasesMin" ; i |> string]
            |OverlapMMp       f -> ["--peOverlapMMp"       ; f |> string]

    type AlignmentGeneralOptions = 
        ///minimum intron size: genomic gap is considered intron if its length>=alignIntronMin, otherwise it is considered Deletion
        |IntronMin                      of int
        ///maximum intron size, if 0, max intron size will be determined by (2^winBinNbits)*winAnchorDistNbins
        |IntronMax                      of int
        ///maximum gap between two mates, if 0, max intron gap will be determined by (2^winBinNbits)*winAnchorDistNbins
        |MatesGapMax                    of int
        ///minimum overhang (i.e. block size) for spliced alignments
        |SJoverhangMin                  of int
        ///maximum number of mismatches for stitching of the splice junctions (-1: no limit). (1) non-canonical motifs, (2) GT/AG and CT/AC motif, (3) GC/AG and CT/GC motif, (4) AT/AC and GT/AT motif.
        |SJstitchMismatchNmax           of (int*int*int*int)
        ///minimum overhang (i.e. block size) for annotated (sjdb) spliced alignments
        |SJDBoverhangMin                of int
        ///minimum mapped length for a read mate that is spliced
        |SplicedMateMapLmin             of int
        ///alignSplicedMateMapLmin normalized to mate length
        |SplicedMateMapLminOverLmate    of float
        ///max number of windows per read
        |WindowsPerReadNmax             of int
        ///max number of transcripts per window
        |TranscriptsPerWindowNmax       of int
        ///max number of different alignments per read to consider
        |TranscriptsPerReadNmax         of int
        ///type of read ends alignment
        |EndsType                       of AlignmentEndsTypeOptions
        ///maximum and type of allowed allow protrusion of alignment ends, i.e. start (end) of the +strand mate downstream of the start (end) of the -strand mate
        |EndsProtrusion                 of AlignmentEndsProtrusionOptions
        ///allow the soft-clipping of the alignments past the end of the chromosomes: allow, or prohibit(useful for compatibility with Cufflinks)
        |SoftClipAtReferenceEnds        of bool //true -> Yes false -> No
        ///string: how to flush ambiguous insertion positions
        |InsertionFlush                 of AlignmentInsertionFlushOptions
        ///options how to handle paired end reads
        |PairedEndReadOptions           of AligmentPairedEndReadOptions list

        static member makeCmd = function
            |IntronMin                   p -> ["--alignIntronMin"                   ; p |> string]
            |IntronMax                   p -> ["--alignIntronMax"                   ; p |> string]
            |MatesGapMax                 p -> ["--alignMatesGapMax"                 ; p |> string]
            |SJoverhangMin               p -> ["--alignSJoverhangMin"               ; p |> string]
            |SJstitchMismatchNmax        p -> ["--alignSJstitchMismatchNmax"        ; p |> handleQuadruples]
            |SJDBoverhangMin             p -> ["--alignSJDBoverhangMin"             ; p |> string]
            |SplicedMateMapLmin          p -> ["--alignSplicedMateMapLmin"          ; p |> string]
            |SplicedMateMapLminOverLmate p -> ["--alignSplicedMateMapLminOverLmate" ; p |> string]
            |WindowsPerReadNmax          p -> ["--alignWindowsPerReadNmax"          ; p |> string]
            |TranscriptsPerWindowNmax    p -> ["--alignTranscriptsPerWindowNmax"    ; p |> string]
            |TranscriptsPerReadNmax      p -> ["--alignTranscriptsPerReadNmax"      ; p |> string]
            |EndsType                    p -> ["--alignEndsType"                    ; p |> AlignmentEndsTypeOptions.make]
            |EndsProtrusion              p -> ["--alignEndsProtrude"                ; p |> AlignmentEndsProtrusionOptions.make]
            |SoftClipAtReferenceEnds     p -> ["--alignSoftClipAtReferenceEnds"     ; (if p then "Yes" else "No")]
            |InsertionFlush              p -> ["--alignInsertionFlush"              ; p |> AlignmentInsertionFlushOptions.make]
            |PairedEndReadOptions        p -> p |> List.map AligmentPairedEndReadOptions.makeCmd |> List.concat
    
    ///Windows, Anchors, Binning Options
    type AlignmentWindowOptions =
        ///max number of loci anchors are allowed to map to
        |AnchorMultimapNmax      of int
        ///=log2(winBin), where winBin is the size of the bin for the windows/clustering, each window will occupy an integer number of bins.
        |BinNbits                of int
        ///max number of bins between two anchors that allows aggregation of anchors into one window
        |AnchorDistNbins         of int
        ///log2(winFlank), where win Flank is the size of the left and right flanking regions for each window
        |FlankNbins              of int
        ///minimum relative coverage of the read sequence by the seeds in a window, for STARlong algorithm only.
        |ReadCoverageRelativeMin of float
        ///minimum number of bases covered by the seeds in a window , for STARlong algorithm only.
        |ReadCoverageBasesMin    of int

        static member makeCmd = function
            |AnchorMultimapNmax       i -> ["--winAnchorMultimapNmax"       ; i |> string ]
            |BinNbits                 i -> ["--winBinNbits"                 ; i |> string ]
            |AnchorDistNbins          i -> ["--winAnchorDistNbins"          ; i |> string ]
            |FlankNbins               i -> ["--winFlankNbins"               ; i |> string ]
            |ReadCoverageRelativeMin  f -> ["--winReadCoverageRelativeMin"  ; f |> string ]
            |ReadCoverageBasesMin     i -> ["--winReadCoverageBasesMin"     ; i |> string ]

    ///type of chimeric output
    type ChimericAlignmentOutputTypeOptions =
        ///Chimeric.out.junction
        ///output old SAM into separate Chimeric.out.sam file
        ///output into main aligned BAM files (Aligned.*.bam)
        ///(default) hard-clipping in the CIGAR for supplemental chimeric alignments (defaultif no 2nd word is present)
        ///soft-clipping in the CIGAR for supplemental chimeric alignments
        |Junctions         
        |SeparateSAMold    
        |WithinBAM         
        |WithinBAMHardClip
        |WithinBAMSoftClip
            
        static member make = function 
            |Junctions          -> "Junctions"
            |SeparateSAMold     -> "SeparateSAMold"
            |WithinBAM          -> "WithinBAM"
            |WithinBAMHardClip  -> "WithinBAM HardClip"
            |WithinBAMSoftClip  -> "WithinBAM SoftClip"
    ///different filters for chimeric alignments
    type ChimericAlignmentFilterOptions =
        ///no filtering
        |NoFiltering
        ///Ns are not allowed in the genome sequence around the chimeric junction
        |BanGenomicN

        static member make = function
           |NoFiltering -> "None"
           |BanGenomicN -> "banGenomicN"

    ///formatting type for the Chimeric.out.junction file
    type ChimericOutJunctionFormatOptions =
        ///no comment lines/headers
        |NoComments
        ///comment lines at the end of the file: command line and Nreads: total, unique, multi
        |CommentLines

        static member make = function
            |NoComments     -> "0"
            |CommentLines   -> "1"

    type AlignmentChimericOptions =
        ///type of chimeric output
        |ChimericAlignmentOutputType    of ChimericAlignmentOutputTypeOptions
        ///minimum length of chimeric segment length, if ==0, no chimeric output
        |SegmentMin                     of int
        ///minimum total (summed) score of the chimeric segments
        |ScoreMin                       of int
        ///max drop (difference) of chimeric score (the sum of scores of all chimeric segments) from the read length
        |ScoreDropMax                   of int
        ///minimum difference (separation) between the best chimeric score and the next one
        |ScoreSeparation                of int
        ///penalty for a non-GT/AG chimeric junction
        |ScoreJunctionNonGTAG           of int
        ///minimum overhang for a chimeric junction
        |JunctionOverhangMin            of int
        ///maximum gap in the read sequence between chimeric segments
        |SegmentReadGapMax              of int
        ///different filters for chimeric alignments
        |ChimericAlignmentFilter        of ChimericAlignmentFilterOptions
        ///maximum number of multi-alignments for the main chimeric segment. =1 will prohibit multimapping main segments.
        |MainSegmentMultNmax            of int
        ///maximum number of chimeric multi-alignments use the old scheme for chimeric detection which only considered unique alignments
        |MultimapNmax                   of int
        ///the score range for multi-mapping chimeras below the best chimeric score. Only works with --chimMultimapNmax > 1
        |MultimapScoreRange             of int
        ///to trigger chimeric detection, the drop in the best non-chimeric alignment score with respect to the read length has to be greater than this value
        |NonchimScoreDropMin            of int
        ///formatting type for the Chimeric.out.junction file
        |OutJunctionFormat              of ChimericOutJunctionFormatOptions

        static member makeCmd = function
            |ChimericAlignmentOutputType c -> ["--chimOutType"              ; c |> ChimericAlignmentOutputTypeOptions.make]
            |SegmentMin                  c -> ["--chimSegmentMin"           ; c |> string]
            |ScoreMin                    c -> ["--chimScoreMin"             ; c |> string]
            |ScoreDropMax                c -> ["--chimScoreDropMax"         ; c |> string]
            |ScoreSeparation             c -> ["--chimScoreSeparation"      ; c |> string]
            |ScoreJunctionNonGTAG        c -> ["--chimScoreJunctionNonGTAG" ; c |> string]
            |JunctionOverhangMin         c -> ["--chimJunctionOverhangMin"  ; c |> string]
            |SegmentReadGapMax           c -> ["--chimSegmentReadGapMax"    ; c |> string]
            |ChimericAlignmentFilter     c -> ["--chimFilter"               ; c |> ChimericAlignmentFilterOptions.make]
            |MainSegmentMultNmax         c -> ["--chimMainSegmentMultNmax"  ; c |> string]
            |MultimapNmax                c -> ["--chimMultimapNmax"         ; c |> string]
            |MultimapScoreRange          c -> ["--chimMultimapScoreRange"   ; c |> string]
            |NonchimScoreDropMin         c -> ["--chimNonchimScoreDropMin"  ; c |> string]
            |OutJunctionFormat           c -> ["--chimOutJunctionFormat"    ; c |> ChimericOutJunctionFormatOptions.make]

    type AlignmentParams =
        ///various options regarding alignment scoring
        |ScoringOptions     of AlignmentScoringOptions  list
        ///various options regarding alignment seeds
        |SeedOptions        of AlignmentSeedOptions     list
        ///various general alignment options
        |AlignmentOptions   of AlignmentGeneralOptions  list
        ///various options regarding alignment windows
        |WindowOptions      of AlignmentWindowOptions   list
        ///various options regarding chimeric alignments
        |ChimericOptions    of AlignmentChimericOptions list

        static member makeCmd = function
            |ScoringOptions     s -> s |> List.map AlignmentScoringOptions.makeCmd  |> List.concat
            |SeedOptions        a -> a |> List.map AlignmentSeedOptions.makeCmd     |> List.concat
            |AlignmentOptions   a -> a |> List.map AlignmentGeneralOptions.makeCmd  |> List.concat
            |WindowOptions      w -> w |> List.map AlignmentWindowOptions.makeCmd   |> List.concat
            |ChimericOptions    c -> c |> List.map AlignmentChimericOptions.makeCmd |> List.concat

//### Quantification of Annotations

    ///types of quantification requested
    type AnnotationQuantificationModeOptions =
        ///none
        | NoQuantification
        ///output SAM/BAM alignments to transcriptome into a separate file
        | TranscriptomeSAM
        ///count reads per gene
        | GeneCounts      
        
        static member make = function 
            | NoQuantification -> "-"
            | TranscriptomeSAM -> "TranscriptomeSAM"
            | GeneCounts       -> "GeneCounts"

    ///prohibit various alignment type
    type TranscriptomeBanOptions =
        ///prohibit indels, soft clipping and single-end alignments - compatible with RSEM
        |IndelSoftclipSingleend
        ///prohibit single-end alignments
        |Singleend             

        static member make = function
            |IndelSoftclipSingleend -> "IndelSoftclipSingleend"
            |Singleend              -> "Singleend"

    type AnnotationQuantificationParams =
        ///types of quantification requested
        |QuantificationMode             of AnnotationQuantificationModeOptions
        ///-2 to 10  transcriptome BAM compression level
        |TranscriptomeBAMcompression    of int
        ///prohibit various alignment type
        |TranscriptomeBan               of TranscriptomeBanOptions
        
        static member makeCmd = function
            |QuantificationMode          a -> ["--quantMode"                        ; a |> AnnotationQuantificationModeOptions.make]
            |TranscriptomeBAMcompression i -> ["--quantTranscriptomeBAMcompression" ; i |> string]
            |TranscriptomeBan            a -> ["--quantTranscriptomeBan"            ; a |> TranscriptomeBanOptions.make]

    ///type of filtering
//### 2-pass Mapping 

    ///2-pass mapping mode.
    type TwoPassModeOptions = 
        ///1-pass mapping
        |NoTwoPassMapping
        ///basic 2-pass mapping, with all 1st pass junctions inserted into the genome indices on the fly
        |Basic

        static member make = function
            |NoTwoPassMapping   -> "None"
            |Basic              -> "Basic"

    type TwoPassMappingParams =
        ///2-pass mapping mode.
        |Mode           of TwoPassModeOptions
        ///number of reads to process for the 1st step. Use very large number (or default -1) to map all reads in the first step.
        |FirstStepReadN of int

        static member makeCmd = function
            |Mode           m -> ["--twopassMode"   ; m |> TwoPassModeOptions.make]
            |FirstStepReadN i -> ["--twopass1readsN"; i |> string]

//### WASP parameters

    ///WASP allele-specific output type. This is re-implemenation of the original WASP mappability filtering by Bryce van de Geijn, Graham McVicker, Yoav Gilad & Jonathan K Pritchard. Please cite the original WASP paper: Nature Methods 12, 1061–1063 (2015), https://www.nature.com/articles/nmeth.3582 .
    type WASPOutputModeOptions =
        |NoWASP
        |SAMtag

        static member make = function
            |NoWASP -> "None"
            |SAMtag -> "SAMtag"

    type WASPParams =
        ///WASP allele-specific output type. This is re-implemenation of the original WASP mappability filtering by Bryce van de Geijn, Graham McVicker, Yoav Gilad & Jonathan K Pritchard. Please cite the original WASP paper: Nature Methods 12, 1061–1063 (2015), https://www.nature.com/articles/nmeth.3582 .
        |OutputMode of WASPOutputModeOptions

        static member makeCmd = function
            |OutputMode o -> ["--waspOutputMode"; o |> WASPOutputModeOptions.make]

//### STARsolo (single cell RNA-seq) parameters

    ///type of single-cell RNA-seq
    type STARSoloTypeOptions =
        ///None
        |NoSoloType
        ///(a.k.a. Droplet) one UMI and one Cell Barcode of fixed length in read2, e.g. Drop-seq and 10X Chromium
        |CB_UMI_Simple 
        ///one UMI of fixed length, but multiple Cell Barcodes of varying length, as well as adapters sequences are allowed in read2 only, e.g. inDrop.
        |CB_UMI_Complex

        static member make = function
            |NoSoloType     -> "None"
            |CB_UMI_Simple  -> "CB_UMI_Simple "
            |CB_UMI_Complex -> "CB_UMI_Complex"

    ///length of the barcode read
    type STARSoloBarcodeReadLengthOptions =
        ///not defined, do not check
        |NotDefined
        ///equal to sum of soloCBlen+soloUMIlen
        |Sum

        static member make = function
            |NotDefined -> "0"
            |Sum        -> "1"

    ///matching the Cell Barcodes to the WhiteList
    type STARSoloCellBarcodeWhiteListOptions =
        ///only exact matches allowed
        |Exact    
        ///only one match in whitelist with 1 mismatched base allowed. Allowed CBs have to have at least one read with exact match.
        |OneMM      
        ///multiple matches in whitelist with 1 mismatched base allowed, posterior probability calculation is used choose one of the matches. Allowed CBs have to have at least one read with exact match. Similar to CellRanger 2.2.0
        |OneMM_multi
        ///same as 1MM_Multi, but pseudocounts of 1 are added to all whitelist barcodes. Similar to CellRanger 3.x.x
        |OneMM_multi_pseudocounts

        static member make = function   
            |Exact                      -> "Exact"
            |OneMM                      -> "1MM"
            |OneMM_multi                -> "1MM_multi"
            |OneMM_multi_pseudocounts   -> "1MM_multi_pseudocounts"

    ///strandedness of the solo libraries:
    type STARSoloStrandOptions =
        ///no strand information
        |Unstranded
        ///read strand same as the original RNA molecule
        |Forward   
        ///read strand opposite to the original RNA molecule
        |Reverse   

        static member make = function
            |Unstranded -> "Unstranded"
            |Forward    -> "Forward"
            |Reverse    -> "Reverse"

    ///genomic features for which the UMI counts per Cell Barcode are collected
    type STARSoloFeaturesOptions =
        ///genes: reads match the gene transcript
        |Gene           
        ///splice junctions: reported in SJ.out.tab
        |SJ          
        ///full genes: count all reads overlapping genes' exons and introns
        |GeneFull    
        ///quantification of transcript for 3' protocols
        |Transcript3p

        static member make = function
            |Gene         -> "Gene"
            |SJ           -> "SJ"
            |GeneFull     -> "GeneFull"
            |Transcript3p -> "Transcript3p"

    ///type of UMI deduplication (collapsing) algorithm
    type STARSoloUMIDeDuplicationOptions =
        ///all UMIs with 1 mismatch distance to each other are collapsed (i.e. counted once)
        |OneMM_All        
        ///follows the "directional" method from the UMI-tools by Smith, Heger and Sudbery (Genome Research 2017).
        |OneMM_Directional
        ///only exactly matching UMIs are collapsed
        |Exact          

        static member make = function
            |OneMM_All          -> "1MM_All"
            |OneMM_Directional  -> "1MM_Directional"
            |Exact              -> "Exact"

    ///type of UMI filtering
    type STARSoloUMIFilteringOptions =
        ///basic filtering: remove UMIs with N and homopolymers (similar to CellRanger 2.2.0)
        |Basic
        ///remove lower-count UMIs that map to more than one gene (introduced in CellRanger 3.x.x)
        |MultiGeneUMI

        static member make = function
              |Basic        -> "-"
              |MultiGeneUMI -> "MultiGeneUMI"
    
    ///cell filtering type and parameters
    type STARSoloCellFilterOptions =
        ///do not output filtered cells
        |NoCellFilter   
        ///simple filtering of CellRanger 2.2, followed by thre numbers: number of expected cells, robust maximum percentile for UMI count, maximum to minimum ratio for UMI count
        |CellRanger2_2
        ///only report top cells by UMI count, followed by the excat number of cells
        |TopCells     

        static member make = function
            |NoCellFilter   -> "None"
            |CellRanger2_2  -> "CellRanger2.2"
            |TopCells       -> "TopCells"

    type STARSoloParams =
        ///type of single-cell RNA-seq
        |SoloType                   of STARSoloTypeOptions
        ///length of the barcode read
        |BarcodeReadLengthOptions   of STARSoloBarcodeReadLengthOptions
        ///matching the Cell Barcodes to the WhiteList
        |CBmatchWLtype              of STARSoloCellBarcodeWhiteListOptions
        ///strandedness of the solo libraries
        |Strand                     of STARSoloStrandOptions
        ///genomic features for which the UMI counts per Cell Barcode are collected
        |Features                   of STARSoloFeaturesOptions list
        ///type of UMI deduplication (collapsing) algorithm
        |UMIdedup                   of STARSoloUMIDeDuplicationOptions list
        ///type of UMI filtering
        |UMIfiltering               of STARSoloUMIFilteringOptions list
        ///cell filtering type and parameters
        |CellFilter                 of STARSoloCellFilterOptions list
        ///file names for STARsolo output. default: file_name_prefix   gene_names   barcode_sequences   cell_feature_count_matrix
        |OutFileNames               of string list
        ///position of the UMI on the barcode read, same as soloCBposition Example: inDrop (Zilionis et al, Nat. Protocols, 2017): --soloCBposition  3_9_3_14
        |UMIposition                of string
        ///position of Cell Barcode(s) on the barcode read. Presently only works with --soloType CB_UMI_Complex, and barcodes are assumed to be on Read2. Format for each barcode: startAnchor_startDistance_endAnchor_endDistance start(end)Anchor defines the anchor base for the CB: 0: read start; 1: read end; 2: adapter start; 3: adapter end start(end)Distance is the distance from the CB start(end) to the Anchor base String for different barcodes are separated by space. Example: inDrop (Zilionis et al, Nat. Protocols, 2017): --soloCBposition  0_0_2_-1  3_1_3_8
        |CBposition                 of string
        ///adapter sequence to anchor barcodes.
        |AdapterSequence            of string
        ///file(s) with whitelist(s) of cell barcodes. Only one file allowed with
        |CBwhitelist                of string list
        ///cell barcode start base
        |CBstart                    of int
        ///cell barcode length
        |CBlen                      of int
        ///UMI start base
        |UMIstart                   of int
        ///UMI length
        |UMIlen                     of int
        ///maximum number of mismatches allowed in adapter sequence.
        |AdapterMismatchesNmax      of int

        static member makeCmd = function
            |SoloType                 s     -> ["--soloType"                  ; s |> STARSoloTypeOptions.make]
            |BarcodeReadLengthOptions s     -> ["--soloBarcodeReadLength"     ; s |> STARSoloBarcodeReadLengthOptions.make]
            |CBmatchWLtype            s     -> ["--soloCBmatchWLtype"         ; s |> STARSoloCellBarcodeWhiteListOptions.make]
            |Strand                   s     -> ["--soloStrand"                ; s |> STARSoloStrandOptions.make]
            |Features                 s     -> ["--soloFeatures"              ; s |> List.map STARSoloFeaturesOptions.make          |> String.concat " "]
            |UMIdedup                 s     -> ["--soloUMIdedup"              ; s |> List.map STARSoloUMIDeDuplicationOptions.make  |> String.concat " "]
            |UMIfiltering             s     -> ["--soloUMIfiltering"          ; s |> List.map STARSoloUMIFilteringOptions.make      |> String.concat " "]
            |CellFilter               s     -> ["--soloCellFilter"            ; s |> List.map STARSoloCellFilterOptions.make        |> String.concat " "]
            |OutFileNames             s     -> ["--soloOutFileNames"          ; s |> String.concat " "]
            |UMIposition              s     -> ["--soloUMIposition"           ; s ]
            |CBposition               s     -> ["--soloCBposition"            ; s ]
            |AdapterSequence          s     -> ["--soloAdapterSequence"       ; s ]
            |CBwhitelist              s     -> ["--soloCBwhitelist"           ; s |> String.concat " "]
            |CBstart                  s     -> ["--soloCBstart"               ; s |> string]
            |CBlen                    s     -> ["--soloCBlen"                 ; s |> string]
            |UMIstart                 s     -> ["--soloUMIstart"              ; s |> string]
            |UMIlen                   s     -> ["--soloUMIlen"                ; s |> string]
            |AdapterMismatchesNmax    s     -> ["--soloAdapterMismatchesNmax" ; s |> string]

        static member makeCmdWith (m:MountInfo) = function
            |SoloType                 s     -> ["--soloType"                  ; s |> STARSoloTypeOptions.make]
            |BarcodeReadLengthOptions s     -> ["--soloBarcodeReadLength"     ; s |> STARSoloBarcodeReadLengthOptions.make]
            |CBmatchWLtype            s     -> ["--soloCBmatchWLtype"         ; s |> STARSoloCellBarcodeWhiteListOptions.make]
            |Strand                   s     -> ["--soloStrand"                ; s |> STARSoloStrandOptions.make]
            |Features                 s     -> ["--soloFeatures"              ; s |> List.map STARSoloFeaturesOptions.make          |> String.concat " "]
            |UMIdedup                 s     -> ["--soloUMIdedup"              ; s |> List.map STARSoloUMIDeDuplicationOptions.make  |> String.concat " "]
            |UMIfiltering             s     -> ["--soloUMIfiltering"          ; s |> List.map STARSoloUMIFilteringOptions.make      |> String.concat " "]
            |CellFilter               s     -> ["--soloCellFilter"            ; s |> List.map STARSoloCellFilterOptions.make        |> String.concat " "]
            |OutFileNames             s     -> ["--soloOutFileNames"          ; s |> String.concat " "]
            |UMIposition              s     -> ["--soloUMIposition"           ; s ]
            |CBposition               s     -> ["--soloCBposition"            ; s ]
            |AdapterSequence          s     -> ["--soloAdapterSequence"       ; s ]
            |CBwhitelist              s     -> ["--soloCBwhitelist"           ; s |> List.map (MountInfo.containerPathOf m) |> String.concat " "]
            |CBstart                  s     -> ["--soloCBstart"               ; s |> string]
            |CBlen                    s     -> ["--soloCBlen"                 ; s |> string]
            |UMIstart                 s     -> ["--soloUMIstart"              ; s |> string]
            |UMIlen                   s     -> ["--soloUMIlen"                ; s |> string]
            |AdapterMismatchesNmax    s     -> ["--soloAdapterMismatchesNmax" ; s |> string]

    type STARParams =
    //    string: name of a user-defined parameters file, "-": none. Can only be defined on the command line.
    //    string: path to the VCF file that contains variation data.
    //    string: path to BAM input file, to be used with --runMode inputAlignmentsFromBAM
        |ParametersFilePath                 of string
        |VariationDataFile                  of string
        |InputBAMFile                       of string
        |RunParameters                      of RunParams                        list
        |GenomeParameters                   of GenomeParams                     list
        |SpliceJunctionsDatabaseParameters  of SpliceJunctionsDatabaseParams    list
        |LimitParameters                    of LimitParams                      list
        |ReadParameters                     of ReadParams                       list
        |OutputParameters                   of OutputParams                     list
        |OutputWiggleParameters             of OutputWiggleParams               list
        |OutputFilteringParameters          of OutputFilteringParams            list
        |AlignmentParameters                of AlignmentParams                  list
        |AnnotationQuantificationParameters of AnnotationQuantificationParams   list
        |TwoPassMappingParameters           of TwoPassMappingParams             list
        |WASPParameters                     of WASPParams                       list
        |STARSoloParameters                 of STARSoloParams                   list
        
        static member makeCmd = function
            |ParametersFilePath                 p -> ["--parametersFiles" ; p]
            |VariationDataFile                  p -> ["--varVCFfile"      ; p]
            |InputBAMFile                       p -> ["--inputBAMfile"    ; p]
            |RunParameters                      p -> p |> List.map RunParams                     .makeCmd |> List.concat
            |GenomeParameters                   p -> p |> List.map GenomeParams                  .makeCmd |> List.concat
            |SpliceJunctionsDatabaseParameters  p -> p |> List.map SpliceJunctionsDatabaseParams .makeCmd |> List.concat
            |LimitParameters                    p -> p |> List.map LimitParams                   .makeCmd |> List.concat
            |ReadParameters                     p -> p |> List.map ReadParams                    .makeCmd |> List.concat
            |OutputParameters                   p -> p |> List.map OutputParams                  .makeCmd |> List.concat
            |OutputWiggleParameters             p -> p |> List.map OutputWiggleParams            .makeCmd |> List.concat
            |OutputFilteringParameters          p -> p |> List.map OutputFilteringParams         .makeCmd |> List.concat
            |AlignmentParameters                p -> p |> List.map AlignmentParams               .makeCmd |> List.concat
            |AnnotationQuantificationParameters p -> p |> List.map AnnotationQuantificationParams.makeCmd |> List.concat
            |TwoPassMappingParameters           p -> p |> List.map TwoPassMappingParams          .makeCmd |> List.concat
            |WASPParameters                     p -> p |> List.map WASPParams                    .makeCmd |> List.concat
            |STARSoloParameters                 p -> p |> List.map STARSoloParams                .makeCmd |> List.concat

        static member makeCmdWith (m:MountInfo) = function
            |ParametersFilePath                 p -> ["--parametersFiles" ; p |> MountInfo.containerPathOf m]
            |VariationDataFile                  p -> ["--varVCFfile"      ; p |> MountInfo.containerPathOf m]
            |InputBAMFile                       p -> ["--inputBAMfile"    ; p |> MountInfo.containerPathOf m]
            |RunParameters                      p -> p |> List.map (RunParams                     .makeCmd) |> List.concat
            |GenomeParameters                   p -> p |> List.map (GenomeParams                  .makeCmdWith m) |> List.concat
            |SpliceJunctionsDatabaseParameters  p -> p |> List.map (SpliceJunctionsDatabaseParams .makeCmdWith m) |> List.concat
            |LimitParameters                    p -> p |> List.map (LimitParams                   .makeCmd) |> List.concat
            |ReadParameters                     p -> p |> List.map (ReadParams                    .makeCmdWith m) |> List.concat
            |OutputParameters                   p -> p |> List.map (OutputParams                  .makeCmdWith m) |> List.concat
            |OutputWiggleParameters             p -> p |> List.map (OutputWiggleParams            .makeCmd) |> List.concat
            |OutputFilteringParameters          p -> p |> List.map (OutputFilteringParams         .makeCmd) |> List.concat
            |AlignmentParameters                p -> p |> List.map (AlignmentParams               .makeCmd) |> List.concat
            |AnnotationQuantificationParameters p -> p |> List.map (AnnotationQuantificationParams.makeCmd) |> List.concat
            |TwoPassMappingParameters           p -> p |> List.map (TwoPassMappingParams          .makeCmd) |> List.concat
            |WASPParameters                     p -> p |> List.map (WASPParams                    .makeCmd) |> List.concat
            |STARSoloParameters                 p -> p |> List.map (STARSoloParams                .makeCmdWith m) |> List.concat


    let runSTARAsync (bcContext:BioContainer.BcContext) (opt:STARParams list)= 

        let cmds = (opt |> List.map (STARParams.makeCmdWith bcContext.Mount))
        let tp = "STAR"::(cmds |> List.concat)

        printfn "Starting process STAR\r\nparameters:"
        cmds |> List.iter (fun op -> printfn "\t%s" (String.concat " " op))

        async {
                let! res = BioContainer.execAsync bcContext tp           
                return res
        }

    let runStar (bcContext:BioContainer.BcContext) (opt:STARParams list) = 
        runSTARAsync bcContext opt 
        |> Async.RunSynchronously

    module BasicWorkflows =

        let runBasicGenomeIndexingAsync threads directory genomesFastaPaths gtfPath (bcContext:BioContainer.BcContext) (additionalParameters:STARParams list) = 
            let presetOpts =
                [
                    STARParams.RunParameters [
                        RunParams.Threads threads
                        RunParams.Mode RunMode.GenomeGenerate
                    ]
                    STARParams.GenomeParameters [
                        GenomeParams.GenomeDirectory  directory
                        GenomeParams.GenomeFastaFiles genomesFastaPaths
                    ]
                    SpliceJunctionsDatabaseParameters [
                        SpliceJunctionsDatabaseParams.GTFfile gtfPath
                    ]
                ]
            runSTARAsync bcContext (presetOpts@additionalParameters)

        let runBasicGenomeIndexing threads directory genomesFastaPaths gtfPath (bcContext:BioContainer.BcContext) (additionalParameters:STARParams list) =
            runBasicGenomeIndexingAsync threads directory genomesFastaPaths gtfPath bcContext additionalParameters
            |> Async.RunSynchronously

