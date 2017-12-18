(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.IO.dll"
#r "../../bin/FSharp.Care.dll"
#r "../../bin/FSharp.Care.IO.dll"

(**
<table class="HeadAPI">
<td class="Head"><h1>GenBank format</h1></td>
<td class="API">
    <a id="APILink" href="https://csbiology.github.io/BioFSharp/reference/biofsharp-io-genbank.html" >&#128194;View module documentation</a>
</td>
</table>
GenBank is the NIH genetic sequence database, an annotated collection of all publicly available DNA sequences
([Nucleic Acids Research, 2013 Jan;41(D1):D36-42](https://www.ncbi.nlm.nih.gov/pubmed/23193287)). 
A GenBank file contains various information about a sequence and its features.

The file structure of a GenBank (.gb) flat file can be split into 4 subsections:
<br>
<div id="responsiveTable">

| Section    | Description                                                                                                            | 
| ---------- | ---------------------------------------------------------------------------------------------------------------------- | 
| Meta       | Contains meta information about the annotated sequence, the file itself and the organism the sequence is found in      | 
| References | A collection of publications about the annotated sequence(or a subsequence of it) and information associated with them | 
| Features   | A collection of features and their position within the sequence                                                        | 
| Origin     | The annotated sequence itself                                                                                          | 

</div>
<br>
With the origins section being optional and the features section usually being by far the largest amongst them. 
For more information about the sections and their formatting, have a look at this 
[annotated sample record](https://www.ncbi.nlm.nih.gov/genbank/samplerecord/) over at NCBI. 

This file will also be used for the purpose of this tutorial and in plain text looks like this:

<br>
<button type="button" class="btn" data-toggle="collapse" data-target="#gbFileExample">Show/Hide example file</button>
<div id="gbFileExample" class="collapse fileExamples ">
    <pre>
        LOCUS       SCU49845                5028 bp    DNA     linear   PLN 14-JUL-2016 
        DEFINITION  Saccharomyces cerevisiae TCP1-beta gene, partial cds; and Axl2p     
                    (AXL2) and Rev7p (REV7) genes, complete cds.                        
        ACCESSION   U49845                                                              
        VERSION     U49845.1                                                            
        KEYWORDS    .                                                                   
        SOURCE      Saccharomyces cerevisiae (baker's yeast)                            
          ORGANISM  Saccharomyces cerevisiae                                            
                    Eukaryota; Fungi; Dikarya; Ascomycota; Saccharomycotina;            
                    Saccharomycetes; Saccharomycetales; Saccharomycetaceae;             
                    Saccharomyces.                                                      
        REFERENCE   1  (bases 1 to 5028)                                                
          AUTHORS   Roemer,T., Madden,K., Chang,J. and Snyder,M.                        
          TITLE     Selection of axial growth sites in yeast requires Axl2p, a novel    
                    plasma membrane glycoprotein                                        
          JOURNAL   Genes Dev. 10 (7), 777-793 (1996)                                   
           PUBMED   8846915                                                             
        REFERENCE   2  (bases 1 to 5028)                                                
          AUTHORS   Roemer,T.                                                           
          TITLE     Direct Submission                                                   
          JOURNAL   Submitted (22-FEB-1996) Biology, Yale University, New Haven, CT     
                    06520, USA                                                          
        FEATURES             Location/Qualifiers                                        
             source          1..5028                                                    
                             /organism="Saccharomyces cerevisiae"                       
                             /mol_type="genomic DNA"                                    
                             /db_xref="taxon:4932"                                      
                             /chromosome="IX"                                           
             mRNA            <1..>206                                                   
                             /product="TCP1-beta"                                       
             CDS             <1..206                                                    
                             /codon_start=3                                             
                             /product="TCP1-beta"                                       
                             /protein_id="AAA98665.1"                                   
                             /translation="SSIYNGISTSGLDLNNGTIADMRQLGIVESYKLKRAVVSSASEA 
                             AEVLLRVDNIIRARPRTANRQHM"                                   
             gene            <687..>3158                                                
                             /gene="AXL2"                                               
             mRNA            <687..>3158                                                
                             /gene="AXL2"                                               
                             /product="Axl2p"                                           
             CDS             687..3158                                                  
                             /gene="AXL2"                                               
                             /note="plasma membrane glycoprotein"                       
                             /codon_start=1                                             
                             /product="Axl2p"                                           
                             /protein_id="AAA98666.1"                                   
                             /translation="MTQLQISLLLTATISLLHLVVATPYEAYPIGKQYPPVARVNESF 
                             TFQISNDTYKSSVDKTAQITYNCFDLPSWLSFDSSSRTFSGEPSSDLLSDANTTLYFN 
                             VILEGTDSADSTSLNNTYQFVVTNRPSISLSSDFNLLALLKNYGYTNGKNALKLDPNE 
                             VFNVTFDRSMFTNEESIVSYYGRSQLYNAPLPNWLFFDSGELKFTGTAPVINSAIAPE 
                             TSYSFVIIATDIEGFSAVEVEFELVIGAHQLTTSIQNSLIINVTDTGNVSYDLPLNYV 
                             YLDDDPISSDKLGSINLLDAPDWVALDNATISGSVPDELLGKNSNPANFSVSIYDTYG 
                             DVIYFNFEVVSTTDLFAISSLPNINATRGEWFSYYFLPSQFTDYVNTNVSLEFTNSSQ 
                             DHDWVKFQSSNLTLAGEVPKNFDKLSLGLKANQGSQSQELYFNIIGMDSKITHSNHSA 
                             NATSTRSSHHSTSTSSYTSSTYTAKISSTSAAATSSAPAALPAANKTSSHNKKAVAIA 
                             CGVAIPLGVILVALICFLIFWRRRRENPDDENLPHAISGPDLNNPANKPNQENATPLN 
                             NPFDDDASSYDDTSIARRLAALNTLKLDNHSATESDISSVDEKRDSLSGMNTYNDQFQ 
                             SQSKEELLAKPPVQPPESPFFDPQNRSSSVYMDSEPAVNKSWRYTGNLSPVSDIVRDS 
                             YGSQKTVDTEKLFDLEAPEKEKRTSRDVTMSSLDPWNSNISPSPVRKSVTPSPYNVTK 
                             HRNRHLQNIQDSQSGKNGITPTTMSTSSSDDFVPVKDGENFCWVHSMEPDRRPSKKRL 
                             VDFSNKSNVNVGQVKDIHGRIPEML"                                 
             gene            complement(<3300..>4037)                                   
                             /gene="REV7"                                               
             mRNA            complement(<3300..>4037)                                   
                             /gene="REV7"                                               
                             /product="Rev7p"                                           
             CDS             complement(3300..4037)                                     
                             /gene="REV7"                                               
                             /codon_start=1                                             
                             /product="Rev7p"                                           
                             /protein_id="AAA98667.1"                                   
                             /translation="MNRWVEKWLRVYLKCYINLILFYRNVYPPQSFDYTTYQSFNLPQ 
                             FVPINRHPALIDYIEELILDVLSKLTHVYRFSICIINKKNDLCIEKYVLDFSELQHVD 
                             KDDQIITETEVFDEFRSSLNSLIMHLEKLPKVNDDTITFEAVINAIELELGHKLDRNR 
                             RVDSLEEKAEIERDSNWVKCQEDENLPDNNGFQPPKIKLTSLVGSDVGPLIIHQFSEK 
                             LISGDDKILNGVYSQYEEGESIFGSLF"                               
        ORIGIN                                                                          
                1 gatcctccat atacaacggt atctccacct caggtttaga tctcaacaac ggaaccattg     
               61 ccgacatgag acagttaggt atcgtcgaga gttacaagct aaaacgagca gtagtcagct     
              121 ctgcatctga agccgctgaa gttctactaa gggtggataa catcatccgt gcaagaccaa     
              181 gaaccgccaa tagacaacat atgtaacata tttaggatat acctcgaaaa taataaaccg     
              241 ccacactgtc attattataa ttagaaacag aacgcaaaaa ttatccacta tataattcaa     
              301 agacgcgaaa aaaaaagaac aacgcgtcat agaacttttg gcaattcgcg tcacaaataa     
              361 attttggcaa cttatgtttc ctcttcgagc agtactcgag ccctgtctca agaatgtaat     
              421 aatacccatc gtaggtatgg ttaaagatag catctccaca acctcaaagc tccttgccga     
              481 gagtcgccct cctttgtcga gtaattttca cttttcatat gagaacttat tttcttattc     
              541 tttactctca catcctgtag tgattgacac tgcaacagcc accatcacta gaagaacaga     
              601 acaattactt aatagaaaaa ttatatcttc ctcgaaacga tttcctgctt ccaacatcta     
              661 cgtatatcaa gaagcattca cttaccatga cacagcttca gatttcatta ttgctgacag     
              721 ctactatatc actactccat ctagtagtgg ccacgcccta tgaggcatat cctatcggaa     
              781 aacaataccc cccagtggca agagtcaatg aatcgtttac atttcaaatt tccaatgata     
              841 cctataaatc gtctgtagac aagacagctc aaataacata caattgcttc gacttaccga     
              901 gctggctttc gtttgactct agttctagaa cgttctcagg tgaaccttct tctgacttac     
              961 tatctgatgc gaacaccacg ttgtatttca atgtaatact cgagggtacg gactctgccg     
             1021 acagcacgtc tttgaacaat acataccaat ttgttgttac aaaccgtcca tccatctcgc     
             1081 tatcgtcaga tttcaatcta ttggcgttgt taaaaaacta tggttatact aacggcaaaa     
             1141 acgctctgaa actagatcct aatgaagtct tcaacgtgac ttttgaccgt tcaatgttca     
             1201 ctaacgaaga atccattgtg tcgtattacg gacgttctca gttgtataat gcgccgttac     
             1261 ccaattggct gttcttcgat tctggcgagt tgaagtttac tgggacggca ccggtgataa     
             1321 actcggcgat tgctccagaa acaagctaca gttttgtcat catcgctaca gacattgaag     
             1381 gattttctgc cgttgaggta gaattcgaat tagtcatcgg ggctcaccag ttaactacct     
             1441 ctattcaaaa tagtttgata atcaacgtta ctgacacagg taacgtttca tatgacttac     
             1501 ctctaaacta tgtttatctc gatgacgatc ctatttcttc tgataaattg ggttctataa     
             1561 acttattgga tgctccagac tgggtggcat tagataatgc taccatttcc gggtctgtcc     
             1621 cagatgaatt actcggtaag aactccaatc ctgccaattt ttctgtgtcc atttatgata     
             1681 cttatggtga tgtgatttat ttcaacttcg aagttgtctc cacaacggat ttgtttgcca     
             1741 ttagttctct tcccaatatt aacgctacaa ggggtgaatg gttctcctac tattttttgc     
             1801 cttctcagtt tacagactac gtgaatacaa acgtttcatt agagtttact aattcaagcc     
             1861 aagaccatga ctgggtgaaa ttccaatcat ctaatttaac attagctgga gaagtgccca     
             1921 agaatttcga caagctttca ttaggtttga aagcgaacca aggttcacaa tctcaagagc     
             1981 tatattttaa catcattggc atggattcaa agataactca ctcaaaccac agtgcgaatg     
             2041 caacgtccac aagaagttct caccactcca cctcaacaag ttcttacaca tcttctactt     
             2101 acactgcaaa aatttcttct acctccgctg ctgctacttc ttctgctcca gcagcgctgc     
             2161 cagcagccaa taaaacttca tctcacaata aaaaagcagt agcaattgcg tgcggtgttg     
             2221 ctatcccatt aggcgttatc ctagtagctc tcatttgctt cctaatattc tggagacgca     
             2281 gaagggaaaa tccagacgat gaaaacttac cgcatgctat tagtggacct gatttgaata     
             2341 atcctgcaaa taaaccaaat caagaaaacg ctacaccttt gaacaacccc tttgatgatg     
             2401 atgcttcctc gtacgatgat acttcaatag caagaagatt ggctgctttg aacactttga     
             2461 aattggataa ccactctgcc actgaatctg atatttccag cgtggatgaa aagagagatt     
             2521 ctctatcagg tatgaataca tacaatgatc agttccaatc ccaaagtaaa gaagaattat     
             2581 tagcaaaacc cccagtacag cctccagaga gcccgttctt tgacccacag aataggtctt     
             2641 cttctgtgta tatggatagt gaaccagcag taaataaatc ctggcgatat actggcaacc     
             2701 tgtcaccagt ctctgatatt gtcagagaca gttacggatc acaaaaaact gttgatacag     
             2761 aaaaactttt cgatttagaa gcaccagaga aggaaaaacg tacgtcaagg gatgtcacta     
             2821 tgtcttcact ggacccttgg aacagcaata ttagcccttc tcccgtaaga aaatcagtaa     
             2881 caccatcacc atataacgta acgaagcatc gtaaccgcca cttacaaaat attcaagact     
             2941 ctcaaagcgg taaaaacgga atcactccca caacaatgtc aacttcatct tctgacgatt     
             3001 ttgttccggt taaagatggt gaaaattttt gctgggtcca tagcatggaa ccagacagaa     
             3061 gaccaagtaa gaaaaggtta gtagattttt caaataagag taatgtcaat gttggtcaag     
             3121 ttaaggacat tcacggacgc atcccagaaa tgctgtgatt atacgcaacg atattttgct     
             3181 taattttatt ttcctgtttt attttttatt agtggtttac agatacccta tattttattt     
             3241 agtttttata cttagagaca tttaatttta attccattct tcaaatttca tttttgcact     
             3301 taaaacaaag atccaaaaat gctctcgccc tcttcatatt gagaatacac tccattcaaa     
             3361 attttgtcgt caccgctgat taatttttca ctaaactgat gaataatcaa aggccccacg     
             3421 tcagaaccga ctaaagaagt gagttttatt ttaggaggtt gaaaaccatt attgtctggt     
             3481 aaattttcat cttcttgaca tttaacccag tttgaatccc tttcaatttc tgctttttcc     
             3541 tccaaactat cgaccctcct gtttctgtcc aacttatgtc ctagttccaa ttcgatcgca     
             3601 ttaataactg cttcaaatgt tattgtgtca tcgttgactt taggtaattt ctccaaatgc     
             3661 ataatcaaac tatttaagga agatcggaat tcgtcgaaca cttcagtttc cgtaatgatc     
             3721 tgatcgtctt tatccacatg ttgtaattca ctaaaatcta aaacgtattt ttcaatgcat     
             3781 aaatcgttct ttttattaat aatgcagatg gaaaatctgt aaacgtgcgt taatttagaa     
             3841 agaacatcca gtataagttc ttctatatag tcaattaaag caggatgcct attaatggga     
             3901 acgaactgcg gcaagttgaa tgactggtaa gtagtgtagt cgaatgactg aggtgggtat     
             3961 acatttctat aaaataaaat caaattaatg tagcatttta agtataccct cagccacttc     
             4021 tctacccatc tattcataaa gctgacgcaa cgattactat tttttttttc ttcttggatc     
             4081 tcagtcgtcg caaaaacgta taccttcttt ttccgacctt ttttttagct ttctggaaaa     
             4141 gtttatatta gttaaacagg gtctagtctt agtgtgaaag ctagtggttt cgattgactg     
             4201 atattaagaa agtggaaatt aaattagtag tgtagacgta tatgcatatg tatttctcgc     
             4261 ctgtttatgt ttctacgtac ttttgattta tagcaagggg aaaagaaata catactattt     
             4321 tttggtaaag gtgaaagcat aatgtaaaag ctagaataaa atggacgaaa taaagagagg     
             4381 cttagttcat cttttttcca aaaagcaccc aatgataata actaaaatga aaaggatttg     
             4441 ccatctgtca gcaacatcag ttgtgtgagc aataataaaa tcatcacctc cgttgccttt     
             4501 agcgcgtttg tcgtttgtat cttccgtaat tttagtctta tcaatgggaa tcataaattt     
             4561 tccaatgaat tagcaatttc gtccaattct ttttgagctt cttcatattt gctttggaat     
             4621 tcttcgcact tcttttccca ttcatctctt tcttcttcca aagcaacgat ccttctaccc     
             4681 atttgctcag agttcaaatc ggcctctttc agtttatcca ttgcttcctt cagtttggct     
             4741 tcactgtctt ctagctgttg ttctagatcc tggtttttct tggtgtagtt ctcattatta     
             4801 gatctcaagt tattggagtc ttcagccaat tgctttgtat cagacaattg actctctaac     
             4861 ttctccactt cactgtcgag ttgctcgttt ttagcggaca aagatttaat ctcgttttct     
             4921 ttttcagtgt tagattgctc taattctttg agctgttctc tcagctcctc atatttttct     
             4981 tgccatgact cagattctaa ttttaagcta ttcaatttct ctttgatc                  
        //                                                                              
    </pre>
</div>
<br>

Reading GenBank files
---------------------
The type equivalent for a GenBank file in BioFSharp is a dictionary, mapping `string` keys to the `GenBankItem<'a>` type, where 'a is the type of the origin sequence in the file.
More information about the type modelling can be found in our [API reference](https://csbiology.github.io/BioFSharp/reference/biofsharp-io-genbank.html).
The default reader `fromFile` simply takes the origin sequence as a sequence of `chars`.
*)
open BioFSharp.IO
open BioFSharp

///Path of the example file
let exampleFilePath = __SOURCE_DIRECTORY__ + @"\data\sequence.gb"

///Parsed Example File 
let parsedGBFile = GenBank.Read.fromFile exampleFilePath

(**
You can also use converter functions for the origin sequence, which makes it easier to use them for other BioFSharp workflows. There are multiple prebuilt converters contained in the
OriginConverters module for reading and writing. For example, the following code will parse the sequence as a `BioSeq` containing nucleotides
*)

let converter = GenBank.OriginConverters.Input.nucleotideConverter

let parsedGBFile' = GenBank.Read.fromFileWithOriginConverter converter exampleFilePath

(**
This makes it easy to perform additional tasks with the origin sequence:
*)

let origin = GenBank.getOrigin parsedGBFile'

///Transcribed origin sequence
let rnaSeq = origin |> BioSeq.transcribeCodeingStrand // val it : seq<Nucleotides.Nucleotide> = seq [G; A; U; C; ...]

///Translated origin sequence
let protein = rnaSeq |> BioSeq.translate 0 // val it : seq<AminoAcids.AminoAcid> = seq [Asp; Pro; Pro; Tyr; ...]
  

(**
Writing GenBank files
---------------------
Just as in the Read module, there is a default writer for a GenBank dictionary that contains a sequence of chars, as a writer taking a custom converter function. Just specify the output path, and you are ready to go.
*)

let outputPath1 = __SOURCE_DIRECTORY__ + @"\data\sequenceTestWrite1.gb"

let outputPath2 = __SOURCE_DIRECTORY__ + @"\data\sequenceTestWrite2.gb"
let outputConverter = GenBank.OriginConverters.Output.bioItemConverter

parsedGBFile |> GenBank.Write.toFile outputPath1

parsedGBFile' |> GenBank.Write.toFileWithOriginConverter outputPath2 outputConverter