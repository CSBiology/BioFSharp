#load "CNTKLoadscript.fsx"
open CNTKLoadscript

CNTKLoadscript.resolveCNTKDependencies ()

#r @"..\..\packages\FSharpAux\lib\netstandard2.0\FSharpAux.dll"
#r @"..\..\packages\FSharpAux.IO\lib\netstandard2.0\FSharpAux.IO.dll"
#r @"..\..\packages\FSharp.Stats\lib\netstandard2.0\FSharp.Stats.dll"
#I @"..\..\bin\BioFSharp.ML\netstandard2.0"
#r @"BioFSharp.dll"
#r "BioFSharp.IO"
//#r "BioFSharp.ML
#load "DPPOP.fs"

open BioFSharp.ML.DPPOP
open BioFSharp.IO
open BioFSharp
open BioFSharp.IO.FastA
open FSharpAux


type Qid = {
    Id        : int
    Rank      : int
    Data      : float[]
    ProtId    : string
    Intensity : float
    Sequence  : string
}

let getQid str =
    let m = System.Text.RegularExpressions.Regex.Match(str,@"([\d]*) qid:([\d]*) ([\d]*:([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?) )*#p:(\S*) i:(-?[\d]*.[\d]*) s:(\S*)")
    if m.Success then
        let arr = [|for c in m.Groups.[4].Captures -> float c.Value|]
        {Id=int (m.Groups.[2].Value);Rank=int (m.Groups.[1].Value);Data=arr;ProtId=m.Groups.[6].Value;Intensity=float (m.Groups.[7].Value);Sequence=m.Groups.[8].Value}
    else
        {Id=(-1);Rank=(-1);Data=[||];ProtId="";Intensity=nan;Sequence=""}

let test = getQid "1 qid:0 1:0.000000 2:0.000000 3:0.000000 4:0.000000 5:0.333333 6:0.000000 7:0.000000 8:0.000000 9:0.111111 10:0.000000 11:0.111111 12:0.111111 13:0.000000 14:0.000000 15:0.000000 16:0.222222 17:0.000000 18:0.000000 19:0.111111 20:0.000000 21:9.000000 22:1175.363298 23:1.000000 24:1.000000 25:0.000000 26:-0.359141 27:0.444444 28:0.444444 29:0.000000 30:17.992222 31:3.656667 32:-0.044444 33:0.071111 34:0.946667 35:6.023333 36:3.837778 37:0.074444 38:-0.400000 39:0.929000 40:1.043333 41:4.500000 42:1.244444 43:-0.185153 44:0.000000 45:-0.083359 #p:ATCG00500.1 pacid=19637947 transcript=ATCG00500.1 locus=ATCG00500 ID=ATCG00500.1.TAIR10 annot-version=TAIR10 i:1.000000 s:SWFNFMFSK"

let testFeats = 
    "1:0.000000 2:0.000000 3:0.000000 4:0.000000 5:0.333333 6:0.000000 7:0.000000 8:0.000000 9:0.111111 10:0.000000 11:0.111111 12:0.111111 13:0.000000 14:0.000000 15:0.000000 16:0.222222 17:0.000000 18:0.000000 19:0.111111 20:0.000000 21:9.000000 22:1175.363298 23:1.000000 24:1.000000 25:0.000000 26:-0.359141 27:0.444444 28:0.444444 29:0.000000 30:17.992222 31:3.656667 32:-0.044444 33:0.071111 34:0.946667 35:6.023333 36:3.837778 37:0.074444 38:-0.400000 39:0.929000 40:1.043333 41:4.500000 42:1.244444 43:-0.185153 44:0.000000 45:-0.083359"
    |> String.split ' '
    |> Array.map (fun x -> float (x.Split(':').[1]))


let chlamyProteome = 
    FastA.fromFile BioArray.ofAminoAcidString @"C:\Users\Kevin\Desktop\Chlamy_JGI5_5.fasta"
    |> List.ofSeq

let distinctPeptides = Classification.getDistinctTrypticPeptidesFromFasta chlamyProteome

let testProt = List.find (fun (x:FastaItem<'a>) -> x.Header = @"Cre02.g120100.t1.2 Ribulose-1,5-bisphosphate carboxylase/oxygenase small subunit 1, chloroplast precursor ALS=RBCS1 DBV=JGI5.5 GN=RBCS1 OS=Chlamydomonas reinhardtii SV=2 TOU=Cre") chlamyProteome

//let efficMap = Classification.createDigestionEfficiencyMapFromFasta chlamyProteome


let protId = testProt.Header
let gigestionEfficiencyMap = Classification.createDigestionEfficiencyMapFromFasta (seq { yield testProt})
let digested =
    Classification.digestTryptic testProt.Sequence
    |> Seq.map (fun x -> BioArray.toString x)
    |> List.ofSeq

let candidatePeptides = 
    digested
    |> Seq.filter (fun p -> distinctPeptides.Contains(p))
    |> Seq.map (fun p -> Classification.getPeptideFeatures gigestionEfficiencyMap protId (BioArray.ofAminoAcidSymbolString p))
    |> Array.ofSeq
    |> Array.choose id

Prediction.scoreBy (Prediction.Model.Custom @"C:\Users\Kevin\source\repos\CSBiology\BioFSharp\src\BioFSharp.ML\Resources\Chlamy5Times128.model") candidatePeptides
|> Array.map (fun x -> x.Sequence,abs x.PredictionScore)
|> (fun x ->    let max = 
                    Array.maxBy (fun (_,y )-> y) x
                    |> snd
                x |> Array.map (fun (p,s) -> p,(s/max))
        )
|> Array.sortByDescending (fun (_,s) -> s)

open FSharpAux.IO
open FSharp.Stats

type Item = {
    Query: int*string
    Rank : int
    Features:float array
    }

let getData path = 
    FileIO.readFile  path 
    |> Seq.map (fun line -> 
                    let comment = line |> String.split '#'
                    comment.[0].Trim()
                    |> String.split ' ' 
                    |> Array.mapi (fun i x -> 
                        if i = 0 then x else x.Split ':' |> Array.item 1 )
                    |> fun processedLine -> 
                        let rank = processedLine.[0] |> int
                        let query = processedLine.[1] |> int 
                        let feats = 
                            processedLine.[2..processedLine.Length-1] 
                            |> Array.map float
                        rank,(query,comment.[1]),feats
                )
    |> Array.ofSeq
    |> Array.unzip3 
    |> (fun (rank,query,feats) ->
            let normFeats = 
                feats 
                |> JaggedArray.transpose
                /// Hier kommt dann ein Vektor mit länge 45 == anzahl der Features raus. 
                |> Array.map (fun x -> 
                                /// Diesen Mean speichern
                                let mean = FSharp.Stats.Seq.mean x
                                /// Diese Varianz speichern
                                let var = FSharp.Stats.Seq.stDev x 
                                /// Hier erfolgt die Normierung, heißt du musst in deinem Vektor n tuple (mean,varianz) speichern um neue daten zu normieren
                                var,mean
                                )
            normFeats
            )

let normChlamy = getData @"C:\Users\Kevin\Documents\Downloads\unique_allproteins_chlamy_withoutMiss_withInSilico_12032018.dat"
let normYeast = getData @"C:\Users\Kevin\Documents\Downloads\unique_allproteins_yeast_withoutMiss_withInSilico_12032018.dat"

let testP:PredictionInput = {
    ProtId = ""
    Data = testFeats
    Sequence = "SWFNFMFSK"
}

let testProtein = 
    FastA.fromFileEnumerator BioArray.ofAminoAcidString

        [">Test;";
        "MEKSWFNFMFSKGELEYRGELSKAMDSFAPGEKTTISQDRFIYDMDKNFY";
        "GWDERSSYSSSYSNNVDLLVSSKDIRNFISDDTFFVRDSNKNSYSIFFDK";
        "KKKIFEIDNDFSDLEKFFYSYCSSSYLNNRSKGDNDLHYDPYIKDTKYNC";
        "TNHINSCIDSYFRSYICIDNNFLIDSNNFNESYIYNFICSESGKIRESKN";
        "YKIRTNRNRSNLISSKDFDITQNYNQLWIQCDNCYGLMYKKVKMNVCEQC";
        "GHYLKMSSSERIELLIDPGTWNPMDEDMVSADPIKFHSKEEPYKNRIDSA";
        "QKTTGLTDAVQTGTGQLNGIPVALGVMDFRFMGGSMGSVVGEKITRLIEY";
        "ATNQCLPLILVCSSGGARMQEGSLSLMQMAKISSVLCDYQSSKKLFYISI";
        "LTSPTTGGVTASFGMLGDIIIAEPYAYIAFAGKRVIEQTLKKAVPEGSQA";
        "AESLLRKGLLDAIVPRNLLKGVLSELFQLHAFFPLNTN"]

    |> Seq.item 0

let gigestionEfficiencyMap2 = Classification.createDigestionEfficiencyMapFromFasta (seq { yield testProtein})

let digested2 =
    Classification.digestTryptic testProtein.Sequence
    |> Seq.map (fun x -> BioArray.toString x)
    |> List.ofSeq

let candidatePeptides2 = 
    digested2
    |> Seq.map (fun p -> Classification.getPeptideFeatures gigestionEfficiencyMap2 "Test;" (BioArray.ofAminoAcidSymbolString p))
    |> Array.ofSeq
    |> Array.choose id

candidatePeptides
|> Array.map (fun x -> {x with Data= (Array.map2 (fun d (stDev,mean) -> if nan.Equals((d-mean)/stDev) then 0. else (d-mean)/stDev) ) x.Data normChlamy })
|> Prediction.scoreBy (Prediction.Model.Custom @"C:\Users\Kevin\source\repos\CSBiology\BioFSharp\src\BioFSharp.ML\Resources\Chlamy5Times128.model")
|> Array.map (fun x -> x.Sequence,x.PredictionScore)
|> Array.sortByDescending (fun (_,s) -> s)
|> Array.mapi (fun i (p,x) -> printfn "%20s\t%.4f" p x) 

Prediction.scoreBy (Prediction.Model.Custom @"C:\Users\Kevin\source\repos\CSBiology\BioFSharp\src\BioFSharp.ML\Resources\Chlamy5Times128.model") [|testP|]