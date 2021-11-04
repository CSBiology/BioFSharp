namespace BioFSharp.IO

open System
open BioFSharp
open System.IO

//TO-DO refactor into core
module internal AminoAcids =

    open AminoAcids

    type AminoAcid with
        static member internal ofThreeLetterCode (letters: string) = 
            match letters with
            | "Ala" | "ALA" -> Ala
            | "Cys" | "CYS" -> Cys
            | "Asp" | "ASP" -> Asp
            | "Glu" | "GLU" -> Glu
            | "Phe" | "PHE" -> Phe
            | "Gly" | "GLY" -> Gly
            | "His" | "HIS" -> His
            | "Ile" | "ILE" -> Ile
            | "Lys" | "LYS" -> Lys
            | "Leu" | "LEU" -> Leu
            | "Met" | "MET" -> Met
            | "Asn" | "ASN" -> Asn
            | "Pyl" | "PYL" -> Pyl
            | "Pro" | "PRO" -> Pro
            | "Gln" | "GLN" -> Gln
            | "Arg" | "ARG" -> Arg
            | "Ser" | "SER" -> Ser
            | "Thr" | "THR" -> Thr
            | "Sec" | "SEC" -> Sec
            | "Val" | "VAL" -> Val
            | "Trp" | "TRP" -> Trp
            | "Tyr" | "TYR" -> Tyr
            | "Xaa" | "XAA" -> Xaa
            | "Xle" | "XLE" -> Xle
            | "Glx" | "GLX" -> Glx
            | "Asx" | "ASX" -> Asx
            | "Gap" | "GAP"| "-" -> Gap
            | "Ter" | "TER"| "*" -> Ter
            | _ -> failwith $"{letters} is not a valid three letter code for an amino acid."

//TO-DO refactor into FSharpAux
module internal Seq =
    let chunkBy (projection: 'T -> 'Key) (source: _ seq) = 
        seq {
            use e = source.GetEnumerator ()
            if e.MoveNext () then
                let mutable g = projection e.Current
                let mutable members = ResizeArray ()
                members.Add e.Current
                while e.MoveNext () do
                    let key = projection e.Current
                    if g = key then 
                        members.Add e.Current
                    else
                        yield g, members |> Seq.cast<'T>
                        g <- key
                        members <- ResizeArray ()
                        members.Add e.Current
                yield g, members |> Seq.cast<'T>
        }

open AminoAcids

[<RequireQualifiedAccess>]
type StructureFormat =
    | DSSP
    | Stride

///compromise summary of secondary structure, intended to approximate crystallographers' intuition, based on columns 19-38, which are the principal result of DSSP analysis of the atomic coordinates.
type SecondaryStructure =
    /// α-helix
    | AlphaHelix        of StructureInfo: string * Format: StructureFormat
    /// residue in isolated β-bridge
    | IsolatedBetaBridge of StructureInfo: string * Format: StructureFormat
    /// extended strand, participates in β ladder
    | BetaSheet         of StructureInfo: string * Format: StructureFormat
    /// 3-helix (310 helix)
    | Helix310          of StructureInfo: string * Format: StructureFormat
    /// 5 helix (π-helix)
    | PiHelix           of StructureInfo: string * Format: StructureFormat
    /// hydrogen bonded turn
    | Turn              of StructureInfo: string * Format: StructureFormat
    /// bend
    | Bend              of StructureInfo: string * Format: StructureFormat
    ///
    | NoStructure       of StructureInfo: string * Format: StructureFormat

    static member toString (s: SecondaryStructure) =
        match s with
        | AlphaHelix         _ -> "H"
        | IsolatedBetaBridge _ -> "B"
        | BetaSheet          _ -> "E"
        | Helix310           _ -> "G"
        | PiHelix            _ -> "I"
        | Turn               _ -> "T"
        | Bend               _ -> "S"
        | NoStructure       (_, StructureFormat.DSSP) -> " "
        | NoStructure       (_, StructureFormat.Stride) -> "C"

    static member isHelical (s: SecondaryStructure) =
        match s with
        | AlphaHelix _ | Helix310 _ | PiHelix _ -> true
        | _ -> false

    static member isSheet (s: SecondaryStructure) =
        match s with
        | IsolatedBetaBridge _ | BetaSheet _ -> true
        | _ -> false

    static member isNoStructure (s: SecondaryStructure) =
        match s with
        | NoStructure _ -> true
        | _ -> false

    static member ofString (structureFormat:StructureFormat) (str:string) =
        let identifier = str.[0]
        
        match (structureFormat, identifier) with 
        | _,'H'       -> AlphaHelix           (str.Trim(), structureFormat)
        | StructureFormat.DSSP,'B' | StructureFormat.Stride,'b' | StructureFormat.Stride,'B' -> IsolatedBetaBridge   (str.Trim(), structureFormat)
        | _,'E'       -> BetaSheet            (str.Trim(), structureFormat)
        | _,'G'       -> Helix310             (str.Trim(), structureFormat)
        | _,'I'       -> PiHelix              (str.Trim(), structureFormat)
        | _,'T'       -> Turn                 (str.Trim(), structureFormat)
        | _,'S'       -> Bend                 (str.Trim(), structureFormat)
        | StructureFormat.DSSP,' ' | StructureFormat.Stride,'C' -> NoStructure          (str.Trim(), structureFormat)
        | _ -> failwith $"{str} does not start with a valid DSSP secondary structure code. Valid codes: H, B, E, G, I, T, S, ' ', C but got '%c{str.[0]}'"

module Stride =

    type StrideLine = {
        ResidueIndex            : int
        OrdinalResidueIndex     : int
        ResidueName             : string
        ChainId                 : string
        AminoAcid               : AminoAcids.AminoAcid
        SecondaryStructure      : SecondaryStructure
        AccessibleSurface       : float
        PHI                     : float
        PSI                     : float 
    } with
        static member create residueindex ordinalresidueindex residuename chainid aminoacid secondarystructure accessiblesurface phi psi =
            {
                ResidueIndex        = residueindex      
                OrdinalResidueIndex = ordinalresidueindex      
                ResidueName         = residuename       
                ChainId             = chainid           
                AminoAcid           = aminoacid         
                SecondaryStructure  = secondarystructure
                AccessibleSurface   = accessiblesurface 
                PHI                 = phi               
                PSI                 = psi               
            }
    
        static member ofParseResults residueindex ordinalresidueindex residuename chainid aminoacid secondarystructure accessiblesurface phi psi =
            
            let handleResult (columnName: string) (result:Result<'A,'B*exn>)=
                match result with 
                | Ok value -> value
                | Error (value,e) -> failwith $"""parser error for {columnName} column value: "{value}";{System.Environment.NewLine} {e.Message}"""
    
            StrideLine.create 
                (residueindex        |> handleResult "residueindex")
                (ordinalresidueindex |> handleResult "ordinalresidueindex")
                (residuename         |> handleResult "residuename")
                (chainid             |> handleResult "chainid")
                (aminoacid           |> handleResult "aminoacid")
                (secondarystructure  |> handleResult "secondarystructure")
                (accessiblesurface   |> handleResult "accessiblesurface")
                (phi                 |> handleResult "phi")
                (psi                 |> handleResult "psi")
    
        static member ofString (line: string) = 
    
            let tryParseColumnBy f (c: string) = (try f c |> Ok with e -> Error (c,e))
    
            let residueindex                    = line.[11..14]     |> tryParseColumnBy (fun c -> c.Trim() |> int )
            let ordinalresidueindex             = line.[16..19]     |> tryParseColumnBy (fun c -> c.Trim() |> int )
            let residuename                     = line.[5..7]       |> tryParseColumnBy (fun c -> c.Trim())
            let chainid                         = line.[10..10]     |> tryParseColumnBy (fun c -> c.Trim())
            let aminoacid                       = line.[5..7]       |> tryParseColumnBy (fun c -> c.Trim() |> AminoAcid.ofThreeLetterCode)
            let secondarystructure              = line.[24..38]     |> tryParseColumnBy (SecondaryStructure.ofString StructureFormat.Stride)
            let accessiblesurface               = line.[64..68]     |> tryParseColumnBy (fun c -> c.Trim() |> float)
            let phi                             = line.[42..48]     |> tryParseColumnBy (fun c -> c.Trim() |> float)
            let psi                             = line.[52..58]     |> tryParseColumnBy (fun c -> c.Trim() |> float)
    
            StrideLine.ofParseResults residueindex ordinalresidueindex residuename chainid aminoacid secondarystructure accessiblesurface phi psi

    let fromLines (source: seq<string>) =
        let en = source.GetEnumerator()
        let rec loop (lineIndex:int) (acc: StrideLine list) =
            if en.MoveNext() then
                match en.Current with
                | asignment when en.Current.StartsWith("ASG") -> 
                    let line =
                        try
                            asignment |> StrideLine.ofString
                        with e as exn ->
                            failwith $"parser failed at line {lineIndex}: {en.Current}{System.Environment.NewLine}{e.Message}"
                    loop (lineIndex + 1) (line::acc)
                | _ -> 
                    loop (lineIndex + 1) acc

            else acc |> List.rev
        loop 0 []
    
    let fromFile (path:string) =
        path
        |> File.ReadAllLines
        |> fromLines

    let toAASequence (stride:seq<StrideLine>) =
        stride
        |> Seq.map (fun x -> x.AminoAcid)
        |> Array.ofSeq

    let toStructureSequence (stride:seq<StrideLine>) =
        stride
        |> Seq.map (fun stride -> stride.SecondaryStructure)
        |> Array.ofSeq    

    let toSequenceFeatures (stride:seq<StrideLine>) =
        stride
        |> Seq.map (fun x -> x.SecondaryStructure)
        |> Seq.indexed
        |> Seq.chunkBy (snd >> SecondaryStructure.toString)
        |> Seq.map (fun (structure,stretch) ->
            SequenceFeature.create(
                "Stride Structure",
                (stretch |> Seq.minBy fst |> fst),
                (stretch |> Seq.maxBy fst |> fst),
                (char structure)
            )
        )

    let prettyPrint (stride:seq<StrideLine>) =
        stride
        |> fun x -> 
            let seq = 
                x
                |> Seq.map (fun x -> x.AminoAcid)

            
            let features = ["DSSP Structure", toSequenceFeatures x |> List.ofSeq] |> Map.ofList
        
            AnnotatedSequence.create "stride" seq features
        
        |> AnnotatedSequence.format

        |> fun s -> $"""{System.Environment.NewLine}
DSSP (Define Secondary Structure of Proteins) result summary.

Seconary structure keys:
- H = α-helix
- B = residue in isolated β-bridge
- E = extended strand, participates in β ladder
- G = 3-helix (310 helix)
- I = 5 helix (π-helix)
- T = hydrogen bonded turn
- S = bend

{s}{System.Environment.NewLine}"""

module DSSP =

    type DSSPLine = {
        ResidueIndex            : int
        ResidueName             : string
        InsertionCode           : string
        ChainId                 : string
        AminoAcid               : string
        SecondaryStructure      : SecondaryStructure
        AccessibleSurface       : int
        NH_O_1_Relidx           : int
        NH_O_1_Energy           : float
        O_NH_1_Relidx           : int
        O_NH_1_Energy           : float
        NH_O_2_Relidx           : int
        NH_O_2_Energy           : float
        O_NH_2_Relidx           : int
        O_NH_2_Energy           : float
        TCO                     : float
        KAPPA                   : float
        ALPHA                   : float 
        PHI                     : float
        PSI                     : float 
        X_CA                    : float
        Y_CA                    : float
        Z_CA                    : float
        Chain                   : string
    
    } with
        static member create residueindex residuename insertioncode chainid aminoacid secondarystructure accessiblesurface nh_o_1_relidx nh_o_1_energy o_nh_1_relidx o_nh_1_energy nh_o_2_relidx nh_o_2_energy o_nh_2_relidx o_nh_2_energy tco kappa alpha phi psi x_ca y_ca z_ca chain =
            {
                ResidueIndex        = residueindex      
                ResidueName         = residuename       
                InsertionCode       = insertioncode     
                ChainId             = chainid           
                AminoAcid           = aminoacid         
                SecondaryStructure  = secondarystructure
                AccessibleSurface   = accessiblesurface 
                NH_O_1_Relidx       = nh_o_1_relidx     
                NH_O_1_Energy       = nh_o_1_energy     
                O_NH_1_Relidx       = o_nh_1_relidx     
                O_NH_1_Energy       = o_nh_1_energy     
                NH_O_2_Relidx       = nh_o_2_relidx     
                NH_O_2_Energy       = nh_o_2_energy     
                O_NH_2_Relidx       = o_nh_2_relidx     
                O_NH_2_Energy       = o_nh_2_energy     
                TCO                 = tco               
                KAPPA               = kappa             
                ALPHA               = alpha             
                PHI                 = phi               
                PSI                 = psi               
                X_CA                = x_ca              
                Y_CA                = y_ca              
                Z_CA                = z_ca              
                Chain               = chain             
            }
    
        static member ofParseResults residueindex residuename insertioncode chainid aminoacid secondarystructure accessiblesurface nh_o_1_relidx nh_o_1_energy o_nh_1_relidx o_nh_1_energy nh_o_2_relidx nh_o_2_energy o_nh_2_relidx o_nh_2_energy tco kappa alpha phi psi x_ca y_ca z_ca chain =
            
            let handleResult (columnName: string) (result:Result<'A,'B*exn>)=
                match result with 
                | Ok value -> value
                | Error (value,e) -> failwith $"""parser error for {columnName} column value: "{value}";{System.Environment.NewLine} {e.Message}"""
    
            DSSPLine.create 
                (residueindex        |> handleResult "residueindex")
                (residuename         |> handleResult "residuename")
                (insertioncode       |> handleResult "insertioncode")
                (chainid             |> handleResult "chainid")
                (aminoacid           |> handleResult "aminoacid")
                (secondarystructure  |> handleResult "secondarystructure")
                (accessiblesurface   |> handleResult "accessiblesurface")
                (nh_o_1_relidx       |> handleResult "nh_o_1_relidx")
                (nh_o_1_energy       |> handleResult "nh_o_1_energy")
                (o_nh_1_relidx       |> handleResult "o_nh_1_relidx")
                (o_nh_1_energy       |> handleResult "o_nh_1_energy")
                (nh_o_2_relidx       |> handleResult "nh_o_2_relidx")
                (nh_o_2_energy       |> handleResult "nh_o_2_energy")
                (o_nh_2_relidx       |> handleResult "o_nh_2_relidx")
                (o_nh_2_energy       |> handleResult "o_nh_2_energy")
                (tco                 |> handleResult "tco")
                (kappa               |> handleResult "kappa")
                (alpha               |> handleResult "alpha")
                (phi                 |> handleResult "phi")
                (psi                 |> handleResult "psi")
                (x_ca                |> handleResult "x_ca")
                (y_ca                |> handleResult "y_ca")
                (z_ca                |> handleResult "z_ca")
                (chain               |> handleResult "chain")
    
    
        static member ofString (line: string) = 
    
            let tryParseColumnBy f (c: string) = (try f c |> Ok with e -> Error (c,e))
            let tryParseTupleColumnBy (f: 'c -> 'a *'b) (c: string) = (try f c |> fun (a,b) -> Ok a, Ok b with e -> Error (c,e), Error (c,e))
    
            let residueindex                    = line.[0..4]        |> tryParseColumnBy (fun c -> c.Trim() |> int )
            let residuename                     = line.[5..9]        |> tryParseColumnBy (fun c -> c.Trim())
            let insertioncode                   = line.[10..10]      |> tryParseColumnBy (fun c -> c.Trim())
            let chainid                         = line.[11..11]      |> tryParseColumnBy (fun c -> c.Trim())
            let aminoacid                       = line.[13..13]      |> tryParseColumnBy (fun c -> c.Trim())
            let secondarystructure              = line.[16..33]      |> tryParseColumnBy (SecondaryStructure.ofString StructureFormat.DSSP)
            let accessiblesurface               = line.[34..37]      |> tryParseColumnBy (fun c -> c.Trim() |> int)
            let nh_o_1_relidx, nh_o_1_energy    = line.[38..49]      |> tryParseTupleColumnBy (fun c -> c.Trim().Split(',') |> fun [|idx;energy|] -> int (idx.Trim()), float (energy.Trim()))
            let o_nh_1_relidx, o_nh_1_energy    = line.[50..60]      |> tryParseTupleColumnBy (fun c -> c.Trim().Split(',') |> fun [|idx;energy|] -> int (idx.Trim()), float (energy.Trim()))
            let nh_o_2_relidx, nh_o_2_energy    = line.[61..71]      |> tryParseTupleColumnBy (fun c -> c.Trim().Split(',') |> fun [|idx;energy|] -> int (idx.Trim()), float (energy.Trim()))
            let o_nh_2_relidx, o_nh_2_energy    = line.[72..82]      |> tryParseTupleColumnBy (fun c -> c.Trim().Split(',') |> fun [|idx;energy|] -> int (idx.Trim()), float (energy.Trim()))
            let tco                             = line.[83..90]      |> tryParseColumnBy (fun c -> c.Trim() |> float)
            let kappa                           = line.[91..96]      |> tryParseColumnBy (fun c -> c.Trim() |> float)
            let alpha                           = line.[97..102]     |> tryParseColumnBy (fun c -> c.Trim() |> float)
            let phi                             = line.[103..108]    |> tryParseColumnBy (fun c -> c.Trim() |> float)
            let psi                             = line.[109..114]    |> tryParseColumnBy (fun c -> c.Trim() |> float)
            let x_ca                            = line.[115..121]    |> tryParseColumnBy (fun c -> c.Trim() |> float)
            let y_ca                            = line.[122..128]    |> tryParseColumnBy (fun c -> c.Trim() |> float)
            let z_ca                            = line.[129..135]    |> tryParseColumnBy (fun c -> c.Trim() |> float)
            let chain                           = line.[136..]       |> tryParseColumnBy (fun c -> c.Trim())
    
            DSSPLine.ofParseResults residueindex residuename insertioncode chainid aminoacid secondarystructure accessiblesurface nh_o_1_relidx nh_o_1_energy o_nh_1_relidx o_nh_1_energy nh_o_2_relidx nh_o_2_energy o_nh_2_relidx o_nh_2_energy tco kappa alpha phi psi x_ca y_ca z_ca chain

    let fromLines (source: seq<string>) =
        let en = source.GetEnumerator()
        let rec loop (yieldLine:bool) (lineIndex:int) (acc: DSSPLine list) =
            if en.MoveNext() then
                match en.Current with
                | tableStartLine when en.Current.StartsWith("  #  RESIDUE") -> 
                    loop true (lineIndex + 1) acc
                | dsspLine when yieldLine -> 
                    let line =
                        try
                            dsspLine |> DSSPLine.ofString
                        with e as exn ->
                            failwith $"parser failed at line {lineIndex}: {en.Current}{System.Environment.NewLine}{e.Message}"
                    loop yieldLine (lineIndex + 1) (line::acc)
                | _ ->  
                    loop false (lineIndex + 1) acc
            else acc |> List.rev
        loop false 0 []
    
    let fromFile (path:string) =
        path
        |> File.ReadAllLines
        |> fromLines

    let toAASequence (dssp:seq<DSSPLine>) =
        dssp
        |> Seq.choose (fun x -> x.AminoAcid |> char |> BioItemsConverter.OptionConverter.charToOptionAminoAcid)
        |> Array.ofSeq

    let toStructureSequence (dssp:seq<DSSPLine>) =
        dssp
        |> Seq.map (fun dssp -> dssp.SecondaryStructure)
        |> Array.ofSeq    

    
    let toSequenceFeatures (dssp:seq<DSSPLine>) =
        dssp
        |> Seq.map (fun x -> x.SecondaryStructure)
        |> Seq.indexed
        |> Seq.chunkBy (snd >> SecondaryStructure.toString)
        |> Seq.map (fun (structure,stretch) ->
            SequenceFeature.create(
                "DSSP Structure",
                (stretch |> Seq.minBy fst |> fst),
                (stretch |> Seq.maxBy fst |> fst),
                (char structure)
            )
        )

    let prettyPrint (dssp:seq<DSSPLine>) =
        dssp
        |> fun x -> 
            let seq = 
                x
                |> Seq.map (fun x -> x.AminoAcid)
                |> String.concat ""
                |> BioArray.ofAminoAcidString
            
            let features = ["DSSP Structure", toSequenceFeatures x |> List.ofSeq] |> Map.ofList
        
            AnnotatedSequence.create "dssp" seq features
        
        |> AnnotatedSequence.format

        |> fun s -> $"""{System.Environment.NewLine}
DSSP (Define Secondary Structure of Proteins) result summary.

Seconary structure keys:
- H = α-helix
- B = residue in isolated β-bridge
- E = extended strand, participates in β ladder
- G = 3-helix (310 helix)
- I = 5 helix (π-helix)
- T = hydrogen bonded turn
- S = bend

{s}{System.Environment.NewLine}"""