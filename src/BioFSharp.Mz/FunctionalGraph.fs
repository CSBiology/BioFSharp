namespace BioFSharp.Mz

open System
open BioFSharp
open BioFSharp.Mz

open FSharp.FGL
open FSharp.FGL.Directed

module FunctionalGraph =

    
    
    (*  
        1. Declaration of types and utility functions.
        ==============================================
        This segment is used to define data structures as well as useful utility functions.
    
    *)
    
    /// AA-references for MS data, including Xle instead of Leu/Ile, Glx instead of Gln/Glu and Asx instead of Asn/Asp.
    let refAminoAcids = 
        [| AminoAcids.Ala; AminoAcids.Cys; AminoAcids.Phe; AminoAcids.Gly; AminoAcids.His; AminoAcids.Lys; AminoAcids.Met; 
           AminoAcids.Pro; AminoAcids.Arg; AminoAcids.Ser; AminoAcids.Thr; AminoAcids.Val; AminoAcids.Trp; AminoAcids.Tyr; 
           AminoAcids.Asx; AminoAcids.Glx; AminoAcids.Xle |]
    
    let H20_Mass = Formula.Table.H2O |> Formula.monoisoMass

    /// Returns an array with AA-names, masses.
    let references = 
        refAminoAcids
        |> Seq.map (fun aa -> aa, Formula.monoisoMass (AminoAcids.formula aa))
        |> Seq.toArray

    /// Type used to represent peaks of MS data.
    type SpectrumGraphNode = 
        | N of uint64
        | C of uint64
        override this.ToString() = 
            match this with
            | N mass -> sprintf "N %A" mass
            | C mass -> sprintf "C %A" mass
    
    /// Checks if nodes are equal.
    let isEqualSpectrumGraphNodeType (a : SpectrumGraphNode) (b : SpectrumGraphNode) = 
        match a, b with
        | N x, N y -> true
        | C x, C y -> true
        | _ -> false
    
    /// Gets the value of a node.
    let valueOfSpectrumGraphNode (sgn : SpectrumGraphNode) = 
        match sgn with
        | N v -> v
        | C v -> v
    
    /// Converts mass as float to unsigned int64
    let convertFromMass = 
        let representationAccuracy = 10000000.
        fun mass -> mass * representationAccuracy |> uint64
    
    /// Converts unsigned int64 to float
    let convertToMass = 
        let representationAccuracy = 10000000.
        fun (u : uint64) -> float u / representationAccuracy
       
    /// Transforms spectrum peaks to nodes.
    let spectrumGraphNodeFromAPeak (p : PeakAnnotation) = 
        match p.Meta with
        | IonTypes.Main B -> SpectrumGraphNode.N(convertFromMass p.Data.Mz)
        | IonTypes.Main Y -> SpectrumGraphNode.C(convertFromMass p.Data.Mz)
        | _ -> failwithf "%A : IonType is not supported" p.Meta
    
    /// Transforms spectrum peaks to C-nodes.
    let spectrumGraphNodeFromPeak (p : Peak) = SpectrumGraphNode.C(convertFromMass p.Mz)
    
    /// Prints value of nodes as string.
    let sprintNC (sgn : SpectrumGraphNode) = 
        match sgn with
        | N v -> sprintf "N : %f" (convertToMass v)
        | C v -> sprintf "C : %f" (convertToMass v)
    
    /// Returns List without item with the index i.
    let rec deleteValue i l = 
        match i, l with
        | 0, x :: xs -> xs
        | i, x :: xs -> x :: deleteValue (i - 1) xs
        | i, [] -> failwith "index out of range"
    
    
    /// Returns the distance between each peak and each with a greater mz value.
    let calculateDistancesBetweenPeaks (rawPeakList : Peak list) = 
        let peakList = rawPeakList |> List.sort
        
        let rec inner (fromPL : Peak) (peakList : Peak list) acc = 
            match peakList with
            | h :: t -> 
                let difference = h.Mz - fromPL.Mz
                inner fromPL t (((fromPL.Mz, h.Mz), difference) :: acc)
            | [] -> acc
        
        let rec outer (peakList : Peak list) acc = 
            match peakList with
            | h :: t -> outer t (inner h t acc)
            | [] -> acc |> List.toArray
        
        outer peakList []
    
    
    /// Matches differences between mz values to an array of reference amino acids. Returns the difference and Some aa if an amino acid could be found.
    let matchedAAMasses accuracy refAaMass (calcDiff : ((float * float) * float) []) = 
        let findAAMass refAaMass queryMass = 
            let mm, pm = Mass.rangePpm accuracy queryMass
            Array.tryFind (fun (x, n) -> n <= pm && n > mm) refAaMass
        [ for i in 0..(calcDiff.Length - 1) do
              yield calcDiff.[i], (findAAMass refAaMass (snd calcDiff.[i])) ]
    
    
    
    (*
        The creation of the SpectrumGraphNode list is based on: 
        Chen T., Kao M., Tepel M., Rush J., Church G. (2001). A Dynamic Programming Approach to De Novo Peptide Sequencing via Tandem Mass Spectrometry.
        It is unclear if a peak of the spectrum is an N-terminal or a C-terminal ion. It is therefore assumed that each N-terminal b-ion has the extra mass
        of one Hydrogen and a complementary C-terminal y-ion which can be calculated as the difference between the precursor mass and the b-ion mass.
    
    *)
    
    
    /// Generates N - list from a list of Peaks. 
    let nIonsOfSpectrum precursorMass (spectrum : PeakList<Peak>) = 
        let peakToB (p : Peak) = SpectrumGraphNode.N(convertFromMass (p.Mz - Mass.Table.PMassInU))
        let nStartforN = SpectrumGraphNode.N(0. |> convertFromMass)
        let cEndforC = SpectrumGraphNode.N((precursorMass - H20_Mass) |> convertFromMass)
        let precursor = SpectrumGraphNode.N((precursorMass) |> convertFromMass)
        spectrum
        |> List.fold (fun acc item -> (peakToB item) :: acc) [ nStartforN; precursor ]
        |> List.sortBy valueOfSpectrumGraphNode
    
    /// Generates C-terminal ion list from a list of Peaks. 
    let cIonsOfSpectrum precursorMass (spectrum : PeakList<Peak>) = 
        let peakToY precursorMass (p : Peak) = SpectrumGraphNode.C(convertFromMass ((precursorMass) - p.Mz))
        let cStartforN = SpectrumGraphNode.C (0. |> convertFromMass)
        let cEndforC = SpectrumGraphNode.C((precursorMass - H20_Mass - Mass.Table.PMassInU) |> convertFromMass)
        let precursor = SpectrumGraphNode.C((precursorMass) |> convertFromMass)
        spectrum
        |> List.fold (fun acc item -> (peakToY precursorMass item) :: acc) [cStartforN;cEndforC;precursor]
        |> List.sortBy valueOfSpectrumGraphNode
    
    /// Generates C-terminal ion list with additional nodes representing charged peaks. 
    let cIonsOfSpectrumWCharges precursorMass (chargeState : int) (spectrum : PeakList<Peak>) =
        let cStartforC = SpectrumGraphNode.C ((H20_Mass - Mass.Table.PMassInU)|> convertFromMass)
        let cStartforN = SpectrumGraphNode.C (0. |> convertFromMass) 
        let cEndforN = SpectrumGraphNode.C((precursorMass - H20_Mass - Mass.Table.PMassInU) |> convertFromMass)
        let precursor = SpectrumGraphNode.C((precursorMass) |> convertFromMass)
        let peakToY0 precursorMass (p : Peak) = SpectrumGraphNode.C(convertFromMass ((precursorMass) - p.Mz))
        
        let calcCharge precursorMass (c : int) (p : Peak) = 
            [ for i in 1..c do
                  yield (SpectrumGraphNode.C
                             (convertFromMass (((precursorMass) - (Mass.ofMZ p.Mz ((float chargeState) - (float i))))))) ]
        
        let x1 = spectrum |> List.fold (fun acc item -> (peakToY0 precursorMass item) :: acc) [cStartforN; cStartforC; cEndforN; precursor]
        match chargeState with
        | 1 -> x1
        | 2 -> 
            spectrum
            |> List.fold (fun acc item -> (calcCharge precursorMass 1 item) :: acc) [ x1 ]
            |> List.concat
            |> List.sortBy valueOfSpectrumGraphNode
        | 3 -> 
            spectrum
            |> List.fold (fun acc item -> (calcCharge precursorMass 2 item) :: acc) [ x1 ]
            |> List.concat
            |> List.sortBy valueOfSpectrumGraphNode
        | 4 -> 
            spectrum
            |> List.fold (fun acc item -> (calcCharge precursorMass 3 item) :: acc) [ x1 ]
            |> List.concat
            |> List.sortBy valueOfSpectrumGraphNode
        | _ -> 
            spectrum
            |> List.fold (fun acc item -> (calcCharge precursorMass 4 item) :: acc) [ x1 ]
            |> List.concat
            |> List.sortBy valueOfSpectrumGraphNode
        
    
    /// Weights the edges depending on node-node type differences.
    let getWeight (pNode, cNode) = 
        match pNode, cNode with
        | (N x), (N y) -> 1.5
        | (C x), (C y) -> 1.5
        | (_), (_) -> System.Double.NegativeInfinity
    
    /// Returns labelled edge list from NC - list as (SpectrumGraphNode * SpectrumGraphNode * AminoAcids.AminoAcid) list .
    let findEdges accuracy (aaReferences : (AminoAcids.AminoAcid * float) array) (ncIons : SpectrumGraphNode list) = 
        let max = 
            aaReferences
            |> Array.maxBy snd
            |> snd
        
        let limit = (Mass.deltaMassByPpm accuracy max) + max
        
        /// Searches in the AA-reference array for a matching difference.
        let findAAMass refAaMass queryMass = 
            let mm, pm = Mass.rangePpm accuracy queryMass
            Array.tryFind (fun (x, n) -> n <= pm && n > mm) refAaMass
        
        /// Calculates and matches the differences of one SpectrumGraphNode with each SpectrumGraphNode of the same type.
        let rec inner fromNC (ncIons : SpectrumGraphNode list) acc = 
            match ncIons with
            | h :: t -> 
                if (isEqualSpectrumGraphNodeType h fromNC) then 
                    let massDiff = convertToMass (valueOfSpectrumGraphNode h - valueOfSpectrumGraphNode fromNC)
                    match (findAAMass aaReferences massDiff) with
                    | Some(aa, mass) -> inner fromNC t ((fromNC, h, aa) :: acc) // (h,fromNC,aa)
                    | None -> 
                        if massDiff > limit then acc
                        else inner fromNC t acc
                else inner fromNC t acc
            | [] -> acc
        
        let rec outer (ncIons : SpectrumGraphNode list) acc = 
            match ncIons with
            | h :: t -> outer t (inner h t acc)
            | [] -> acc
        
        outer ncIons []
    
    
    
    
    (*
        2. Creation of a functional graph.
        ==================================
        This segment of code is used to set up a functional graph with the [Functional Graph Library] (https://github.com/CSBiology/FSharp.FGL) based on: 
        Erwig, M. (2001). Inductive graphs and functional graph algorithms. The previously defined list of nodes & edges are
        represented in form of a functional graph, consisting of a list of contexts, whereas each context is related to a specific node
        and contains the information about the nodes predecessor and successor list, as well as its label. 
    
    *)


    
    /// Accesses the context of a specific node and returns either the node, the predecessor list, the successor list or the label.
    module NodeContext = 
        let pNode context = 
            let pNode, (_, _, _) = context
            pNode
        
        let predecessors context = 
            let _, (pred, _, _) = context
            pred
        
        let successors context = 
            let _, (_, _, sucs) = context
            sucs
        
        let labels context = 
            let _, (_, labels, _) = context
            labels

    /// Represents a list of nodes as a functional graph. The accuracy value defines the ppm range in which edges between the nodes are detected.
    let createfunGraph accuracy (aaReferences : (AminoAcids.AminoAcid * float) array) (ncIons : SpectrumGraphNode list) = 
        let nodeList = ncIons |> List.mapi (fun i node -> (node, i + 1))
        let edgeList = findEdges accuracy aaReferences ncIons
        
        Graph.empty |> Vertices.addMany nodeList |> Edges.addMany edgeList  
       
    
    
    