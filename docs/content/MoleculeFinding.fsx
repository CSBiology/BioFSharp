(*** hide ***)
#r "../../bin/BioFSharp.dll"
#r "../../packages/build/FSharp.Plotly/lib/net40/FSharp.Plotly.dll"

(**
Molecule Finding Algorithm
==========================

Given a specific mass and a list of atoms, this algorithm 
looks for atom-combinations, that fit together in mass and
valence electrons.
This documentations provides five examples on how to use the algorithm for 
finding post translational modifications.


List of atoms
-------------
At first a record type is defined "atom". It consists
of the symbol, the monoisotopic mass and the number of valence 
electrons of a chemical element.  The monoisotopic masses can be
found in the isotope table of the BioFSharp repository.
*)


open System
open BioFSharp
open Isotopes.Table
open AminoAcids
open FSharp.Plotly

type atom = {
    symbol : string
    mass : float;
    valence : int;
    }

let H =  { symbol = "H" ; mass = H1.Mass  ; valence = 1 }
let C =  { symbol = "C" ; mass = C12.Mass ; valence = 4 }
let N =  { symbol = "N" ; mass = N14.Mass ; valence = 5 }
let O =  { symbol = "O" ; mass = O16.Mass ; valence = 6 }
let P =  { symbol = "P" ; mass = 30.973762; valence = 5 }
let S =  { symbol = "S" ; mass = S32.Mass; valence = 6 }
let Se = { symbol = "Se"; mass = Se74.Mass; valence = 6 }

(**
Now a list can be created. In addition a list without
Hydrogen is necessary.The five examples shown here use the following list of atoms:
*)
let atoms = [ Se; S; P; O; N; C; H;]
let nodes = [ Se; S; P; O; N; C;] 

(**
The Core Function
-----------------
The algorithm now takes the two predefined lists, a float value as gap (Monoisotopic Mass of examples) 
and another float value as accuracy (0.5 in this examples).

- example 1: Name: Hypusine, Monoisotopic Mass: 87.068414, Atom-Composition: H(9) C(4) N O 
- example 2: Name: Ethylphosphate, Description: O-Ethylphosphorylation, Monoisotopic Mass: 107.997631, Atom-Composition: H(5) C(2) O(3) P
- example 3: Name: DTT_ST , Description: Dithiothreitol (DTT), Monoisotopic Mass: 136.001656 , Atom-Composition: H(8) C(4) O S(2) 
- example 4: Name: Malonyl, Description: Malonylation, Monoisotopic Mass:  86.000394, Atom-Composition: H(2) C(3) O(3) 
- example 5: Name:  Name: Guanidination, Monoisotopic Mass:  42.021798, Atom-Composition: H(2) C N(2)
*)

let moleculefkt (allAtoms: atom list) (nodeAtoms: atom list) (gap: float) (accuracy: float) =
    let rec inner (allAtoms: atom list) (nodeAtoms: atom list) (gap: float) (accuracy: float) (molecule: atom list) (acc: atom list list) (counter: int) =
        allAtoms |> List.map (fun x ->
            
            match (gap,counter) with
            |(g,_) when g = 0. 
                
                -> molecule::acc
            
            |(g,_) when g > 0. && g < accuracy 
            
                -> molecule::acc
            
            |(g,_) when g < 0. && g > (0.-accuracy) 
            
                -> molecule::acc


            |(g,c) when g > accuracy && c = 9 
                
                -> inner allAtoms nodeAtoms (gap-x.mass) accuracy (x::molecule) acc x.valence
            
            |(g,c) when g > accuracy && c > 1 && c < 9
            
                -> inner allAtoms nodeAtoms (gap-x.mass) accuracy (x::molecule) acc (counter-1)

            |(g,c) when g > accuracy && c = 1 
            
                -> inner nodeAtoms nodeAtoms (gap-x.mass) accuracy (x::molecule) acc (x.valence-1) 

            |_
            
                -> acc
        ) |> List.concat

    inner allAtoms nodeAtoms gap accuracy [] [] 9

(**
Example results
---------------
*)
let resulteg1 = moleculefkt atoms nodes 87.068414 0.5
(**
example 1: Name: Hypusine, Monoisotopic Mass: 87.068414, Atom-Composition: H(9) C(4) N O,  Found?-NO , number of potential molecules: 16979, 
time: Real: 00:00:00.185, CPU: 00:00:00.234, GC Gen0: 10, Gen1: 0, Gen2: 0
*)

let resulteg2 = moleculefkt atoms nodes 107.997631 0.5
(**
- example 2: Name: Ethylphosphate, Monoisotopic Mass: 107.997631, Atom-Composition: H(5) C(2) O(3) P, Found?-YES, number of potential molecules: 80529,
time: Real: 00:00:00.543, CPU: 00:00:00.546, GC Gen0: 82, Gen1: 1, Gen2: 0
*)

let resulteg3 = moleculefkt atoms nodes 136.001656 0.5
(**
- example 3: Name: DTT_ST , Monoisotopic Mass: 136.001656 , Atom-Composition: H(8) C(4) O S(2), Found?-YES , number of potential molecules: 995704,
time: Real: 00:00:07.470, CPU: 00:00:07.953, GC Gen0: 968, Gen1: 10, Gen2: 1
*)

let resulteg4 = moleculefkt atoms nodes 86.000394 0.5
(**
- example 4: Name: Malonyl, Monoisotopic Mass:  86.000394, Atom-Composition: H(2) C(3) O(3), Found?-YES , number of potential molecules: 13755,
time: Real: 00:00:00.070, CPU: 00:00:00.046, GC Gen0: 11, Gen1: 1, Gen2: 0
*)

let resulteg5 = moleculefkt atoms nodes 42.021798 0.5
(**
- example 5: Name: Guanidination, Monoisotopic Mass:  42.021798, Atom-Composition: H(2) C N(2), Found?-YES , number of potential molecules: 60,
time: Real: 00:00:00.004, CPU: 00:00:00.000, GC Gen0: 0, Gen1: 0, Gen2: 0


Time complexity
---------------
O(n*g)^g
*)

(***hide***)
let values = [0.004; 0.07; 0.185; 0.543; 7.47] 
let keys = ["Guanidination: 42u, found";"Malonyl: 86u, found";"Hypusine: 88u, not found"; "Ethylphosphate: 108u, found";"DTT_ST: 136u, found"]
let labels = ["60 potential molecules"; "13755 potential molecules"; "16979 potential molecules"; "80529 potential molecules"; "995704 potential molecules"]
(*** define-output:bar1 ***)
Chart.Column(keys,values,Labels=labels,Opacity=1.0,Marker=Options.Marker(Color="rgba(10,100,100,1.0)"))
(*** include-it:bar1 ***)
|> Chart.Show
