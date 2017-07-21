(*** hide ***)
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.Mz.dll"
#r "../../packages/build/FSharp.Plotly/lib/net40/FSharp.Plotly.dll"

(**
#Molecule Finding


Given a specific mass and a list of atoms, this algorithm looks for atom-combinations, that fit together in mass and valence electrons. This documentations provides five examples on how to use the algorithm to identify post translational modifications.

*)

open System
open BioFSharp
open BioFSharp.Mz
open Isotopes.Table
open FSharp.Plotly
open MoleculeFinding

(**
##Input


The function receaves a list of atoms as input. In addition a list without Hydrogen is necessary, because it only has one valence electron and binds only to one `atom`. So it can not act as `node` between two or more atoms.
The five examples shown here use the following list of atoms:
*)
let atoms = [ Se; S; P; O; N; C; H;]
let nodes = [ Se; S; P; O; N; C;] 

(**The Algorithm takes the two predefined lists, a float value as gap (Monoisotopic Mass of examples) and another float value as accuracy (0.5 in this examples). The result is a list of potential molecules.
*)


(**
##Example Results

*)
let resulteg1 = find atoms nodes 87.068414 0.5
(**
###Example 1

- Name: **Hypusine**
- Monoisotopic Mass: _87.068414_
- Atom-Composition: **H(9) C(4) N O**
- Found?-**NO** 
- Number of potential molecules: _16979_
- Time: Real: 00:00:00.185, CPU: 00:00:00.234, GC Gen0: 10, Gen1: 0, Gen2: 0
*)

let resulteg2 = find atoms nodes 107.997631 0.5
(**
###Example 2

- Name: **Ethylphosphate**
- Description: O-Ethylphosphorylation
- Monoisotopic Mass: _107.997631_
- Atom-Composition: **H(5) C(2) O(3) P**
- Found?-**YES**
- Number of potential molecules: _80529_
- Time: Real: 00:00:00.543, CPU: 00:00:00.546, GC Gen0: 82, Gen1: 1, Gen2: 0
*)

let resulteg3 = find atoms nodes 136.001656 0.5
(**
###Example 3

- Name: **DTT_ST**
- Description: Dithiothreitol (DTT)
- Monoisotopic Mass: _136.001656_
- Atom-Composition: **H(8) C(4) O S(2)**
- Found?-**YES**
- Number of potential molecules: _995704_
- Time: Real: 00:00:07.470, CPU: 00:00:07.953, GC Gen0: 968, Gen1: 10, Gen2: 1
*)

let resulteg4 = find atoms nodes 86.000394 0.5
(**
###Example 4

- Name: **Malonyl**
- Monoisotopic Mass: _86.000394_
- Atom-Composition: **H(2) C(3) O(3)**
- Found?-**YES** 
- Number of potential molecules: _13755_
- Time: Real: 00:00:00.070, CPU: 00:00:00.046, GC Gen0: 11, Gen1: 1, Gen2: 0
*)

let resulteg5 = find atoms nodes 42.021798 0.5
(**
###Example 5

- Name: **Guanidination**
- Monoisotopic Mass: _42.021798_
- Atom-Composition: **H(2) C N(2)**
- Found?-**YES** 
- Number of potential molecules: _60_
- Time: Real: 00:00:00.004, CPU: 00:00:00.000, GC Gen0: 0, Gen1: 0, Gen2: 0


##Time complexity

O(n*g)^g
*)

(***hide***)
let values = [0.004; 0.07; 0.185; 0.543; 7.47] 
let keys = ["Guanidination: 42u, found";"Malonyl: 86u, found";"Hypusine: 88u, not found"; "Ethylphosphate: 108u, found";"DTT_ST: 136u, found"]
let labels = ["60 potential molecules"; "13755 potential molecules"; "16979 potential molecules"; "80529 potential molecules"; "995704 potential molecules"]
(*** define-output:bar1 ***)
Chart.Column(keys,values,Labels=labels,Opacity=1.0,Marker=Options.Marker(Color="rgba(10,100,100,1.0)"))
(*** include-it:bar1 ***)

