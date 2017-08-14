(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../bin/BioFSharp.dll"

(**
Isotopes
========
<a id="SourceCode" href="https://github.com/CSBiology/BioFSharp/blob/master/src/BioFSharp/Isotopes.fs">&lt;/&gt;view source code</a>
<a id="Author" href="https://github.com/kMutagene">&#128366;view author of this tutorial</a>
<br><br>

Isotopes are variants of a particular chemical element which differ in neutron number. Knowledge about the isotopic distribution in a sample has widespreaded applications including isotopic analysis and isotopic labeling.

Isotopes are implemented as a record type containing the following fields: 
<br><br>

<div id="responsiveTable">

| Field name    | type      | Description                                                                        | Example                                                |
| ------------- | --------- | ---------------------------------------------------------------------------------- | ------------------------------------------------------ |
| AtomicSymbol  | `string`  | Atomic Symbol of the isotope                                                       | "H" for Hydrogen, "D" for Deuterium                    |
| AtomicNumberZ | `int`     | Proton count (Z) of the isotope                                                    | 1 for Hydrogen                                         |
| MassNumber    | `int`     | Combined proton and neutron count (A) of the isotope                               | 1 for Hydrogen, 2 for Deuterium                        |
| Mass          | `float`   | Mass of the isotope (unit:u)                                                       | 1.00782503207 for Hydrogen, 2.0141017778 for Deuterium |
| NatAbundance  | `float`   | respective natural mole-fraction abundance of the isotope                          | 0.999885 for Hydrogen, 0.000115 for Deuterium          |
| RelAtomicMass | `float`   | ratio of the average mass of atoms of an element to one unified atomic mass unit.  | 1.007947 for both Hydrogen and Deuterium               |

</div>
<br>

Creating an isotope
-------------------
The `Table` module provides prebuilt Isotopes for most biological relevant elements. 

You can either directly create an isotope type like any other record type or use the provided `create` function:

*)
open BioFSharp.Isotopes

let N14 = 
    { 
    AtomicSymbol  = "N";
    AtomicNumberZ = 7;
    MassNumber    = 14;
    Mass          = 14.0030740048;
    NatAbundance  = 0.99636;
    RelAtomicMass = 14.0067;
    }

let N15 = create "N" 7 15 15.0001088982 0.00364 14.0067

(**
<br>
*)
