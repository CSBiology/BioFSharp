(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "../../packages/build/FSharp.Plotly/lib/net40/Fsharp.Plotly.dll"
open FSharp.Plotly


(**
Introducing BlastWrapper
========================

BlastWrapper is a tool for performing different tasks in NCBI BLAST console applications (version 2.2.31+). 
Currently available:
    - blastP: compares a protein query to a protein database
    - blastN: compares a nucleotide query to a nucleotide database
    - makeblastdb: creates a blast database frome given source/s

*)
#r "../../bin/BioFSharp.dll"
#r "../../bin/BioFSharp.IO.dll"

open BioFSharp
open BioFSharp.IO
open BlastNCBI

// Creation of a BLAST database using makeblastdb


///path and name of the input file/database
let inputFile = "./data/Chlamy_Cp.fastA"

///defines database type
let (dbType: Parameters.DbType) = Parameters.Nucleotide
let typeOfDb = Parameters.MakeDbParams.DbType dbType


///optional: name of BLAST database to be created
// Required if multiple sources files are provided as input.
// if no path is provided, the database will be localized in the same folder as the makeblastb.exe file.
// if not provided the default name is the name of the input file/database.

let outputDb = Parameters.MakeDbParams.Output("./data/blastoutput/Chlamy_Cp_Db.fastA")

//Runs the makeblastdb.exe with the input path and a sequence of makeDbParams
//define path of the ncbi bin folder
let ncbiPath = (__SOURCE_DIRECTORY__ + "../../ncbi-blast\bin")
BlastWrapper(ncbiPath).makeblastdb inputFile ([typeOfDb; outputDb] |> seq<Parameters.MakeDbParams>)









