module PDBTests

open BioFSharp
open BioFSharp.IO
open BioFSharp.IO.PDB
open Expecto
open System.Reflection

let atomSingleton = """ATOM   1058  N   ARG A 141      -6.466  12.036 -10.348  7.00 19.11           N  """

let atomSequence = TestingUtils.readEmbeddedDocument "ATOMSequence.txt"

let hetatmSingleton = """HETATM 1109  CAD HEM A   1       7.618   5.696 -20.432  6.00 21.38           C  """

let hetatmSequence = TestingUtils.readEmbeddedDocument "HETATMSequence.txt"

let terSingleton = """TER    1070      ARG A 141""".PadRight(80, ' ')

let glucagon = TestingUtils.readEmbeddedDocument "Glucagon.txt"
    
let HasA = TestingUtils.readEmbeddedDocument "HasA.txt"

[<Tests>]
let recordTests = testList "PDB.RecordTests" [
    testCase "ATOM singleton parsing" (fun _ ->
        let actual = PDB.tryParseCoordinateLine atomSingleton
        let expected = 
            Some (PDB.Coordinate.Atom { 
                SerialNumber = 1058
                Name = { 
                    ChemicalSymbol = " N"
                    RemotenessIndicator = " "
                    BranchDesignator = " " 
                }
                AlternateLocationIndicator = ""
                ResidueName = "ARG"
                ChainIdentifier = "A"
                ResidueSequenceNumber = 141
                ResidueInsertionCode = ""
                X = -6.466
                Y = 12.036
                Z = -10.348
                Occupancy = 7.0
                TemperatureFactor = 19.11
                SegmentIdentifier = ""
                ElementSymbol = "N"
                Charge = "" 
            })
        Expect.equal actual expected "PDB.tryParseCoordinateLine did not return the correct ATOM record"
    )

    testCase "ATOM singleton roundtrip" (fun () ->
        let actual =
            [atomSingleton]
            |> Seq.choose PDB.tryParseCoordinateLine
            |> Seq.item 0
            |> Coordinate.toString

        Expect.equal actual atomSingleton "atom singleton roundtrip unsuccessful"
    )

    testCase "ATOM sequence roundtrip" (fun () ->
        let actual =
            atomSequence
            |> Array.choose PDB.tryParseCoordinateLine
            |> Array.map Coordinate.toString

        Expect.sequenceEqual actual atomSequence "atom sequence roundtrip unsuccessful"
    )

    testCase "HETATM singleton parsing" (fun _ ->
        let actual = PDB.tryParseCoordinateLine hetatmSingleton
        let expected = 
            Some (PDB.Coordinate.HetAtom { 
                SerialNumber = 1109
                Name = { 
                    ChemicalSymbol = " C"
                    RemotenessIndicator = "A"
                    BranchDesignator = "D" 
                }
                AlternateLocationIndicator = ""
                ResidueName = "HEM"
                ChainIdentifier = "A"
                ResidueSequenceNumber = 1
                ResidueInsertionCode = ""
                X = 7.618
                Y = 5.696
                Z = -20.432
                Occupancy = 6.0
                TemperatureFactor = 21.38
                SegmentIdentifier = ""
                ElementSymbol = "C"
                Charge = "" 
            })
        Expect.equal actual expected "PDB.tryParseCoordinateLine did not return the correct HETATM record"
    )

    testCase "HETATM singleton roundtrip" (fun () ->
        let actual =
            [hetatmSingleton]
            |> Seq.choose PDB.tryParseCoordinateLine
            |> Seq.item 0
            |> Coordinate.toString

        Expect.equal actual hetatmSingleton "atom singleton roundtrip unsuccessful"
    )

    testCase "HETATM sequence roundtrip" (fun () ->
        let actual =
            hetatmSequence
            |> Array.choose PDB.tryParseCoordinateLine
            |> Array.map Coordinate.toString

        Expect.sequenceEqual actual hetatmSequence "atom sequence roundtrip unsuccessful"
    )
]

[<Tests>]
let compositeTests = testList "PDB.CompositeTests" [
    testCase "Glucagon roundtrip" (fun _ ->
        let actual = 
            glucagon
            |> Array.choose PDB.tryParseCoordinateLine
            |> Array.map Coordinate.toString

        Expect.equal actual glucagon "glucagon chain roundtrip unsuccessful"
    )

    testCase "HasA roundtrip" (fun _ ->
        let actual = 
            HasA
            |> Array.choose PDB.tryParseCoordinateLine
            |> Array.map Coordinate.toString

        Expect.equal actual HasA "glucagon chain roundtrip unsuccessful"
    )
]