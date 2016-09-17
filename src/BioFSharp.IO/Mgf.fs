namespace BioFSharp.IO

/// Mgf <http://www.matrixscience.com/help/data_file_help.html>`_ is a simple
/// human-readable format for MS/MS data. It allows storing MS/MS peak lists and
/// exprimental parameters.
module Mgf = 

    open System
    open FSharp.Care
    open FSharp.Care.IO

    /// Represents 
    type MgfEntry = {   
        Parameters : Map<string,string>
        Mass       : float []
        Intensity  : float []
    }

    let createMgfEntry parameters mass intensity =
        {Parameters = parameters; Mass = mass; Intensity = intensity;}

    /// Returns the precursor mass. Returns None if the information can't be optained
    let tryGetPrecursorMass (mgf:MgfEntry) =
            if mgf.Parameters.ContainsKey("PEPMASS") then
                Some (String.tryParseFloatDefault nan mgf.Parameters.["PEPMASS"])
            else
                None
        

    /// Returns the posible charge/charges in a list. Returns None if the information can't be optained
    let tryParseCharge str =
        let subParse (digit,sign) =
            if sign = "-" then 
              - int digit
            else
                int digit

        match str with
            | Regex.RegexGroups @"(?<digit>\d*)(?<sign>[+-])" l -> Some (l |> List.map (fun g -> subParse (g.["digit"].Value,g.["sign"].Value)))
            | _ -> None
        
    /// Returns the precursor mass. Returns None if the information can't be optained
    let tryGetPrecursorCharges (mgf:MgfEntry) =
            if mgf.Parameters.ContainsKey("CHARGE") then
                tryParseCharge mgf.Parameters.["CHARGE"]
            else
                None        

    /// Returns the title string of a 'mgf-entry'. Returns None if the information can't be optained
    let tryGetTitle (mgf:MgfEntry) =
            if mgf.Parameters.ContainsKey("TITLE") then
                Some (mgf.Parameters.["TITLE"])
            else
                None        


    /// Returns the retention time and precursor intensity from 'mgf-title' string. Returns None if title does not contain the information
    let tryParseTitle title =
        match title with
            | Regex.RegexValues @"[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?" [ ret; intens; ] -> Some( float ret, float intens )
            | _ -> None



    /// Reads an mgf file into a collection of MgfEntries
    let readMgf path =

        let parseLineInside paramsMap ml (line:string) =
            let spLine = line.Split('=')
            // spectrum-specific parameters
            if spLine.Length > 1 then
                let tmp = Map.add spLine.[0] spLine.[1] paramsMap
                (true,ml,tmp)
            else
                // peak list
                if spLine.Length = 1 then
                    let spLine' = line.Split(' ')
                    let m = String.tryParseFloatDefault nan spLine'.[0]
                    let i = String.tryParseFloatDefault nan spLine'.[1]
                    (true,(m,i)::ml,paramsMap)
                else 
                    failwithf "Format Exception: peak list" 


        let mgfFrom ml (p:Map<string,string>) =
            let m,i = ml  |> List.rev |> List.unzip
            createMgfEntry p (m|> List.toArray) (i |> List.toArray)    
    
    
        let tmp =
            Seq.fromFile path      
            // filter comments (#;!/)
            |> Seq.filter (fun line ->not ( line.StartsWith("#") &&
                                            line.StartsWith("!") &&
                                            line.StartsWith("/") ))
            // filter empty lines
            |> Seq.filter (fun line -> line <> String.Empty)
            |> Seq.fold (fun state line ->                                         
                                let line = line.Trim()
                                //printfn "->%s" line
                                match state with
                                | (true,ml,p),l -> if line <> "END IONS" then
                                                      (parseLineInside p ml (line:string)),l
                                                   else
                                                      let tmp = mgfFrom ml p
                                                      (false,[],Map.empty),(tmp::l)
                                | (false,_,_),l   -> if line <> "BEGIN IONS" then failwithf "Format Exception: BEGIN IONS"
                                                     (true,[],Map.empty),l
                                                                                                
                                    ) ((false,[],Map.empty),[])
        snd tmp




    /// Converts a MgfEntry to string.
    /// Use Seq.write to write to file. 
    let mgfToString (mgf:MgfEntry) =
        seq   [ yield "BEGIN IONS"
                for p in mgf.Parameters do
                    yield sprintf "%s=%s" p.Key p.Value
                yield "\n"
                for m,i in  Seq.zip mgf.Mass mgf.Intensity do
                    yield sprintf "%f %f" m i
                yield "\n"
                yield "END IONS"
                ] 
