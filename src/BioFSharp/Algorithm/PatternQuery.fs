namespace BioFSharp.Algorithm

module RabinKarp =

    // https://github.com/lemire/rollinghashjava/blob/master/src/rollinghash/RabinKarpHash.java
    module RabinKarpHash =

        // b = 31
        // k is the length in items of the blocks you want to hash
        // returns bK
        let initBaseEndFromLength b k = 
            [0..k-1] |> List.fold (fun state i -> state*b) 1
            
        let add b cHashvalue c =
            b*cHashvalue + hash c

        let update b bK cHashvalue inchar outchar =
            b*cHashvalue + (hash inchar) - bK * (hash outchar)

    let isMatchAt (pattern:array<'a>) (s:array<'a>) (startPos:int) =    
        let rec loop i = 
            if i < pattern.Length then
                if pattern.[i] = s.[startPos+i] then
                    loop (i+1)
                else
                    false
            else 
                true
        if startPos+pattern.Length > s.Length then false else loop 0

    let SearchAt (pattern:array<'a>) (s:array<'a>) (startPos:int) =    
        let m,n = pattern.Length,s.Length 
        let b = 31
        let hpattern = pattern |> Array.fold (fun state c -> RabinKarpHash.add b state c) 0
        let initHs   = s.[0..m-1] |> Array.fold (fun state c -> RabinKarpHash.add b state c) 0   
        let rkhUpdate = RabinKarpHash.update b (RabinKarpHash.initBaseEndFromLength b m )

        let rec loop i hs =
            if i < n-m+1 then 
                if hpattern = hs then
                    if (isMatchAt pattern s i)then
                        i
                    else 
                        loop (i+1) hs
                else 
                    loop (i+1) (rkhUpdate hs s.[i+m] s.[i])
            else
                -1
        loop startPos initHs
    
    let Search (pattern:array<'a>) (s:array<'a>) =    
        let m,n = pattern.Length,s.Length 
        let b = 31
        let hpattern = pattern |> Array.fold (fun state c -> RabinKarpHash.add b state c) 0
        let initHs   = s.[0..m-1] |> Array.fold (fun state c -> RabinKarpHash.add b state c) 0   
        let rkhUpdate = RabinKarpHash.update b (RabinKarpHash.initBaseEndFromLength b m )

        let rec loop i hs =
            if i < n-m+1 then 
                if hpattern = hs then
                    if (isMatchAt pattern s i)then
                        i
                    else 
                        loop (i+1) hs
                else 
                    loop (i+1) (rkhUpdate hs s.[i+m] s.[i])
            else
                -1
        loop 0 initHs


//Index out of range!
    let rec SearchAll (pattern:array<'a>) (s:array<'a>) (startPos:int) (resultList : int list) =
        if (startPos + pattern.Length) < s.Length then
            let leMatch = SearchAt pattern s startPos
            SearchAll pattern s leMatch (leMatch::resultList)
        else
            List.rev resultList




module KnuthMorrisPratt = 

    let createPrefixTable (pattern : array<'a>) =
        let prefixTable = Array.zeroCreate pattern.Length
    
        let rec prefixSet startIndex matchCount =
            
            
            let writeIndex = startIndex + matchCount
            if writeIndex < pattern.Length then
    
    
                if pattern.[writeIndex] = pattern.[matchCount] then
                    
    
                    prefixTable.[writeIndex] <- matchCount
    
                    prefixSet startIndex (matchCount + 1)
    
    
                else
    
                    prefixTable.[writeIndex] <- matchCount
    
                    prefixSet (writeIndex + 1) 0
    
        prefixSet 1 0
    
        if pattern.Length > 0 then prefixTable.[0] <- -1
        prefixTable
    
    
    
    
    //Algorithms to find all matches of a pattern in a text
    
    
    
    //without currying
    //needs a precomputed prefixTable, done by the createPrefixTable function
    
    let findAllMatches (pattern : array<'a>) (text : array<'a>) (prefixTable : int [])=
    
        let rec matcher matchStart patternPosition resultList =
    
            if (matchStart + patternPosition + 1) <= text.Length then
    
                if pattern.[patternPosition] = text.[(matchStart + patternPosition)] then
    
                    if patternPosition = (pattern.Length - 1)  then
    
                        //printfn "Match found! starting at position %i" matchStart
                        matcher (matchStart +  prefixTable.[(pattern.Length - 1)] + 1) 0 (matchStart::resultList)
                    
                    else
                        matcher matchStart (patternPosition + 1) resultList
    
                else
    
                    if prefixTable.[patternPosition] > -1 then
    
                        matcher (matchStart + patternPosition - prefixTable.[patternPosition]) prefixTable.[patternPosition] resultList
                    else
                        
                         
                        matcher (matchStart + 1) 0 resultList
            else
                //printfn "Matches found starting on the following indices of the text:" 
                List.rev resultList
    
        matcher 0 0 []    
    
    
    let initFindAllMatches (pattern : array<'a>) = 
        //makes sure that the pattern will only be used with its respective prefixTable
        //initializing: let yourFunctionName = initFindAllMatches yourPattern 
        //!! dont use the name of any of the already existing functions as the name of "yourFunction"!! 
        //to find matches: yourFunctionName yourText
    
    
        let prefixTable = createPrefixTable pattern
    
        let findAllMatches (pattern : array<'a>) (prefixTable : int []) (text : array<'a>)=
    
        //matchStart: start of the current match in the text.
        //patternPosition: index of the current character in the pattern
    
            let rec matcher matchStart patternPosition resultList =
    
                if (matchStart + patternPosition + 1) <= text.Length then
    
                    if pattern.[patternPosition] = text.[(matchStart + patternPosition)] then
    
                        if patternPosition = (pattern.Length - 1)  then
    
                            //printfn "Match found! starting at position %i" matchStart
                            matcher (matchStart +  prefixTable.[(pattern.Length - 1)] + 1) 0 (matchStart::resultList)
    
                        else
                            matcher matchStart (patternPosition + 1) resultList
    
                    else
    
                    //printfn "5"
    
                        if prefixTable.[patternPosition] > -1 then
    
                            matcher (matchStart + patternPosition - prefixTable.[patternPosition]) prefixTable.[patternPosition] resultList
    
                        else
    
                            matcher (matchStart + 1) 0 resultList
                else
                    //printfn "Matches found starting on the following indices of the text:" 
                    List.rev resultList
    
            matcher 0 0 [] 
    
        findAllMatches pattern prefixTable
    
    
    
    //first Match finders
    //Algorithms for finding the first match of a pattern in a text. Terminates after the first match is found  
    let findFirstMatch (prefixTable : int []) (pattern : array<'a>) (text : array<'a>) =
    
        let rec matcher matchStart patternPosition =
    
            if (matchStart + patternPosition + 1) <= text.Length then
    
                if pattern.[patternPosition] = text.[(matchStart + patternPosition)] then
    
                    if patternPosition = (pattern.Length - 1)  then
    
                        //printfn "Match found! starting at position %i" matchStart
                        matchStart
                    
                    else
                        matcher matchStart (patternPosition + 1) 
    
                else
    
                    if prefixTable.[patternPosition] > -1 then
    
                        matcher (matchStart + patternPosition - prefixTable.[patternPosition]) prefixTable.[patternPosition] 
                    else
                        
                         
                        matcher (matchStart + 1) 0 
            else
                //printfn "no matches found"
                -1    
                
    
        matcher 0 0 
    
    
    
    
    let initFindFirstMatch (pattern : array<'a>) = 
        //makes sure that the pattern will only be used with its respective prefixTable
        //initializing: let yourFunctionName = initFindFirstMatch yourPattern 
        //!! dont use the name of any of the already existing functions as the name of "yourFunction"!! 
        //to find matches: yourFunctionName yourText
    
    
        let prefixTable = createPrefixTable pattern
        findFirstMatch  prefixTable pattern

