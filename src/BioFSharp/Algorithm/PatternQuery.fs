namespace BioFSharp.Algorithm

///The naive search algorithm.
module Naive =

    ///find
    let findAll (pattern : array<'a>) (s : array<'a>) =

        let rec loop (sPos:int) (patternPos:int) (matchCount:int) (resultList : int list) =

            if sPos < s.Length then
           
                if pattern.[patternPos] = s.[sPos] then
                    
                    if patternPos = (pattern.Length-1) then

                        loop (sPos - matchCount + 1) 0 0 ((sPos - matchCount)::resultList)
            
                    else 

                        loop (sPos+1) (patternPos+1) (matchCount+1) resultList
                else
                    loop (sPos - matchCount + 1) 0 0 resultList

            else
                
                List.rev resultList
        loop 0 0 0 []


    let findFirstFrom (startPos : int) (pattern : array<'a>) (s : array<'a>) =
        
        let rec loop (sPos:int) (patternPos:int) (matchCount:int) =
            
            if sPos < s.Length then

                if pattern.[patternPos] = s.[sPos] then
                    
                    if patternPos = (pattern.Length-1) then
                        
                        (sPos - pattern.Length)

                    else
                        
                        loop (sPos+1) (patternPos+1) (matchCount+1)
                else
                    
                    loop (sPos - matchCount + 1) 0 0
            else

                -1

        loop 0 0 startPos


    let findFirst (pattern : array<'a>) (s : array<'a>) = findFirstFrom 0 pattern s
        


module RabinKarp =
    
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
    
    let findAllGeneric (updateHash : int -> 'a -> 'a -> int) (blockHash : 'a array -> int ) (pattern : array<'a>) (s : array<'a>) = 
        let m,n = pattern.Length,s.Length
        let hpattern = pattern |> blockHash
        let initHs   = s.[0..m-1] |> blockHash

        let rec loop i hs resultList =
            if i < (n-m) then 
                if hpattern = hs then
                    if (isMatchAt pattern s i)then
                        loop (i+1) (updateHash hs s.[i+m] s.[i]) (i::resultList)

                    else 
                        loop (i+1) hs resultList

                else 
                    loop (i+1) (updateHash hs s.[i+m] s.[i]) resultList
            else
                if (isMatchAt pattern s (n-m)) then
                    ((n-m)::resultList) |> List.rev

                else
                    List.rev resultList
        
        loop 0 initHs []

    let findFirstFromGeneric (startPos:int) (updateHash : int -> 'a -> 'a -> int) (blockHash : 'a array -> int ) (pattern : array<'a>) (text : array<'a>) = 
        let m,n = pattern.Length,text.Length
        let hpattern = pattern |> blockHash
        let initHs   = text.[0..m-1] |> blockHash

        let rec loop i hs =
            if i < (n-m) then 
                if hpattern = hs then
                    if (isMatchAt pattern text i)then
                        i
        
                    else 
                        loop (i+1) hs 

                else 
                    loop (i+1) (updateHash hs text.[i+m] text.[i]) 

            else
                if (isMatchAt pattern text (n-m)) then
                    (n-m)
        
                else
                    -1
        
        loop startPos initHs

    // https://github.com/lemire/rollinghashjava/blob/master/src/rollinghash/RabinKarpHash.java
    module RKStandard =

        // b = 31
        // k is the length in items of the blocks you want to hash
        // returns bK
        let initBaseEndFromLength b k = 
            [0..k-1] |> List.fold (fun state i -> state*b) 1
            
        let addToHash b cHashvalue c =
            b*cHashvalue + hash c

        let blockHash b (pattern:array<'a>) =
            pattern |> Array.fold (fun state c -> addToHash b state c) 0

        let updateHash b bK cHashvalue inchar outchar =
            b*cHashvalue + (hash inchar) - bK * (hash outchar)
    
    module RabinKarpSearch =

        let findAll (pattern : array<'a>) (s : array<'a>) = 
            findAllGeneric (RKStandard.updateHash 31 (RKStandard.initBaseEndFromLength 31 pattern.Length)) (RKStandard.blockHash 31) pattern s 
        
        let findFirstFrom (startPos:int) (pattern : array<'a>) (s : array<'a>) =
            findFirstFromGeneric startPos (RKStandard.updateHash 31 (RKStandard.initBaseEndFromLength 31 pattern.Length)) (RKStandard.blockHash 31) pattern s 

        let findFirst (pattern : array<'a>) (s : array<'a>) =
            findFirstFrom 0 pattern s

    module Buzz = 

        let blockHash (pattern:array<'a>) =
            let n = pattern.Length
            let rec loop index pow acc =
                if index < pattern.Length then
                    loop (index+1) (pow-1) (acc ^^^ ((int <| pattern.[index]) <<< pow))
                else
                    acc
            loop 0 (n-1) 0

        let inline updateHash patternLength cHashvalue inchar outchar =
            
            (cHashvalue <<< 1) ^^^ ((int outchar) <<< patternLength) ^^^ (int inchar)
    
    module BuzzSearch =
        
        let findAll (pattern : array<'a>) (s : array<'a>) = 
            findAllGeneric (Buzz.updateHash pattern.Length) (Buzz.blockHash) pattern s 
        
        let findFirstFrom (startPos:int) (pattern : array<'a>) (s : array<'a>) =
            findFirstFromGeneric startPos (Buzz.updateHash pattern.Length) (Buzz.blockHash) pattern s 

        let findFirst (pattern : array<'a>) (s : array<'a>) =
            findFirstFrom 0 pattern s
    
    

//    let SearchAtFindFirstFrom (pattern:array<'a>) (s:array<'a>) (startPos:int) =    
//        let m,n = pattern.Length,s.Length 
//        let b = 31
//        
//        if startPos > n-m then 
//            -1
//        else
//        
//            let hpattern = pattern |> Array.fold (fun state c -> RKStandard.add b state c) 0
//            let initHs   = s.[startPos..startPos+m-1] |> Array.fold (fun state c ->RabinKarpHash.add b state c) 0   
//            let rkhUpdate = RabinKarpHash.update b (RabinKarpHash.initBaseEndFromLength b m )
//    
//            let rec loop i hs =
//                if i <= n-m then 
//                    if hpattern = hs then
//                        //printfn"1 -"
//                        if (isMatchAt pattern s i)then
//                            //printfn"match"
//                            i
//                        else 
//                            //printfn"incr"
//
//                            loop (i+1) hs
//                    else 
//                        //printfn"kp"
//                        loop (i+1) (rkhUpdate hs s.[i+m] s.[i])
//                else
//                    -1
//            loop startPos initHs
    
//    let Search (pattern:array<'a>) (s:array<'a>) =    
//        SearchAt pattern s 0
//
//    let SearchAll (pattern:array<'a>) (s:array<'a>)=    
//        let m,n = pattern.Length,s.Length 
//        let b = 31
//        let hpattern = pattern |> Array.fold (fun state c -> RabinKarpHash.add b state c) 0
//        let initHs   = s.[0..m-1] |> Array.fold (fun state c ->RabinKarpHash.add b state c) 0   
//        let rkhUpdate = RabinKarpHash.update b (RabinKarpHash.initBaseEndFromLength b m )
//
//        let rec loop i hs resultList =
//            if i < n-m then 
//                if hpattern = hs then
//                    if (isMatchAt pattern s i)then
//                        //printfn"match"
//                        loop (i+1) (rkhUpdate hs s.[i+m] s.[i]) (i::resultList)
//                    else 
//                        //printfn"incr"
//                        loop (i+1) hs resultList
//                else 
//                    //printfn"kp"
//                    loop (i+1) (rkhUpdate hs s.[i+m] s.[i]) resultList
//            else
//
//                if (isMatchAt pattern s (n-m)) then
//
//                    ((n-m)::resultList) |> List.rev
//
//                else
//
//                    List.rev resultList
//        loop 0 initHs []
            
//    let inline SearchAllBuzz (pattern:array<'a>) (s:array<'a>) =   
// 
//        let m,n = pattern.Length,s.Length 
//        let hpattern = BuzzHash.blockHash pattern
//        let initHs   = s.[0..m-1] |> BuzzHash.blockHash 
//        let rkhUpdate = BuzzHash.update m 
//        
//        let rec loop i hs resultList =
//            if i < (n-m) then 
//                if hpattern = hs then
//                    //printfn"1 -"
//                    if (isMatchAt pattern s i)then
//
//                        loop (i+1) (rkhUpdate hs s.[i+m] s.[i]) (i::resultList)
//
//                    else 
//                        //printfn"incr"
//                        loop (i+1) hs resultList
//                else 
//                    //printfn"kp"
//                    loop (i+1) (rkhUpdate hs s.[i+m] s.[i]) resultList
//            else
//
//                if (isMatchAt pattern s (n-m)) then
//
//                    ((n-m)::resultList) |> List.rev
//
//                else
//
//                    List.rev resultList
//
//        loop 0 initHs []  
        
    
//    let inline SearchAllBuzzCont (pattern:array<'a>) (s:array<'a>) =    
//        let m,n = pattern.Length,s.Length 
//        let hpattern = BuzzHash.createBuzhash pattern
//        let initHs   = s.[0..m-1] |> BuzzHash.createBuzhash
//        let rkhUpdate = BuzzHash.update m
//        
//        let rec loop i hs cont =
//            if i < (n-m+1) then 
//                if hpattern = hs then
//                    
//                    if (isMatchAt pattern s i) then
//
//                        if i + m = n then
//                            cont []
//
//                        else
//                            loop (i+1) (rkhUpdate hs s.[i+m] s.[i]) (fun acc -> cont(i::acc))
//                    else 
//                        //printfn"incr"
//                        loop (i+1) hs (fun acc -> cont (acc))
//                else 
//                    //printfn"kp"
//                    loop (i+1) (rkhUpdate hs s.[i+m] s.[i]) (fun acc -> cont (acc))
//            else
//                
//                cont []
//                
//        loop 0 initHs id 
    
    
    
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
    
    let findAllMatches (pattern : array<'a>) (s : array<'a>) (prefixTable : int [])=
    
        let rec matcher matchStart patternPosition resultList =
    
            if (matchStart + patternPosition) < s.Length then
    
                if pattern.[patternPosition] = s.[(matchStart + patternPosition)] then
    
                    if patternPosition = (pattern.Length - 1)  then
    
                        
                        matcher (matchStart + patternPosition - prefixTable.[patternPosition]) prefixTable.[patternPosition] (matchStart::resultList)
                    
                    else
                        matcher matchStart (patternPosition + 1) resultList
    
                else
    
                    if prefixTable.[patternPosition] > -1 then
    
                        matcher (matchStart + patternPosition - prefixTable.[patternPosition]) prefixTable.[patternPosition] resultList
                    else
                        
                         
                        matcher (matchStart + 1) 0 resultList
            else
                
                List.rev resultList
    
        matcher 0 0 []    
    
    
    let initFindAllMatches (pattern : array<'a>) = 
        //makes sure that the pattern will only be used with its respective prefixTable
        //initializing: let yourFunctionName = initFindAllMatches yourPattern 
        //!! dont use the name of any of the already existing functions as the name of "yourFunction"!! 
        //to find matches: yourFunctionName yourText
    
    
        let prefixTable = createPrefixTable pattern
    
        let findAllMatches (pattern : array<'a>) (prefixTable : int []) (s : array<'a>)=
    
        //matchStart: start of the current match in the text.
        //patternPosition: index of the current character in the pattern
    
            let rec matcher matchStart patternPosition resultList =
    
                if (matchStart + patternPosition) < s.Length then
    
                    if pattern.[patternPosition] = s.[(matchStart + patternPosition)] then
    
                        if patternPosition = (pattern.Length - 1)  then
    
                            //printfn "Match found! starting at position %i" matchStart
                            matcher (matchStart + patternPosition - prefixTable.[patternPosition]) prefixTable.[patternPosition] (matchStart::resultList)
    
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
    let findFirstMatch (prefixTable : int []) (pattern : array<'a>) (s : array<'a>) =
    
        let rec matcher matchStart patternPosition =
    
            if (matchStart + patternPosition + 1) <= s.Length then
    
                if pattern.[patternPosition] = s.[(matchStart + patternPosition)] then
    
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


