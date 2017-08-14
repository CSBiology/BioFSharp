namespace BioFSharp.Algorithm

///A collection of different string matching algorithms
module StringMatching =
    
    ///A collection of naive string matching algorithms
    module Naive =
    
    
        ///finds all matches of a query pattern in a source
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
            if s.Length > 0 then loop 0 0 0 [] else []
    
    
        ///finds the first match of a query pattern in a source starting from a specific position in the source
        let findFrom (startPos : int) (pattern : array<'a>) (s : array<'a>) =
            let rec loop (sPos:int) (patternPos:int) (matchCount:int) =
                if sPos < s.Length then
                    if pattern.[patternPos] = s.[sPos] then
                        if patternPos = (pattern.Length-1) then
                            (sPos - pattern.Length + 1)
                        else
                            loop (sPos+1) (patternPos+1) (matchCount+1)
                    else
                        loop (sPos - matchCount + 1) 0 0
                else
                    -1
            if s.Length > 0 then loop startPos 0 0 else -1
    
    
        ///finds the first match of a query pattern in a source
        let find (pattern : array<'a>) (s : array<'a>) = findFrom 0 pattern s
            
    
    ///A collection of Rabin-Karp string matching algorithms and hash functions
    module RabinKarp =
        
        ///checks if content of pattern and source substring match
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
    
        
        ///takes an updateHash and blockHash function to find all matches of a query pattern in a source
        let inline findAllGeneric (updateHash : 'c -> 'a -> 'a -> 'c (*when 'c : unmanaged*)) (blockHash : 'a array -> 'c (*when 'c: unmanaged*)) (pattern : array<'a>) (s : array<'a>) = 
            let m,n = pattern.Length,s.Length
            if m < n && n > 0 then 
                let hpattern = pattern |> blockHash
                let initHs   = s.[0..m-1] |> blockHash
    
                let rec loop i hs resultList =
                    if i < (n-m) then 
                        if hpattern = hs then
                            if (isMatchAt pattern s i)then
                                loop (i+1) (updateHash hs s.[i+m] s.[i]) (i::resultList)
                            else 
                                loop (i+1) (updateHash hs s.[i+m] s.[i]) resultList
                        else 
                            loop (i+1) (updateHash hs s.[i+m] s.[i]) resultList
                    else
                        if (isMatchAt pattern s (n-m)) then
                            ((n-m)::resultList) |> List.rev
                        else
                            List.rev resultList
                loop 0 initHs []
            else []
    
    
        ///takes an updateHash and blockHash function to find the first match of a query pattern in a source starting from a specific position in the source
        let findFromGeneric (startPos:int) (updateHash : 'c -> 'a -> 'a -> 'c (*when 'c : unmanaged*)) (blockHash : 'a array -> 'c (*when 'c: unmanaged*)) (pattern : array<'a>) (text : array<'a>) = 
            let m,n = pattern.Length,text.Length
            if m < n && n > 0 then 
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
            else -1
    
    
        // https://github.com/lemire/rollinghashjava/blob/master/src/rollinghash/RabinKarpHash.java
        ///A collection of Rabin-Karp string matching algorithms using the built-in hash function of f#
        module RKStandard =
    
            // b = 31
            // k is the length in items of the blocks you want to hash
            // returns bK
            let initBaseEndFromLength b k = 
                [0..k-1] |> List.fold (fun state i -> state*b) 1
                
            ///adds a hashvalue to an existing hashvalue
            let addToHash b cHashvalue c =
                b*cHashvalue + hash c
    
            ///hashes a pattern
            let blockHash b (pattern:array<'a>) =
                pattern |> Array.fold (fun state c -> addToHash b state c) 0
    
            ///updates an existing hashvalue 
            let updateHash b bK cHashvalue inchar outchar =
                b*cHashvalue + (hash inchar) - bK * (hash outchar)
    
            ///finds all matches of a query pattern in a source
            let findAll (pattern : array<'a>) (s : array<'a>) = 
                findAllGeneric (updateHash 31 (initBaseEndFromLength 31 pattern.Length)) (blockHash 31) pattern s 
            
            ///finds the first match of a query pattern in a source starting from a specific position in the source
            let findFrom (startPos:int) (pattern : array<'a>) (s : array<'a>) =
                findFromGeneric startPos (updateHash 31 (initBaseEndFromLength 31 pattern.Length)) (blockHash 31) pattern s 
    
            ///finds the first match of a query pattern in a source
            let find (pattern : array<'a>) (s : array<'a>) =
                findFrom 0 pattern s
    
    
        ///A collection of Rabin-Karp string matching algorithms using the cyclic polynomial (CP) hash
        module CP = 
    
            ///bitwise cyclic rotation of a 64 bit pattern
            let inline rotateLeft r x  = (x <<< r) ||| (x >>> (64 - r))
    
    
            ///adds a hashvalue to an existing hashvalue
            let inline addToHashValue hs inchar =
                (rotateLeft 1 hs) ^^^ (uint64 inchar)
    
    
            ///hashes a pattern
            let inline blockHash (pattern : array<'a>) =
                let m = pattern.Length
                let rec loop i hs =
                    if i < m then
                        loop (i+1) (addToHashValue hs pattern.[i])
                    else
                        hs
                loop 0 0UL
    
    
            ///updates an existing hashvalue 
            let inline updateHash pLength hs inchar outchar =
                let z = (rotateLeft pLength (uint64 outchar)) 
                (rotateLeft 1 hs) ^^^ z ^^^ (uint64 inchar)
    
    
            ///find all matches of a query pattern in a source   
            let inline findAll (pattern : array<'a>) (s : array<'a>) = 
                findAllGeneric (updateHash pattern.Length) (blockHash) pattern s 
            
    
            ///find the first match of a query pattern in a source starting from a specific position in the source
            let inline findFrom (startPos:int) (pattern : array<'a>) (s : array<'a>) =
                findFromGeneric startPos (updateHash pattern.Length) (blockHash) pattern s 
    
    
            ///find the first match of a query pattern in a source
            let inline find (pattern : array<'a>) (s : array<'a>) =
                findFrom 0 pattern s
        
    ///A collection of Knuth-Morris-Pratt string matching algorithms
    module KnuthMorrisPratt = 
    
        ///creates a prefix table for a query pattern
        let createPrefixTable (pattern : array<'a>) =
            let prefixTable = Array.zeroCreate pattern.Length
    
            let rec loop startIndex matchCount =
                let writeIndex = startIndex + matchCount
                
                if writeIndex < pattern.Length then
                    if pattern.[writeIndex] = pattern.[matchCount] then
                        prefixTable.[writeIndex] <- matchCount
                        loop startIndex (matchCount + 1)
                    else
                        prefixTable.[writeIndex] <- matchCount
                        loop (writeIndex + 1) 0
            loop 1 0
            if pattern.Length > 0 then prefixTable.[0] <- -1
            prefixTable
        
    
        ///finds all matches of a query pattern in a source using a given prefix table
        let findAll (prefixTable : int []) (pattern : array<'a>) (s : array<'a>) =
            let rec loop matchStart patternPosition resultList =
                if (matchStart + patternPosition) < s.Length then
                    if pattern.[patternPosition] = s.[(matchStart + patternPosition)] then
                        if patternPosition = (pattern.Length - 1)  then
                            loop (matchStart + patternPosition - prefixTable.[patternPosition]) prefixTable.[patternPosition] (matchStart::resultList)
                        else
                            loop matchStart (patternPosition + 1) resultList
                    else
                        if prefixTable.[patternPosition] > -1 then
                            loop (matchStart + patternPosition - prefixTable.[patternPosition]) prefixTable.[patternPosition] resultList
                        else
                            loop (matchStart + 1) 0 resultList
                else
                    List.rev resultList
            if s.Length > 0 then loop 0 0 [] else []   
        
    
        ///returns a findAll function with a set prefix table created from the input
        let initFindAll (pattern : array<'a>) = 
            let prefixTable = createPrefixTable pattern
            findAll prefixTable pattern
    
    
        ///finds the first match of a query pattern in a source starting from a specific position in the source using a given prefix table
        let findFrom (prefixTable : int []) (pattern : array<'a>) (startPos : int) (s : array<'a>) =
            let rec loop matchStart patternPosition =
                if (matchStart + patternPosition) < s.Length then
                    if pattern.[patternPosition] = s.[(matchStart + patternPosition)] then
                        if patternPosition = (pattern.Length - 1)  then
                            matchStart
                        else
                            loop matchStart (patternPosition + 1) 
                    else
                        if prefixTable.[patternPosition] > -1 then
                            loop (matchStart + patternPosition - prefixTable.[patternPosition]) prefixTable.[patternPosition] 
                        else
                            loop (matchStart + 1) 0 
                else
                    -1    
            loop startPos 0 
        
    
        ///returns a findFrom function with a set prefix table created from the input
        let initFindFrom (pattern : array<'a>) = 
            let prefixTable = createPrefixTable pattern
            findFrom prefixTable pattern 
    
    
        ///finds the first match of a query pattern in a source using a given prefix table
        let find (prefixTable : int []) (pattern : array<'a>) (s : array<'a>) =
            findFrom prefixTable pattern 0 s
    
    
        ///returns a find function with a set prefix table created from the input
        let initFind (pattern: array<'a>) =
            let prefixTable = createPrefixTable pattern
            findFrom prefixTable pattern 0
    
    
    
    ///A collection of Boyer-Moore string matching algorithms
    module BoyerMoore =
    
        // <summary>
        /// Creates N(j) that is needed for the good suffix rule.
        // <summary>
        //
        // <param name="searchString" >array of 'a/param>
        // <returns> an array containing the strong suffix rule values </returns> 
        let suffixes (searchString:array<'a>) =
            let length = searchString.Length
            let suffixes = Array.zeroCreate length
            suffixes.[length - 1] <- 0   
    
            let rec getmatches is iq m =    
                if is < 0 || iq < 0 then
                    m
                else
                   if  searchString.[is] = searchString.[iq] then
                       getmatches (is-1) (iq-1) (m+1) 
                   else
                       m
    
            let rec findsuff leftIndex rightIndex =
                if leftIndex < (length-1) then
                    if searchString.[leftIndex] = searchString.[rightIndex] then
                        let sfxlength = getmatches leftIndex rightIndex 0
                        suffixes.[leftIndex] <- sfxlength
                        findsuff (leftIndex+1) (length-1)
                    else
                        findsuff (leftIndex+1) (length-1)
                else
                    suffixes
            findsuff 0 (length-1)
    
    
        // <summary>
        /// Creates an array containing the shiftig values created by using the good suffix heuristics.
        // <summary>
        //
        // <param name="suffixes" >array of int</param>
        // <param name="searchString" >array of 'a</param>
        // <remarks>Shifting distance becomes 1 if the bad character rule and the matchcount returns 0</remarks>
        // <returns> an array containing the shiftig values created by using the good suffix heuristics </returns> 
        let getGoodSuffixShift (suffixes:int[]) (searchString:array<'a> ) =
            let length = searchString.Length    
            let goodSuffixes1 = Array.create length length
    
            let createPreArray=
                let rec goodshift1 rightIndex= 
                    if rightIndex >= 1 then
                        if (suffixes.[rightIndex] = rightIndex + 1) then
                            goodshift1 (rightIndex-1)
                        else
                            goodshift1 (rightIndex-1)
                    else
                        goodSuffixes1
                goodshift1 (length-1)
    
            let goodSuffix = createPreArray
            let rec lastchanges rightIndex =
                if rightIndex <= length-2 then
                    goodSuffix.[length - 1 - suffixes.[rightIndex]] <- length - 1 - rightIndex    
                    lastchanges (rightIndex+1)
                else
                    goodSuffix
            lastchanges 0
    
        // <summary>
        /// Creates an array for any values containing the shiftig values created by using the bad character rule.
        // <summary>
        //
        // <param name="pattern" >array of 'a when 'a : (static member op_Explicit :  'a -> int)</param>
        // <remarks> rewrites the shifting values if a character is contained more than once. This happens until the last appearence of the character is found. </remarks>
        // <returns> an array containing the shiftig values created by using the bad character rule </returns>  
        let inline getBadCharacterShift (pattern:array<'a> when 'a : (static member op_Explicit :  'a -> int)) =
            let length = pattern.Length
            let badCharacters = Array.create 91 (length)      
            for index = 0 to length - 1 do       
                badCharacters.[(int pattern.[index])] <- length - index - 1
            badCharacters
    
    
        // <summary>
        /// Returns a list with the beginning positions of the searched pattern. 
        // <summary>
        //
        // <param name="source" >array of 'a when 'a : (static member op_Explicit :  'a -> int)</param>
        // <param name="query" >array of 'a when 'a : (static member op_Explicit :  'a -> int)</param>
        // <param name="is" >int</param>
        // <remarks> the shifting distance is created in compairing the values of the good suffix heuristics value and the bad character rule </remarks>
        // <returns> a list with the beginning positions of the searched pattern </returns> 
        let inline searchBoyerMoore (badCharPattern:array<int>) (goodCharPattern:array<int>) is (source:array<'a> when 'a : (static member op_Explicit :  'a -> int)) (query:array<'a> when 'a : (static member op_Explicit :  'a -> int))=        
            if query.Length = 0 then failwith "query sequence must not be empty"
            let rec getmatchAmount is iq m =      
                if is >= source.Length || iq < 0 then
                    (m,is,iq)
                else
                   if source.[is] = query.[iq] then
                       getmatchAmount (is-1) (iq-1) (m+1) 
                   else
                       (m,is,iq)
                         
            let rec findMatches is iq =
                if is < source.Length then 
                    if source.[is] = query.[iq] then 
                        let (m,mismatchIS,mismatchIQ) = getmatchAmount (is) (query.Length-1) 0 
                        if m = (query.Length) then 
                               (is-((m)-1))
                        else                                                                        
                            let goodChar = goodCharPattern.[mismatchIQ] 
                            let shift = (max (badCharPattern.[int source.[mismatchIS]]) goodChar)     
                            findMatches (mismatchIS+shift) (query.Length-1) 
                    else
                        let goodChar = goodCharPattern.[iq] 
                        let m' = max (badCharPattern.[int source.[is]]) goodChar 
                        findMatches (is+m') (query.Length-1) 
                else
                    (-1) 
            findMatches is (query.Length-1) 
            
    
        // <summary>
        /// Finds the first occurence of a pattern in a string that is found behind the given index.
        // <summary>
        //
        /// <param name="source" >array of 'a when 'a : (static member op_Explicit :  'a -> int)</param>
        /// <param name="query" >array of 'a when 'a : (static member op_Explicit :  'a -> int)</param>
        /// <param name="startindex" >int</param>
        /// <returns> the first position of a pattern in a source that is found after behind the given index</returns>  
        let inline findFrom badCharPattern goodCharPattern startindex (source:array<'a> when 'a : (static member op_Explicit :  'a -> int)) (query:array<'a> when 'a : (static member op_Explicit :  'a -> int))  =
            searchBoyerMoore badCharPattern goodCharPattern (startindex+(query.Length-1)) source query
        
    
        // <summary>
        /// Finds the first occurence of a pattern in a string.
        // <summary>
        //
        /// <param name="source" >array of 'a when 'a : (static member op_Explicit :  'a -> int)</param>
        /// <param name="query" >array of 'a when 'a : (static member op_Explicit :  'a -> int)</param>
        /// <returns> the first position of a pattern in a source </returns>  
        let inline findFirst (source:array<'a> when 'a : (static member op_Explicit :  'a -> int)) (query:array<'a> when 'a : (static member op_Explicit :  'a -> int) ) = 
            let badCharPattern =  getBadCharacterShift query 
            let goodCharPattern = getGoodSuffixShift (suffixes query) query
            findFrom badCharPattern goodCharPattern 0 source query 
     
    
        // <summary>
        /// Finds all occurences of a pattern in a string.
        // <summary>
        //
        /// <param name="source" >array of 'a when 'a : (static member op_Explicit :  'a -> int)</param>
        /// <param name="query" >array of 'a when 'a : (static member op_Explicit :  'a -> int)</param>
        /// <returns> the  positions of a pattern in a source in a reversed order</returns>  
        let inline findAll (source:array<'a> when 'a : (static member op_Explicit :  'a -> int) ) (query:array<'a> when 'a : (static member op_Explicit :  'a -> int) ) = 
            let badCharPattern =  getBadCharacterShift query 
            let goodCharPattern = getGoodSuffixShift (suffixes query) query
            let currentElements startindex = findFrom badCharPattern goodCharPattern startindex source query  
            let rec findAll is indices =
                if is < source.Length then
                    let current = currentElements is
                    if current = (-1) then  
                        indices
                    else
                        findAll (current+query.Length) (current::indices)
    
                else
                    indices 
            findAll 0 []