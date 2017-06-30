/// The effectiveness rises with longer patterns

namespace BoyerMooreStringSearchAlgorithm 

module BoyerMoore=

    // <summary>
    /// Creates N(j) that is needed for the good suffix rule.
    // <summary>
    //
    /// <param name="searchString" >array of 'a/param>
    /// <returns> an array containing the strong suffix rule values </returns> 
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
    /// <param name="suffixes" >array of int</param>
    /// <param name="searchString" >array of 'a</param>
    /// <remarks>Shifting distance becomes 1 if the bad character rule and the matchcount returns 0</remarks>
    /// <returns> an array containing the shiftig values created by using the good suffix heuristics </returns> 
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
    /// <param name="pattern" >array of 'a when 'a : (static member op_Explicit :  'a -> int)</param>
    /// <remarks> rewrites the shifting values if a character is contained more than once. This happens until the last appearence of the character is found. </remarks>
    /// <returns> an array containing the shiftig values created by using the bad character rule </returns>  
    let inline getBadCharacterShift (pattern:array<'a> when 'a : (static member op_Explicit :  'a -> int)) =
        let length = pattern.Length
        let badCharacters = Array.create 91 (length)      
        for index = 0 to length - 1 do       
            badCharacters.[(int pattern.[index])] <- length - index - 1
        badCharacters


    // <summary>
    /// Retruns a list with the beginning positions of the searched pattern.
    // <summary>
    //
    /// <param name="source" >array of 'a when 'a : (static member op_Explicit :  'a -> int)</param>
    /// <param name="query" >array of 'a when 'a : (static member op_Explicit :  'a -> int)</param>
    /// <param name="is" >int</param>
    /// <remarks> the shifting distance is created in compairing the values of the good suffix heuristics value and the bad character rule </remarks>
    /// <returns> a list with the beginning positions of the searched pattern </returns> 
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

