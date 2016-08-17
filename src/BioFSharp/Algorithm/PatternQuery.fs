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

