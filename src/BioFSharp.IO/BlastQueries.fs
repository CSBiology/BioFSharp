namespace BioFSharp.IO

open BioFSharp.Refactor

module BlastHits =

    type BlastHit(queryId:string, subjectId:string,rank:int) =
        inherit DynamicObj ()
        //interface ITrace with
            // Implictit ITrace    
        member val QueryId = queryId with get,set
        member val SubjectId = subjectId with get,set
        member val Rank = rank with get,set

    // Returns subject id from BlastHit.
    let getQueryId (bh:BlastHit) =
        bh.QueryId

    // Returns query id from BlastHit.
    let getSubjectId (bh:BlastHit) =
        bh.SubjectId

    // Returns rank from BlastHit.
    let getRank (bh:BlastHit) =
        bh.Rank

    // Returns query length from BlastHit. If not set default is -1.
    let getQueryLength (bh:BlastHit) =
        //bh?``subject length``
        match bh.TryGetTypedValue<string>("query length") with 
        | Some v -> int v
        | None   -> -1


    // Returns subject length from BlastHit. If not set default is -1.
    let getSubjectLength (bh:BlastHit) =
        match bh.TryGetTypedValue<string>("subject length") with 
        | Some v -> int v
        | None   -> -1


    // Returns alignment length from BlastHit. If not set default is -1.
    let getAlignmentLength (bh:BlastHit) =

        match bh.TryGetTypedValue<string>("alignment length") with 
        | Some v -> int v
        | None   -> -1


    // Returns number of mismatches from BlastHit. If not set default is -1.
    let getMismatches (bh:BlastHit) =
        match bh.TryGetTypedValue<string>("mismatches") with 
        | Some v -> int v
        | None   -> -1


    // Returns number of identical matches from BlastHit. If not set default is -1.
    let getIdentical (bh:BlastHit) =
        match bh.TryGetTypedValue<string>("identical") with 
        | Some v -> int v
        | None   -> -1


    // Returns number of positive matches from BlastHit. If not set default is -1.
    let getPositives (bh:BlastHit) =
        match bh.TryGetTypedValue<string>("positives") with 
        | Some v -> int v
        | None   -> -1



    // Returns evalue from BlastHit. If not set default is -1.
    let getEValue (bh:BlastHit) =
        match bh.TryGetTypedValue<string>("evalue") with 
        | Some v -> int v
        | None   -> -1

    // Returns bit score from BlastHit. If not set default is -1.
    let getBitScore (bh:BlastHit) =
        match bh.TryGetTypedValue<string>("bit score") with 
        | Some v -> int v
        | None -> -1


    let lengthSimilarity (bh:BlastHit) =
        let ql = getQueryLength bh |> float
        let sl = getSubjectLength bh |> float
        min ql sl / max ql sl

    let subjectQuerySimilarity (bh:BlastHit) =
        let al = getAlignmentLength bh |> float
        let il = getIdentical bh |> float
        min al il / max al il



// 
module BlastQueries =
    open BlastHits

    type BlastQuery =
        | NoHits of string 
        | Hits   of string * BlastHit list


    let getQueryId (bq:BlastQuery) =
        match bq with 
        | NoHits qid    -> qid
        | Hits (qid,_)  -> qid

    let getBlastHits (bq:BlastQuery) =
        match bq with 
        | NoHits _    -> []
        | Hits (_,bh) -> bh


    // Returns the best hit from blast query.
    let tryGetBestHit (bq:BlastQuery) =
        match bq with 
        | NoHits _    -> None
        | Hits (_,bh) -> 
            match bh with 
            | h::t -> Some  h
            | []   -> None

    let mapHits mapping (bq:BlastQuery) =
        match bq with 
        | NoHits _    -> bq
        | Hits (qid,bh) -> 
            BlastQuery.Hits(qid,List.map mapping bh)

    let filterHits predicate (bq:BlastQuery) =
        match bq with 
        | NoHits _    -> bq
        | Hits (qid,bh) -> 
            let fHits = List.filter predicate bh
            match fHits with
            | [] -> BlastQuery.NoHits qid
            | _  -> BlastQuery.Hits(qid,fHits)


    open FSharpAux
    open FSharpAux.IO





    /// Reads BlastQuery from file enumerator 
    let fromFileEnumerator (fileEnumerator) =

        // Conditon of grouping lines
        let sameGroup l =             
            //not (String.length l = 0 || l.[0] <> '#')
            not (String.length l = 0 || l.StartsWith("# BLA") |> not)

        let rec listRevWithRank rank (list:BlastHit list) acc=
            match list with
            | [] -> acc
            | [x] -> x.Rank<-rank
                     x::acc
            | head::tail -> 
                head.Rank<-rank
                listRevWithRank (rank-1) tail (head::acc)

        // Matches grouped lines and concatenates them    
        let rec record  (query:string) (fields:string array) (hitList:BlastHit list) (lines:string list)=
            match lines with
            | line::tail when line.StartsWith "# Query"  -> record (line.Remove(0,9).Trim()) fields hitList tail
            | line::tail when line.StartsWith "# Fields" -> record query (line.Remove(0,10).Split(',')) hitList tail
            | line::tail when line.StartsWith "#"        -> record query fields hitList tail
            | item::tail ->
                let split = item.Split('\t')
                let bh = BlastHit (split.[0],split.[1],-1)
                for i=2 to split.Length-1 do
                    DynObj.setValue bh (fields.[i].Trim()) split.[i]
                record query fields (bh::hitList) tail 
            | [] -> match hitList with
                    | _::_ -> BlastQuery.Hits (query , listRevWithRank hitList.Length hitList [])
                    | []   -> BlastQuery.NoHits query
        
        // main
        fileEnumerator
        |> Seq.groupWhen sameGroup 
        |> Seq.map (fun l -> 
            let l' = List.ofSeq l
            record "" [||] [] l' )


    /// Reads BlastQuery from file.
    let fromFile (filePath) =
        FileIO.readFile filePath
        |> fromFileEnumerator