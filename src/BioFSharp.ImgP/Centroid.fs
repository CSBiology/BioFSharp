namespace BioFSharp.ImgP

open FSharpAux
open FSharp.Stats
open System
open System.IO
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Threading

module Marr =
    type MarrWavelet =  {
        Scale           : float    
        Zero            : float    
        Minimum         : float    
        Diameter        : float    
        Values          : float [,]
        PadAreaRadius   : int      
        LMdistance      : int      
        zFilterdist     : float    
                        }
    
    let marrWaveletCreator (radius : float) = 
        let functionMarr x (y:float) s = (1./(Math.PI*s**2.))*(1.-(x**2.+y**2.)/(2.*s**2.))*(Math.E**(-((x**2.+y**2.)/(2.*s**2.))))
        let functionValuesMarr scale list= Array.map (fun y -> (Array.map (fun x -> functionMarr x y scale) list)) list

        {
        Scale           = 0.7071 * (radius )
        Zero            = radius   
        Minimum         = radius*2.
        Diameter        = radius*2.
        Values          = Array2D.ofJaggedArray (functionValuesMarr (0.7071 * (radius)) [|-(ceil (3. * radius + 2.))..(ceil(3. * radius + 2.))|])
        PadAreaRadius   = ceil (3. * radius + 2.) |> int 
        LMdistance      = (1.2619 * (radius) + 1.3095) |> round 0 |> int 
        zFilterdist     = 3.
        }

    let MarrRadius1_340 = marrWaveletCreator 1.339646565    //LMdistance = 3
    let MarrRadius2_132 = marrWaveletCreator 2.132102385    //LMdistance = 4
    let MarrRadius2_925 = marrWaveletCreator 2.924558206    //LMdistance = 5
    let MarrRadius3_717 = marrWaveletCreator 3.717014026    //LMdistance = 6
    let MarrRadius4_510 = marrWaveletCreator 4.509469847    //LMdistance = 7
    let MarrRadius5_302 = marrWaveletCreator 5.301925668    //LMdistance = 8
    let MarrRadius6_094 = marrWaveletCreator 6.094381488    //LMdistance = 9
    let MarrRadius6_887 = marrWaveletCreator 6.886837309    //LMdistance = 10
    let MarrRadius7_679 = marrWaveletCreator 7.679293129    //LMdistance = 11
    let MarrRadius8_472 = marrWaveletCreator 8.471748950    //LMdistance = 12
    let MarrRadius9_264 = marrWaveletCreator 9.264204771    //LMdistance = 13

module Centroid = 
    open FSharp.Stats.SpecialFunctions

    type LocalMaxima =  {
        FrameNumber : int 
        X           : float
        Y           : float
        Intensity   : float
                    } 

    let createLocalMaxima framenumber x y intensity = {
            FrameNumber = framenumber 
            X           = x
            Y           = y
            Intensity   = intensity
                        } 

    let loadTiff filePath= 
        let stream = File.OpenRead(filePath)
        let tiffDecoder = 
            new TiffBitmapDecoder(
                    stream,
                    BitmapCreateOptions.PreservePixelFormat ||| BitmapCreateOptions.IgnoreImageCache,
                    BitmapCacheOption.None);   
        Seq.init (tiffDecoder.Frames.Count) (fun frameIndex ->
            let cFrame = tiffDecoder.Frames.[frameIndex]
            let bytesPerPixel = cFrame.Format.BitsPerPixel / 8
            let convertedBitmap = new FormatConvertedBitmap(cFrame, PixelFormats.Default, null, 0.) //new FormatConvertedBitmap(cFrame, PixelFormats.Gray16, null, 0.)
            let width  = convertedBitmap.PixelWidth
            let height = convertedBitmap.PixelHeight
            let stride = width * bytesPerPixel
            let bytes : byte[] = Array.zeroCreate (width * height * bytesPerPixel)
            convertedBitmap.CopyPixels(bytes, width * bytesPerPixel, 0)
            let pixelSize = bytesPerPixel
            Array2D.init width height (fun x y -> 
                BitConverter.ToInt16 (bytes,stride * y + x * pixelSize) //ToInt16 default
                )
        )     
    
    let paddTiff (data: seq<'a[,]>)=
        let padArray2DWithRandom (rnd:System.Random) (offset:int) (arr:'a[,]) =
            let rowCount = Array2D.length1 arr 
            let colCount = Array2D.length2 arr
            let rowPadding = rowCount + offset
            let colPadding = colCount + offset
            Array2D.init (rowCount + offset * 2) (colCount + offset * 2)
                (fun rowI colI -> 
                    if (rowI < offset || colI < offset) || (rowI >= rowPadding  || colI >= colPadding) then
                        arr.[rnd.Next(0,rowCount),rnd.Next(0,colCount)] 
                    else
                        arr.[rowI-offset,colI-offset]              
                )
        
        let paddedRawData = 
            let rnd = System.Random()    
            data
            |> Seq.map (padArray2DWithRandom rnd 40)
            |> Array.ofSeq
        paddedRawData


    let inline C3DWT (marr: Marr.MarrWavelet) (frame:'a[,]) =   
        let resolutionPixelfst = (Array2D.length1 frame) - 40 * 2
        let resolutionPixelsnd = (Array2D.length2 frame) - 40 * 2
        let offset = marr.PadAreaRadius
        let paddingoffset = 40
        let (CWTArray2D0: float[,]) = Array2D.zeroCreate (Array2D.length2 frame) (Array2D.length1 frame)
        for x = paddingoffset to (paddingoffset + (resolutionPixelsnd-1)) do
            for y = paddingoffset to (paddingoffset + (resolutionPixelfst-1)) do
                CWTArray2D0.[x,y] <-
                    let rec loop acc' a b =
                        if a <= 2 * offset then
                            if b <= 2 * offset then
                                let acc = acc' + ((marr.Values).[a,b] * (frame.[(y+(a-offset)),(x+(b-offset))] |> float))
                                loop acc a (b + 1)
                            else
                                loop acc' (a + 1) 0
                        else acc'
                    loop 0. 0 0
        let deletePaddingArea =
            let arrayWithoutPaddingoffset = Array2D.zeroCreate ((Array2D.length1 CWTArray2D0)-(2*paddingoffset)) ((Array2D.length2 CWTArray2D0)-(2*paddingoffset))
            for i=paddingoffset to (Array2D.length1 CWTArray2D0)-(paddingoffset+1) do
                for j=paddingoffset to (Array2D.length2 CWTArray2D0)-(paddingoffset+1) do
                    arrayWithoutPaddingoffset.[(i-paddingoffset),(j-paddingoffset)] <- CWTArray2D0.[i,j]
            arrayWithoutPaddingoffset
        deletePaddingArea 

    ///gets Marr, a framenumber, an offset and the number of pixels to look at in the surrounding, and gives [,] of localMaxima
    let inline findLocalMaxima (marr:Marr.MarrWavelet) frame =   
        ///gets single 2D Array with only Maxima in it and gives coordinates of local maxima
        let allmaximaArray (arr:float[,]) =
            let rec loop acc i j =
                if i < (Array2D.length1 arr)-1 then
                    if j < (Array2D.length2 arr)-1  then 
                        if (arr.[i,j]) > 0. then loop ((float i, float j)::acc) i (j+1) 
                        else loop acc i (j+1)    
                    else loop acc (i+1) 0
                else acc
            loop [] 0 0 
        let numberofsurpix = marr.LMdistance
        let (cWTPercArray: float [,]) = C3DWT marr frame  
        let arrayOfMaxima = Array2D.zeroCreate ((Array2D.length1 cWTPercArray)) ((Array2D.length2 cWTPercArray))
        let checkListsForContinuousDecline b c numberofsurpix =      
            let createSurroundingPixelLists b c numberofsurpix =
                let rec loop i accN accS accW accE accNW accSW accNE accSE =
                    let imod = (i |> float) * 0.7071 |> floor |> int
                    if i <= numberofsurpix then 
                        loop (i+1) (cWTPercArray.[b+i   ,c     ]::accN )
                                    (cWTPercArray.[b-i   ,c     ]::accS )
                                    (cWTPercArray.[b     ,c-i   ]::accW )
                                    (cWTPercArray.[b     ,c+i   ]::accE )
                                    (cWTPercArray.[b+imod,c-imod]::accNW)
                                    (cWTPercArray.[b-imod,c-imod]::accSW)
                                    (cWTPercArray.[b+imod,c+imod]::accNE)
                                    (cWTPercArray.[b-imod,c+imod]::accSE)
                    else [accN;accS;accW;accE;accNW;accSW;accNE;accSE]
                loop 0 [] [] [] [] [] [] [] [] 
    
            let surroundingList = createSurroundingPixelLists b c numberofsurpix

            let rec isSortedAsc (list: float list) = 
                match list with
                    | [] -> true
                    | [x] -> true
                    | x::((y::_)as t) -> if x > y then false else isSortedAsc(t) 
            let boolList = surroundingList |> List.map  (fun x -> isSortedAsc x)
            (boolList |> List.contains false) = false

        //calculates checkListsForContinuousDecline for every pixel
        for i=numberofsurpix to (Array2D.length1 cWTPercArray)-(numberofsurpix+1) do 
            for j=numberofsurpix to (Array2D.length2 cWTPercArray)-(numberofsurpix+1) do 
                if cWTPercArray.[i,j] >= 10. then                              
                    if checkListsForContinuousDecline i j numberofsurpix = true     
                        then arrayOfMaxima.[i,j] <- cWTPercArray.[i,j]              
                    else arrayOfMaxima.[i,j] <- 0.                                  
                else arrayOfMaxima.[i,j] <- 0.                                   
        allmaximaArray arrayOfMaxima

    ///gets Marr-Wavelet and gives all LocalMaxima of the whole file as a List
    let inline collectAllFrameInformation2 (marr:Marr.MarrWavelet) (data:seq<'a[,]>) =
        let frames = [|0..(Seq.length data)-1|]
        
        let collectFrameInformation index (frame:'a[,]) =
            printfn "collecting local maxima (MarrRadius: %f Frame: %i)" marr.Zero (index + 1)

            let coorList = findLocalMaxima marr frame

            let intensityInWaveletspace fr = 
                //let offset = marr.PadAreaRadius
                //let paddingoffset = 40
                //let y = ((fst coorList.[fr]) |> int) + paddingoffset 
                //let x = ((snd coorList.[fr]) |> int) + paddingoffset 
                //let mutable acc = 0.
                //for a = 0 to 2*offset do
                //    for b = 0 to 2*offset do               
                //        acc <- acc + ((marr.Values.[a,b]) * (loadAndPadTiff.[frame].[(x+(a-offset)),(y+(b-offset))]))
                //acc
                0.

            let frameInformation = 
                [|0..coorList.Length-1|] 
                |> Array.map (fun x -> 
                    createLocalMaxima index (snd coorList.[x]) (fst coorList.[x]) (intensityInWaveletspace x)
                        )
            frameInformation

        let colFTInfo = 
            [|for x in frames -> async {return collectFrameInformation x (data |> Seq.item x)} |]
            |> Async.Parallel
            |> Async.RunSynchronously              
        colFTInfo

    let inline collectFrameInformation (marr:Marr.MarrWavelet) index (frame:'a[,]) =
        printfn "collecting local maxima (MarrRadius: %f Frame: %i)" marr.Zero (index + 1)
    
        let coorList = findLocalMaxima marr frame
    
        let intensityInWaveletspace = 0.
    
        let frameInformation = 
            [|0..coorList.Length-1|] 
            |> Array.map (fun x -> 
                createLocalMaxima index (snd coorList.[x]) (fst coorList.[x]) (intensityInWaveletspace)
                    )
        frameInformation


    ///filters the local maxima which are tempo-spatially in neighbourhood (gives distinct maxima).
    let zEqualityFilter followingSerialFrames distance (array: LocalMaxima [] [])= 
        ///calculates the centroid of a given LMlist
        let centroidOfLmArr (lMList: LocalMaxima []) =
            match lMList.Length with
            | 0 -> createLocalMaxima 0 0. 0. 0.
            | 1 -> createLocalMaxima 0 lMList.[0].X lMList.[0].Y 0.
            | _ ->  let xValues = Array.map (fun x -> x.X) lMList
                    let yValues = Array.map (fun x -> x.Y) lMList
                    let meanX   = (Array.fold (fun acc a -> acc + a) 0. xValues) / float (xValues.Length)
                    let meanY   = (Array.fold (fun acc a -> acc + a) 0. yValues) / float (yValues.Length)
                    createLocalMaxima 0 meanX meanY nan
        
        let numberOfFrames = array.Length
        
        let searchForSingle (lm: LocalMaxima)=
            let i = lm.FrameNumber
            let isolateZequalLocalMaxima =
                //collects all lM which are within the frames temporal distance an in x in spatial distance
                let zFilteredLMsingle = 
                    if i < numberOfFrames - followingSerialFrames then 
                        array.[i..i+followingSerialFrames-1]
                        |> Array.map (Array.filter (fun y -> FSharp.Stats.ML.DistanceMetrics.euclideanNaNSquared [lm.X;lm.Y] [y.X;y.Y] <= (distance**2.)))
                        |> Array.concat
                    else [||]
                // if there are less than x in temporal distance they get rejected, all other are real signal LocalMaxima
                let finish = 
                    if zFilteredLMsingle.Length >= followingSerialFrames 
                        then Array.append [|lm|] zFilteredLMsingle
                    else [||]
                centroidOfLmArr finish 
            isolateZequalLocalMaxima 
    
        let filterstp =
            let arrConc= array |> Array.concat
            let length = arrConc.Length
            arrConc 
            |> Array.mapi (fun i x ->   if i % 1000 = 0 then
                                            printfn "filtering local maxima: %i/%i"  i length
                                            searchForSingle x      
                                        else                           
                                            searchForSingle x 
                            ) 
            |> Array.distinct
    
        filterstp
    
    //let inline fu marr data= 
    //    let colFTInfo = 
    //        [|for x in [|0..(Seq.length data)-1|] -> async {return Centroid.collectFrameInformation marr x (data |> Seq.item x)} |]
    //        |> Async.Parallel
    //        |> Async.RunSynchronously              
    //    colFTInfo


    //gives final centroids and clusterresult
    let inline isolateCentroids eps minPts (marrList:Marr.MarrWavelet list) followingFrames zEqualityDistance (fu) (data:seq<'a[,]>)=  
        ///calculates the centroid of a given TupelList. This function is needed in the DBSCAN
        let centroidOfTupelList (tList: seq<float*float>) =
            let length = tList |> Seq.length
            if length = 0 
                then (0.,0.) //(nan,nan)
            elif length = 1 
                then tList |> Seq.item 0
            else 
                let xValues = tList |> Seq.map (fun x -> snd x) 
                let yValues = tList |> Seq.map (fun x -> fst x)
                let meanX   = (xValues |> Seq.fold (fun acc a -> acc + a) 0. ) / float (xValues |> Seq.length)
                let meanY   = (yValues |> Seq.fold (fun acc a -> acc + a) 0. ) / float (yValues |> Seq.length)
                let calculateCoorOfCentroid  = meanY,meanX
                calculateCoorOfCentroid

        let filteredPoints = 
            marrList
            |> List.map (fun x -> zEqualityFilter followingFrames zEqualityDistance (fu x data))//(collectAllFrameInformation x data))
            |> Array.concat     
            |> Array.distinct
            |> Array.map (fun x -> [|x.Y;x.X|])

        let clustercentroids = 
            let squaredEPs = (eps * eps)
            let clusterresult =
                FSharp.Stats.ML.Unsupervised.DbScan.compute FSharp.Stats.ML.DistanceMetrics.euclideanNaNSquared minPts squaredEPs filteredPoints                
            let clusterlist=                 
                clusterresult.Clusterlist 
                    |> Seq.map (fun x -> x 
                                            |> Seq.map (fun x -> x.[0],x.[1])
                                            |> centroidOfTupelList
                                ) 
            clusterlist,clusterresult
        clustercentroids

    //gives final centroids and clusterresult
    let inline isolateCentroids2 eps minPts (marrList:Marr.MarrWavelet list) followingFrames zEqualityDistance (data:seq<'a[,]>)=  
        ///calculates the centroid of a given TupelList. This function is needed in the DBSCAN
        let centroidOfTupelList (tList: seq<float*float>) =
            let length = tList |> Seq.length
            if length = 0 
                then (0.,0.) //(nan,nan)
            elif length = 1 
                then tList |> Seq.item 0
            else 
                let xValues = tList |> Seq.map (fun x -> snd x) 
                let yValues = tList |> Seq.map (fun x -> fst x)
                let meanX   = (xValues |> Seq.fold (fun acc a -> acc + a) 0. ) / float (xValues |> Seq.length)
                let meanY   = (yValues |> Seq.fold (fun acc a -> acc + a) 0. ) / float (yValues |> Seq.length)
                let calculateCoorOfCentroid  = meanY,meanX
                calculateCoorOfCentroid

        let filteredPoints = 
            marrList
            |> List.map (fun x -> zEqualityFilter followingFrames zEqualityDistance (collectAllFrameInformation2 x data))
            |> Array.concat     
            |> Array.distinct
            |> Array.map (fun x -> [|x.Y;x.X|])

        let clustercentroids = 
            let squaredEPs = (eps * eps)
            let clusterresult =
                FSharp.Stats.ML.Unsupervised.DbScan.compute FSharp.Stats.ML.DistanceMetrics.euclideanNaNSquared minPts squaredEPs filteredPoints                
            let clusterlist=                 
                clusterresult.Clusterlist 
                    |> Seq.map (fun x -> x 
                                            |> Seq.map (fun x -> x.[0],x.[1])
                                            |> centroidOfTupelList
                                ) 
            clusterlist,clusterresult
        clustercentroids

    let k = Math.PI
