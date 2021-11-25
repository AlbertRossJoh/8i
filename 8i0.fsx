open ImgUtil
open System

type point = int * int 
type color = ImgUtil.color
type figure =
    | Circle of point * int * color
    | Rectangle of point * point * color
    | Mix of figure * figure
/// <summary> a function for figuring out where to color </summary>
/// <param name = "(x, y)"> int * int </param name>
/// <returns> color </returns>
let rec colorAt (x, y) figure =
    match figure with
    | Circle ((cx, cy), r, col) ->
        if (x - cx) * (x - cx) + (y - cy) * (y - cy) <= r * r

        then Some col else None
    | Rectangle ((x0, y0), (x1, y1), col) ->
        if x0 <= x && x <= x1 && y0 <= y && y <= y1

        then Some col else None
    | Mix (f1, f2) ->
        match (colorAt (x, y) f1, colorAt (x, y) f2) with
        | (None , c) -> c
        | (c, None) -> c
        | (Some c1, Some c2) ->
            let (a1, r1, g1, b1) = ImgUtil.fromColor c1
            let (a2, r2, g2, b2) = ImgUtil.fromColor c2
            in Some(ImgUtil.fromArgb((a1 + a2) / 2, (r1 + r2) / 2, (g1 + g2) / 2, (b1 + b2) /2))

/// <summary> a function that uses the mix type figure to mix a circle and a rectangle </summary>
/// <returns> figure </returns>
let figTest = 
    let c = Circle ((50, 50), 45, red)
    let r = Rectangle ((40, 40), (90, 110), blue)
    Mix (c, r)

/// <summary> a function that colors each pixel in the image area to create an image</summary>
/// <param name = "filnavn"> string for filenaming </param name>
/// <param name = "figur"> figure </param name>
/// <param name = "b"> width represented as int </param name>
/// <param name = "h"> height represented as int </param name>
/// <returns> a png file </returns>
let makePicture (filnavn : string) (figur : figure) (b : int) (h : int) : unit = 
    let c = ImgUtil.mk b h
    List.iter (fun x -> 
        List.iter (fun y -> 
            let col =
                match colorAt (x, y) figur with
                | Some color -> color
                | None -> fromRgb (128, 128, 128)
            ImgUtil.setPixel col (x, y) c) [0..h]
            ) [0..b]

    // for x in [0..b] do
    //     for y in [0..h] do
    //         let col =
    //             match colorAt (x, y) figur with
    //             | Some color -> color
    //             | None -> fromRgb (128, 128, 128)
    //         ImgUtil.setPixel col (x, y) c
    ImgUtil.toPngFile (filnavn + ".png") c

/// <summary> a function for making sure the figure is legal </summary>
/// <param name = "figur"> figure </param name>
/// <returns> bool </returns>
let rec checkFigure (figur : figure) : bool =
    match figur with
    | Circle ((x, y), r, col) when r >= 0 -> true
    | Rectangle ((x0, y0), (x1, y1), col) when x0 <= x1 && y0 <= y1 -> true 
    | Mix (x, y) when checkFigure x = true && checkFigure y = true -> true
    | _ -> false

/// <summary> a function for moving the figure with a vector </summary>
/// <param name = "figur"> figure </param name>
/// <param name = "p"> point representing vector </param name>
/// <returns> figure </returns>
let rec move (figur : figure) (p : point) : figure =
    let dotp (x0, y0) (x1, y1) : point = (x0 + x1, y0 + y1)
    match figur with
    | Circle ((x, y), r, col) -> Circle ((dotp (x, y) p), r, col)
    | Rectangle ((x0, y0), (x1, y1), col) -> Rectangle ((dotp (x0, y0) p), (dotp (x1, y1) p), col)
    | Mix (x, y) -> Mix (move x p, move y p)

/// <summary> a function that creates two point for a rectangle that covers the entire image </summary>
/// <param name = "figur"> figure </param name>
/// <returns> point * point </returns>
let rec boundingBox (figur : figure) : (point * point) = 
    match figur with
    | Circle ((x, y), r, col) -> ((x - r, y - r), (x + r, y + r))
    | Rectangle ((x0, y0), (x1, y1), col) -> ((x0, y0), (x1, y1))
    | Mix (x, y) -> 
        let (a1, b1), (a2, b2) = boundingBox x
        let (x1, y1), (x2, y2) = boundingBox y
        ((min a1 x1, min b1 y1),(max a2 x2, max b2 y2))


makePicture "figtest" figTest 100 150
makePicture "moveTest" (move figTest (-20,20)) 100 150
printfn "%b" (checkFigure figTest) // remove
printfn "%A" (boundingBox figTest) // remove

     
