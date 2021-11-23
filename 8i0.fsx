open ImgUtil
open System

type point = int * int // a point (x, y) in the plane 
type color = ImgUtil.color
type figure =
    | Circle of point * int * color
        // defined by center , radius , and color
    | Rectangle of point * point * color
        // defined by corners top-left, bottom-right, and color
    | Mix of figure * figure
        // combine figures with mixed color at overlap

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

let figTest = 
    let c = Circle ((50, 50), 45, red)
    let r = Rectangle ((40, 40), (90, 110), blue)
    Mix (c, r)

let makePicture (filnavn : string) (figur : figure) (b : int) (h : int) : unit = 
    let c = ImgUtil.mk b h
    for i in 0..b do
        for j in 0..h do
            let col =
                match colorAt (i, j) figur with
                | Some color -> color
                | None -> fromRgb (128, 128, 128)
            ImgUtil.setPixel col (i, j) c
    ImgUtil.toPngFile (filnavn + ".png") c

makePicture "figtest" figTest 100 150

let rec checkFigure (figur : figure) : bool =
    match figur with
    | Circle ((x, y), r, col) when r > 0 -> true
    | Rectangle ((x0, y0), (x1, y1), col) when x0 < x1 && y0 < y1 -> true 
    | Mix (x, y) when checkFigure x = true && checkFigure y = true -> true
    | _ -> false

printfn "%b" (checkFigure figTest)

let rec move (figur : figure) (p : point) : figure =
    let dotp (x0, y0) (x1, y1) : point = (x0 + x1, y0 + y1)
    match figur with
    | Circle ((x, y), r, col) -> Circle ((dotp (x, y) p), r, col)
    | Rectangle ((x0, y0), (x1, y1), col) -> Rectangle ((dotp (x0, y0) p), (dotp (x1, y1) p), col)
    | Mix (x, y) -> Mix (move x p, move y p)

makePicture "moveTest" (move figTest (-20,20)) 100 150

//let boundingBox (figur : figure) : (point * point) = // finish
    
