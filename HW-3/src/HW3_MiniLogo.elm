module HW3_MiniLogo exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)

-- Elm/SVG auxiliary definitions
--
scale = 30

xPt : Int -> String
xPt i = String.fromInt (scale+i*scale)

yPt : Int -> String
yPt i = String.fromInt (398-i*scale)

svgLine : Line -> Svg a
svgLine ((a,b),(c,d)) = line
    [ x1 (xPt a), y1 (yPt b), x2 (xPt c), y2 (yPt d)
    , stroke "green", strokeWidth "4", strokeLinecap "round"] []

main : Html msg
main = svg [viewBox "0 0 400 400", width "800", height "800"]
           (List.map svgLine logoResult)

----- BEGIN HW3 solution

type alias Point = (Int,Int)

type Cmd = Pen Mode
         | MoveTo Point
         | Seq Cmd Cmd

type Mode = Up | Down

type alias Line = (Point,Point)
type alias Lines = List Line
type alias State = (Mode,Point)

semCmd : Cmd -> State -> (State,Lines)
semCmd c = \(sm, (sx, sy)) -> case c of 
    Pen m -> ((m, (sx, sy)), [])
    MoveTo (x, y) -> case sm of
        Up -> ((sm, (x, y)), [])
        Down -> ((sm, (x, y)), [((sx, sy), (x, y))])
    Seq c1 c2 -> let (s1, ls1) = semCmd c1 (sm, (sx, sy)) in
        let (s2, ls2) = semCmd c2 s1 in
            (s2, ls2 ++ ls1)

lines : Cmd -> Lines
lines c = Tuple.second (semCmd c (Up, (0, 0)))

logoResult : Lines
logoResult = lines (Seq (Seq (Seq (Pen Up) (Seq (MoveTo (0,0)) (Seq (Pen Down) (MoveTo (0,1))))) (MoveTo (1,1))) (Seq (MoveTo (1,2)) (MoveTo (2,2))))
