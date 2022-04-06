module HW1_Elm exposing (..)

import HW1_Def exposing (..)

--Part 1
{--
ins : a -> Bag a -> Bag a
ins n b = 
    if List.member n (map fst b) then
        map (\(x, n2) -> if n2 == n then (x, n2 + 1) else (x, n2)) b
    else
        (n, 1)::b

del : a -> Bag a -> Bag a
del x b = map (\(x2, n) -> if x2 == x then (if n > 1 then (x2, n - 1)) else (x2, n)) b

bag : List a -> Bag a
bag l = case l of
--}

subbag : Bag a -> Bag a -> Bool
subbag b1 b2 = 
    map (\(x1, n1) -> ) b2

isSet : Bag a -> Bool
isSet b =
    (filter ((==) 1) (map snd b)) == []

size : Bag a -> Int
size b =
    sum (map snd b)

-- Part 2

--nodes : Graph -> List Node

suc : Node -> Graph -> List Node
suc n g = g
    |> filter (\e -> (fst e) == n)
    |> map snd

--detach : Node -> Graph -> Graph

-- Part 3

width : Shape -> Length
width s = case s of
    Pt p -> 0
    Circle p l -> l * 2
    Rect p w l -> w

bbox : Shape -> BBox
bbox s = case s of
    Pt (x, y) -> ((x, y), (x, y))
    Circle (x, y) l -> ((x - l, y - l), (x + l, y + l))
    Rect (x, y) w l -> ((x, y), (x + w, y + l))

minX : Shape -> Number
minX s = case s of 
    Pt (x, y) -> x
    Circle (x, y) l -> x - l
    Rect (x, y) w l -> x

addPoint : Point -> Point -> Point
addPoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 

move : Point -> Shape -> Shape
move p s = case s of
    Pt pp -> Pt (addPoint pp p)
    Circle cp l -> Circle (addPoint cp p) l
    Rect rp w l -> Rect (addPoint rp p) w l