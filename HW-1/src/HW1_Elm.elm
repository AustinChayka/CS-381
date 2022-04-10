module HW1_Elm exposing (..)

import HW1_Def exposing (..)

--Part 1

ins : a -> Bag a -> Bag a
ins x b = case b of
    [] -> (x, 1)::[]
    (y, n)::ys -> if y == x then ((y, n + 1)::ys) else (y, n)::(ins x ys)


del : a -> Bag a -> Bag a
del x b = case b of
    (bx, n)::bs -> if bx == x then (if n == 1 then del x bs else (bx, n - 1)::(del x bs)) else (bx, n)::(del x bs)
    [] -> []

bag : List a -> Bag a
bag l = case l of
    [] -> []
    x::xs -> ins x (bag xs)

subitem : (a, Int) -> Bag a -> Bool
subitem (x, n) b = case b of
    [] -> False
    (y, m)::ys -> if (y == x && n <= m) then True else subitem (x, n) ys
    
subbag : Bag a -> Bag a -> Bool
subbag b1 b2 = case b1 of 
    [] -> True
    (x, n)::xs -> (subitem (x, n) b2) && (subbag xs b2)

isSet : Bag a -> Bool
isSet b =
    (filter ((==) 1) (map snd b)) == []

size : Bag a -> Int
size b =
    sum (map snd b)

-- Part 2

nodes : Graph -> List Node
nodes g = asSet((map fst g) ++ (map snd g))


suc : Node -> Graph -> List Node
suc n g = g
    |> filter (\e -> (fst e) == n)
    |> map snd

detach : Node -> Graph -> Graph
detach n g = case g of
    [] -> []
    (n1, n2)::nx -> if (n1 == n || n2 == n) then (detach n nx) else (n1, n2)::(detach n nx)

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