module HW4 exposing (..)

--E1

type Op = LD Int | ADD | MULT | DUP | DEC | SWAP | POP Int
type alias Prog = List Op
type alias Rank = Int
type alias OpRank = (Int,Int)

rankOp : Op -> OpRank
rankOp  o = case o of
    LD i -> (0, 1)
    ADD -> (2, 1)
    MULT -> (2, 1)
    DUP -> (1, 2)
    DEC -> (1, 1)
    SWAP -> (2, 2)
    POP i -> (i, 0)

rank : Prog -> Rank -> Maybe Rank
rank p r = case p of
    op::ops -> let rop = rankOp op in 
        let rchng = (Tuple.second rop) - (Tuple.first rop) in
            if (rchng + r < 0) then Nothing else (rank ops (rchng + r))
    [] -> Just r

rankP : Prog -> Maybe Rank
rankP p = rank p 0

type alias Stack = List Int

semOp : Op -> Stack -> Stack
semOp c l = case (c,l) of
    (LD i,s) -> i::s
    (DUP, i::s) -> i::i::s
    (ADD, i::j::s) -> i+j::s
    (MULT,i::j::s) -> i*j::s
    (DEC, i::s) -> (i - 1)::s
    (SWAP, i::j::s) -> j::i::s
    (POP n, i::s) -> if (n == 0) then i::s else (semOp (POP (n - 1)) s)
    _              -> l

semProg : Prog -> Stack -> Stack
semProg p s = case p of
    [] -> s
    o::os -> semProg os (semOp o s)

semTC:  Prog -> Maybe Stack
semTC p = case (rankP p) of 
    Nothing -> Nothing
    Just r -> Just (semProg p [])

--E2

type Shape = X | TD Shape Shape | LR Shape Shape
type alias BBox = (Int,Int)

bbox : Shape -> BBox
bbox s = case s of 
    X -> (1, 1)
    TD s1 s2 -> 
        let 
            (x1, y1) = bbox s1
            (x2, y2) = bbox s2
        in 
            let 
                x = max x1 x2
                y = y1 + y2
            in
                (x, y)
    LR s1 s2 ->
        let 
            (x1, y1) = bbox s1
            (x2, y2) = bbox s2
        in 
            let 
                x = x1 +  x2
                y = max y1 y2
            in
                (x, y)

countS : Shape -> Int
countS s = case s of
    X -> 1
    TD s1 s2 -> (countS s1) + (countS s2)
    LR s1 s2 -> (countS s1) + (countS s2)

rect : Shape -> Maybe BBox
rect s = 
    let
        bbx = bbox s
    in 
        let 
            bx = Tuple.first bbx
            by = Tuple.second bbx
        in
            if (bx * by == (countS s)) then (Just bbx) else Nothing