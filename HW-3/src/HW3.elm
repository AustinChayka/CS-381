module HW3 exposing (..)

type Op = LD Int | ADD | MULT | DUP
type alias Prog = List Op
type alias Stack = List Int

type Result = S Stack | Error
type alias D = Result -> Result

semOp : Op -> D
semOp op = \s -> case op of
    LD n -> case s of 
        S xs -> S (n::xs)
        _ -> Error
    ADD -> case s of
        S (x1::x2::xs) -> S ((x1 + x2)::xs)
        _ -> Error
    MULT -> case s of
        S (x1::x2::xs) -> S ((x1 * x2)::xs)
        _ -> Error
    DUP -> case s of
        S (x::xs) -> S (x::(x::xs))
        _ -> Error

semProg: Prog -> D
semProg o = \s -> case o of
    op::ops -> semProg ops (semOp op s)
    [] -> s