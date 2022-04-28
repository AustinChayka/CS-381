module HW2 exposing (..)
import List

-- Part 1

type alias Name = String
type alias Num = Int

type Mode = Up | Down
type Pos = PosNum Num | PosName Name
type Pars = ParN Name Pars | Par0 Name
type Vals = ValN Num Vals | Val0 Vals

type Cmd = Pen Mode
    | Moveto (Pos, Pos)
    | Def Name Pars Cmd
    | Call Name Vals
    | Seq Cmd Cmd

vector = Def "vector" (ParN "x1" (ParN "y1" (ParN "x2" (Par0 "y2")))) (Seq (Seq (Moveto (PosName "x1", PosName "y1")) (Pen Down)) (Seq (Moveto (PosName "x2", PosName "y2")) (Pen Up)))

steps : Int -> Cmd
steps n = case n of
    1 -> Seq (Pen Up) (Seq (Moveto (PosNum 0, PosNum 0)) (Seq (Pen Down) (Seq (Moveto (PosNum 0, PosNum 1)) (Moveto (PosNum 1, PosNum 1)))))
    _ -> Seq (Seq (steps (n - 1)) (Seq (Moveto (PosNum (n - 1), PosNum n)) (Moveto (PosNum n, PosNum n)))) (Pen Up)

-- Part 2

type alias NonTerm = String
type alias Term = String

type Symbol = Nt NonTerm | T Term
type Rhs = R (List Symbol)
type Prod = P NonTerm (List Rhs)
type Grammar = G (List Prod)

imp : Grammar
imp = G [P "cond" [(R [(T "T")]), (R [(T "not"), (Nt "cond")]), (R [(T "("), (Nt "cond"), (T ")")])], P "stmt" [(R [(T "skip")]), (R [(T "while"), (Nt "cond"), (T "do"), (T "{"), (Nt "stmt"), (T "}")]), (R [(Nt "stmt"), (T ";"), (Nt "stmt")])]]

symbolsFindNonterm : List Symbol -> List NonTerm
symbolsFindNonterm sym = case sym of
    [] -> []
    x::xs -> case x of
        Nt nonterm -> nonterm :: (symbolsFindNonterm xs)
        T term -> symbolsFindNonterm xs

symbolsFindTerm : List Symbol -> List Term
symbolsFindTerm sym = case sym of
    [] -> []
    x::xs -> case x of
        Nt nonterm -> symbolsFindTerm xs
        T term -> term :: (symbolsFindTerm xs)

rmdup : List a -> List a
rmdup la = case la of
    [] -> []
    x::xs -> if (List.member x xs) then (rmdup xs) else x::(rmdup xs)

nonterminals : Grammar -> List NonTerm
nonterminals (G p) = rmdup (List.concat (List.map (\(P nt rhs) -> nt :: (List.concat (List.map (\(R ls) -> symbolsFindNonterm ls) rhs))) p))

terminals : Grammar -> List Term
terminals (G p) = rmdup (List.concat (List.map (\(P nt rhs) -> (List.concat (List.map (\(R ls) -> symbolsFindTerm ls) rhs))) p))