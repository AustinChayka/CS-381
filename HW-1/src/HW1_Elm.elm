module HW1_Elm exposing (..)

import HW1_Def exposing (..)

size : Bag a -> Int
size b = sum (map snd b)