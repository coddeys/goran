module Dice where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Mouse
import Signal exposing (..)
import Window
import List exposing (..)
import Time exposing (..)
import Random exposing (..)

main = Signal.map show <| randomFloat <~ (round <~ (every <| second * 3))

randomFloat n =
  let seed = Random.initialSeed n in
  fst (Random.generate (Random.float 0 1) seed)

