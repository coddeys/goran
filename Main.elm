import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Mouse
import Signal exposing (..)
import Window

main : Signal Element
main =
  Signal.map render <| relativeMouse (200, 200)<~ Mouse.position

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x,y) = (x - ox, -(y - oy))

center : (Int, Int) -> (Int, Int)
center (w, h) = (w // 2, h // 2)

render (x, y) =
  let forms = [circle 15 |> filled blue
                         |> move (toFloat x, toFloat y) ]
  in color gray <| collage 400 400 forms
