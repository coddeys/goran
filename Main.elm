import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Mouse
import Signal exposing (..)
import Window
import List exposing (..)
import Time exposing (..)
import Random exposing (..)

relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x,y) = (x - ox, -(y - oy))

(width, height) = (400, 400)
(hWidth, hHeight) = (width / 2, height / 2)

center : (Int, Int) -> (Int, Int)
center (w, h) = (w // 2, h // 2)

type alias Vec = ( Float, Float )

vecAdd : Vec -> Vec -> Vec
vecAdd (px,py) (vx,vy) = (px+vx, py+vy)

vecSub : Vec -> Vec -> Vec
vecSub (px,py) (vx,vy) = (px-vx, py-vy)

vecLen : Vec -> Float
vecLen (x, y) = sqrt (x*y + y*y) 

vecMulS : Vec -> Time -> Vec
vecMulS (x,y) t = (x*t, y*t)

type alias Pill = { pos:Vec, vel:Vec, rad:Float, col:Color }

defaultPill = { pos = (0, hHeight)
              , vel = (0, -30)
              , rad = 15
              , col = lightRed }

defaultPlayer = { defaultPill | pos <- (0,0)
                                ,col <- black }

type alias Game = {player:Pill, pills:(List Pill) }

defaultGame = { player = defaultPlayer,
                pills  = [] }

newPill : Float -> Pill
newPill x = { defaultPill | pos <- (x, hHeight) }

initPill : Signal Random.Seed
initPill =
  (\(time, _) -> Random.initialSeed (round time)) <~ Time.timestamp (Signal.constant ())

type Event = Tick (Time, (Int, Int)) | Add Pill

stepGame : Event -> Game -> Game
stepGame event ({player, pills} as g) =
  case event of
    Tick (t, mp) -> let hit pill  = (vecLen <| vecSub player.pos pill.pos) < player.rad + pill.rad
                        unculled  = List.filter (\pill -> (snd pill.pos > -hHeight)) pills
                        untouched = List.filter (not << hit) unculled
                    in { g | player <- stepPlayer mp player
                       , pills <- List.map (stepPill t) untouched }
    Add p        -> { g | pills <- p :: g.pills }

stepPlayer : (Int, Int) -> Pill -> Pill
stepPlayer (x, y) p = { p | pos <- (toFloat x, toFloat y) }

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos <| vecMulS p.vel t}

render : (Int, Int) -> Game -> Element
render (w, h) game =
  let formPill {rad, col, pos} = circle rad |> filled col
                                            |> move pos
      forms = formPill game.player :: List.map formPill game.pills
  in color lightGray
       <| container w h middle
       <| color white
       <| collage width height forms

delta = (fps 30)
input = (,) <~ Signal.map inSeconds delta
             ~ sampleOn delta (Signal.map2 relativeMouse (Signal.map center Window.dimensions) Mouse.position)

randx sig = let rnd = randomFloat <~ sig
                coord r = (width * r) - hWidth
            in Signal.map coord rnd

randomFloat n =
  let seed = Random.initialSeed n in
  fst (Random.generate (Random.float 0 1) seed)


event =
  Signal.merge
          (Signal.map Tick input)
          (Signal.map (Add << newPill) <| randx (round <~ (every (second * 3))))
                            
main : Signal Element
main = render <~ Window.dimensions ~ foldp stepGame defaultGame event


