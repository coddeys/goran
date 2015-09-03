import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Mouse
import Signal exposing (..)
import Window
import List exposing (..)
import Time exposing (..)
import Random exposing (..)
import Text exposing (..)

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
              , vel = (0, -100)
              , rad = 15
              , col = lightRed }

defaultPlayer = { defaultPill | pos <- (0,0)
                                ,col <- black }

type alias Game = {player:Pill, pills:(List Pill) }

defaultGame = { player = defaultPlayer,
                pills  = [] }

newPill : Float -> Color -> Pill
newPill x  col = { defaultPill | pos <- (x, hHeight)
                               , col <- col }

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

tf : Float -> Float -> String -> Form
tf y scl str = Text.fromString str |> Text.color gray
                          |> centered
                          |> toForm
                          |> scale scl
                          |> move (0,y)

render : (Int, Int) -> Game -> Element
render (w, h) g =
  let formPill {rad, col, pos} = circle rad |> filled col
                                            |> move pos
      txt   = tf 0 2 "Dima"
      forms = txt :: (List.map formPill <| g.player :: g.pills)
  in Graphics.Element.color lightGray
       <| container w h middle
       <| Graphics.Element.color white
       <| collage width height forms

delta = (fps 30)
input = (,) <~ Signal.map inSeconds delta
             ~ sampleOn delta (Signal.map2 relativeMouse (Signal.map center Window.dimensions) Mouse.position)

randomFloat n =
  let seed = Random.initialSeed n in
  fst (Random.generate (Random.float 0 1) seed)

rand fn sig = Signal.map fn (randomFloat <~ sig)

randX   = rand (\r -> (width * r) - hWidth)
randCol = rand (\r -> if r < 0.1 then lightBlue else defaultPill.col)

interval = round <~ (every (second * 2))


event =
  Signal.merge
          (Signal.map Tick input)
          (Signal.map2 (\x col -> Add (newPill x col)) (randX interval) (randCol interval))
                            
main : Signal Element
main = render <~ Window.dimensions ~ foldp stepGame defaultGame event


