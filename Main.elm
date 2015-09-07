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

-- CONFIG
speed      = 500
spawnInterval = 57 / speed
sizePill   = 15
sizePlayer = sizePill
             
relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x,y) = (x - ox, -(y - oy))

-- HELPER FUNCITONS
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

tf : Float -> Float -> String -> Form
tf y scl str = Text.fromString str |> Text.color gray
                          |> centered
                          |> toForm
                          |> scale scl
                          |> move (0,y)

-- INPUT
delta = (fps 30)
input = (,) <~ Signal.map inSeconds delta
             ~ sampleOn delta (Signal.map2 relativeMouse (Signal.map center Window.dimensions) Mouse.position)

randomFloat n =
  let seed = Random.initialSeed n in
  fst (Random.generate (Random.float 0 1) seed)

rand fn sig = Signal.map fn (randomFloat <~ sig)

randX   = rand (\r -> (width * r) - hWidth)
randCol = rand (\r -> if r < 0.1 then lightBlue else defaultPill.col)

interval = round <~ (every (second * spawnInterval))

-- MODEL
type alias Pill = { pos:Vec, vel:Vec, rad:Float, col:Color }

defaultPill = { pos = (0, hHeight)
              , vel = (0, -speed)
              , rad = sizePill
              , col = lightRed }

defaultPlayer = { defaultPill | pos <- (0, -hHeight - sizePlayer)
                                , rad <- sizePlayer
                                , col <- black }

type State = Start | Play | Over
type alias Game = {player:Pill, pills:(List Pill), score:Int, state: State }

defaultGame = { player = defaultPlayer
              , pills  = []
              , score = 0
              , state = Start }

newPill : Float -> Color -> Pill
newPill x  col = { defaultPill | pos <- (x, hHeight)
                               , col <- col }

initPill : Signal Random.Seed
initPill =
  (\(time, _) -> Random.initialSeed (round time)) <~ Time.timestamp (Signal.constant ())

-- UPDATE
type Event = Tick (Time, (Int, Int)) | Add Pill | Click

stepPlay : Event -> Game -> Game
stepPlay event g =
  case event of
    Tick (t, mp) -> let hit pill  = (vecLen <| vecSub g.player.pos pill.pos) < g.player.rad + pill.rad
                        unculled  = List.filter (\{pos} -> (snd pos > -hHeight)) g.pills
                        untouched = List.filter (not << hit) unculled
                        touched   = List.filter hit unculled
                        hitColor c = not <| isEmpty  <| List.filter (\{col} -> col == c) touched
                        hitBlue    = hitColor lightBlue 
                        hitRed     = hitColor lightRed 
                        out        = let (x,y) = mp in abs (toFloat x) > hWidth  || abs (toFloat y) > hHeight
                        g' = { g | player <- stepPlayer mp g.player
                             , pills <- List.map (stepPill t) untouched
                             , score <- if hitBlue then g.score + 1 else g.score }
                    in if hitRed || out then { defaultGame | score <- g'.score
                                                    , state <- Over } else g'
    Add p        -> { g | pills <- p :: g.pills }
    Click        -> g

click : Event -> Bool
click event =
  case event of
    Click -> True
    _     -> False

stepGame : Event -> Game -> Game
stepGame event ({state} as g) =
  let playGame = { defaultGame | state <- Play }
      toPlay  = if click event then playGame else g
  in case state of
       Play  -> stepPlay event g
       _     -> toPlay

stepPlayer : (Int, Int) -> Pill -> Pill
stepPlayer (x, y) p = { p | pos <- (toFloat x, toFloat y) }

stepPill : Time -> Pill -> Pill
stepPill t p = { p | pos <- vecAdd p.pos <| vecMulS p.vel t}


-- DISPLAY
render : (Int, Int) -> Game -> Element
render (w, h) g =
  let formPill {rad, col, pos} = circle rad |> filled col
                                            |> move pos
      txts   = case g.state of
                 Start -> [ tf  70 4 "Blue Pill"
                          , tf   0 2 "Click to Start" ]
                 Play  -> [ tf   0 4 (toString g.score) ]
                 Over  -> [ tf  70 4 "Game Over"
                          , tf   0 4 (toString g.score)
                          , tf -50 2 "Click to Restart" ]
      forms = txts ++ (List.map formPill <| g.player :: g.pills)
  in Graphics.Element.color lightGray
       <| container w h middle
       <| Graphics.Element.color white
       <| collage width height forms

event =
  Signal.mergeMany
          [ (Signal.map Tick input)
            , (Signal.map2 (\x col -> Add (newPill x col)) (randX interval) (randCol interval))
            , (Signal.map (\_ -> Click) Mouse.clicks) ]
                            
main : Signal Element
main = render <~ Window.dimensions ~ foldp stepGame defaultGame event


