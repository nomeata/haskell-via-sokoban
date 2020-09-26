{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

wall, ground, storage, box :: Picture
wall =    colored grey   (solidRectangle 1 1)
ground =  colored yellow (solidRectangle 1 1)
storage = colored white (solidCircle 0.3) & ground
box =     colored brown  (solidRectangle 1 1)

data Tile = Wall | Ground | Storage | Box | Blank

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (maze c))
         
maze :: Coord -> Tile 
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

data Direction = R | U | L | D

data Coord = C Integer Integer

initialCoord :: Coord
initialCoord = C 0 0

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

-- Exercise 1

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) c
    | key == "Right" = tryGoTo c R
    | key == "Up"    = tryGoTo c U
    | key == "Left"  = tryGoTo c L
    | key == "Down"  = tryGoTo c D
    | otherwise      = c
handleEvent _ c = c

isOk :: Tile -> Bool
isOk Ground = True
isOk Storage = True
isOk _ = False

player :: Direction -> Picture
player R = translated 0 0.3 cranium
          & polyline [(0,0),(0.3,0.05)] 
          & polyline [(0,0),(0.3,-0.05)] 
          & polyline [(0,-0.2),(0,0.1)] 
          & polyline [(0,-0.2),(0.1,-0.5)]
          & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player L = scaled (-1) 1 (player R) -- Cunning!
player U = translated 0 0.3 cranium
          & polyline [(0,0),(0.3,0.05)] 
          & polyline [(0,0),(-0.3,0.05)] 
          & polyline [(0,-0.2),(0,0.1)] 
          & polyline [(0,-0.2),(0.1,-0.5)]
          & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player D = translated 0 0.3 cranium
          & polyline [(0,0),(0.3,-0.05)] 
          & polyline [(0,0),(-0.3,-0.05)] 
          & polyline [(0,-0.2),(0,0.1)] 
          & polyline [(0,-0.2),(0.1,-0.5)]
          & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)

data State = State Coord Direction

initialState :: State
initialState = State (C 0 1) R

tryGoTo :: State -> Direction -> State
tryGoTo (State from _) d
  | isOk (maze to) = State to d
  | otherwise      = State from d
  where to = adjacentCoord d from

drawState :: State -> Picture
drawState (State c d) = atCoord c (player d) & pictureOfMaze

resetableActivityOf ::
    world ->
    (Event -> world -> world) ->
    (world -> Picture) ->
    IO ()
resetableActivityOf state0 handle draw
  = activityOf state0 handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

main :: IO ()
main = resetableActivityOf initialState handleEvent drawState
