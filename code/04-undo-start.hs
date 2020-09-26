{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Lists

data List a = Empty | Entry a (List a) deriving Eq

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

allList :: List Bool -> Bool
allList Empty = True
allList (Entry b bs) = b && allList bs

-- Coordinates


data Coord = C Integer Integer deriving Eq

data Direction = R | U | L | D deriving Eq

eqCoord :: Coord -> Coord -> Bool
eqCoord (C x1 y1) (C x2 y2) = x1 == x2 && y1 == y2

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo c1 c2 c | c1 `eqCoord` c = c2
                   | otherwise      = c


-- The maze

data Tile = Wall | Ground | Storage | Box | Blank

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze c = case maze c of
  Box -> Ground
  t   -> t

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes Empty c'        = noBoxMaze  c'
mazeWithBoxes (Entry c cs) c'
  | eqCoord c c' = Box
  | otherwise  = mazeWithBoxes cs c'

-- The state

data State = State Coord Direction (List Coord) deriving Eq


initialBoxes :: List Coord
initialBoxes = go (-10) (-10)
  where
    go 11 11 = Empty
    go x  11 = go (x+1) (-10)
    go x  y  = case maze (C x y) of
      Box -> Entry (C x y) (go x (y+1))
      _   ->                go x (y+1)

initialState :: State
initialState = State (C 0 1) R initialBoxes

-- Event handling

tryGoTo :: State -> Direction -> State
tryGoTo (State from _ bx) d
  = case currentMaze to of
    Box -> case currentMaze beyond of
      Ground -> movedState
      Storage -> movedState
      _ -> didn'tMove
    Ground -> movedState
    Storage -> movedState
    _ -> didn'tMove
  where to          = adjacentCoord d from
        beyond      = adjacentCoord d to
        currentMaze = mazeWithBoxes bx
        movedState  = State to d movedBx
        movedBx     = mapList (moveFromTo to beyond) bx
        didn'tMove  = State from d bx -- Yes, ' may be part of an identifier

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
    | key == "Right" = tryGoTo s R
    | key == "Up"    = tryGoTo s U
    | key == "Left"  = tryGoTo s L
    | key == "Down"  = tryGoTo s D
handleEvent _ s      = s

-- Drawing

wall, ground, storage, box :: Picture
wall =    colored grey   (solidRectangle 1 1)
ground =  colored yellow (solidRectangle 1 1)
storage = colored white (solidCircle 0.3) & ground
box =     colored brown  (solidRectangle 1 1)

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
drawTileAt c = atCoord c (drawTile (noBoxMaze c))


atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic


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


pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

drawState :: State -> Picture
drawState (State c d boxes)
  = atCoord c (player d)
  & pictureOfBoxes boxes
  & pictureOfMaze

-- The complete activity

sokoban :: Activity State
sokoban = Activity initialState handleEvent drawState

-- The general activity type

data Activity world = Activity
        world
        (Event -> world -> world)
        (world -> Picture)


runActivity :: Activity s -> IO ()
runActivity (Activity state0 handle draw)
  = activityOf state0 handle draw

-- Resetable activities

resetable :: Activity s -> Activity s
resetable (Activity state0 handle draw)
  = Activity state0 handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

data SSState world = StartScreen | Running world

instance Eq s => Eq (SSState s) where
  StartScreen == StartScreen = True
  Running s == Running s' = s == s'
  _ == _ = False

withStartScreen :: Activity s  -> Activity (SSState s)
withStartScreen (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen | key == " " = Running state0
    handle' _              StartScreen              = StartScreen
    handle' e              (Running s)              = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

data WithUndo a = WithUndo a (List a)

-- Undo ability

withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = WithUndo state0 Empty

    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of Entry s' stack' -> WithUndo s' stack'
                      Empty           -> WithUndo s Empty
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (Entry s stack)
      where s' = handle e s

    draw' (WithUndo s _) = draw s

-- The main function

main :: IO ()
main = runActivity (resetable (withUndo (withStartScreen sokoban)))
