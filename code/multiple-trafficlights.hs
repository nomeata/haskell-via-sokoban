{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

botCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-1.5) (solidCircle 1))
topCircle c = colored c (translated 0   1.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 5.5

trafficLight :: Bool -> Picture
trafficLight True  = botCircle green & topCircle black & frame
trafficLight False = botCircle black & topCircle red   & frame

lights :: Integer -> Picture
lights 0 = blank
lights n = trafficLight True & translated 3 0 (lights (n-1))

ourPicture :: Picture
ourPicture = lights 3

main :: IO ()
main = drawingOf ourPicture
