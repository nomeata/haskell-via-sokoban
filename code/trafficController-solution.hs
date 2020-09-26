{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

lightBulb :: Color -> Double ->  Picture
lightBulb c dx = colored c (translated 0 dx (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

trafficLight :: Color -> Color -> Color -> Picture
trafficLight c1 c2 c3 =
  lightBulb c1 2.5 &
  lightBulb c2 0   &
  lightBulb c3 (-2.5) &
  frame

trafficController :: Integer -> Picture
trafficController s
  | s >= 0 && s <= 2 = trafficLight black black  green
  | s == 3           = trafficLight black yellow black
  | s >= 4 && s <= 6 = trafficLight red   black  black
  | otherwise        = trafficLight red   yellow black


trafficLightAnimation :: Double -> Picture
trafficLightAnimation t = trafficController (round t `mod` 8)

main :: IO ()
main = animationOf trafficLightAnimation
