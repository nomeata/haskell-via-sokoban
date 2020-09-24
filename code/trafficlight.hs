{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

botCircleGreen, topCircleRed, frame, trafficLight :: Picture
botCircleGreen = colored green (translated 0 (-1.5) (solidCircle 1))
topCircleRed   = colored red   (translated 0   1.5  (solidCircle 1))
frame = rectangle 2.5 5.5
trafficLight = botCircleGreen & topCircleRed & frame

ourPicture :: Picture
ourPicture = trafficLight

main :: IO ()
main = drawingOf ourPicture
