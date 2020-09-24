import CodeWorld

botCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-1.5) (solidCircle 1))
topCircle c = colored c (translated 0   1.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 5.5

trafficLight :: Bool -> Picture
trafficLight True  = botCircle green & topCircle black & frame
trafficLight False = botCircle black & topCircle red   & frame

spread :: Picture -> Double -> Integer -> Picture
spread _   _  0 = blank
spread pic dx n = pic & translated dx 0 (spread pic dx (n-1))
     
ourPicture :: Picture
ourPicture = spread (trafficLight True) 3 3

main :: IO ()
main = drawingOf ourPicture
