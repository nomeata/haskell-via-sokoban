{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

tree :: Picture -> Integer -> Picture
tree b 0 = b
tree b n = translated 0 1 (rotated (pi/10) (tree b (n-1)) & rotated (- pi/10) (tree b (n-1))) &
           polyline [(0,0),(0,1)]

blossom :: Double -> Picture
blossom t = colored yellow (solidCircle ((min t 10)/50))

myAnimation :: Double -> Picture
myAnimation t = tree (blossom t) 8

main :: IO ()
main = animationOf myAnimation
