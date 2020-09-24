import CodeWorld

tree :: Integer -> Double -> Picture
tree 0 _ = blank
tree n f = path [(0,0),(0,1)] & translated 0 1 (
  rotated (f*pi/10) (tree (n-1) f) & rotated (- f*pi/10) (tree (n-1) f))

main :: IO ()
main = animationOf (tree 8 . sin)
