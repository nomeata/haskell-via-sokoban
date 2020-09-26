✍️ Exercises: Graph algorithms {.exercises}
===========================

Import the list of mazes
------------------------

The students of the CIS194 course have contributed mazes, and you can [download the
code of the mazes](../extras/Mazes.hs). It contains a list of mazes (`mazes`)
and a longer list including broken mazes (`extraMazes`). Paste them into your
file, at the very end.

Because the starting position is relevant, we added a data type to go along with the maze:

```haskell
data Maze = Maze Coord (Coord -> Tile)
mazes :: List Maze
mazes = …
extraMazes :: List Maze
extraMazes = …
```

More polymorphic list function
------------------------------

Implement these generally useful functions:
```haskell
elemList :: Eq a => a -> List a -> Bool
appendList :: List a -> List a -> List a
listLength :: List a -> Integer
filterList :: (a -> Bool) -> List a -> List a
nth :: List a -> Integer -> a
```
These should do what their names and types imply:

  * `elemList x xs` is `True` if and only if at least one entry in `xs` equals to `x`.
  * `appendList xs ys` should be the list containing the entries of `xs`
    followed by those of `ys`, in that order.
  * `listLength xs` should be the number of entries in `xs`.
  * `filterList p xs` should be the list containing those entries `x` of `xs`
    for which `p x` is true.
  * `nths xs n` extracts the $n$th entry of the list (start counting with 1).
    If $n$ is too large, you may abort the program (by writing `error "list too
    short"`, which is an expression that can be used at any type). This is not
    good style, but shall do for now.

Graph search
------------

(Read exercise 3 first, to understand why this is an interesting function.)

The algorithm you have to implement below can be phrased very generally, and we want it to be general. So implement a function
```haskell
isGraphClosed :: Eq a => a -> (a -> List a) -> (a -> Bool) -> Bool
```
so that in a call `isGraphClosed initial adjacent isOk`, where the parameters are

 * `initial`, an initial node,
 * `adjacent`, a function that for every node lists all walkable adjacent nodes and
 * `isOk`, which checks if the node is ok to have in the graph,

the function returns `True` if all reachable nodes are “ok” and `False` otherwise.

Note that the graph described by `adjacent` can have circles, and you do not want your program to keep running in circles. So you will have to remember what nodes you have already visted.

The algorithm follows quite naturally from handling the various cases in a local helper function `go` that takes two arguments, namely a list of seen nodes and a list of nodes that need to be handled. If the latter list is empty, you are done. If it is not empty, look at the first entry. Ignore it if you have seen it before. Otherwise, if it is not ok, you are also done. Otherwise, add its adjacent elements to the list of nodes to look at.

Check closedness of mazes
-------------------------

Write a function
```haskell
isClosed :: Maze -> Bool
```
that checks whether the maze is closed. A maze is closed if

 * the starting position is either `Ground` or `Storage` and
 * every reachable tile is either `Ground`, `Storage` or `Box`.

Use `isGraphClosed` to do the second check. Implement `adjacent` so that `isGraphClosed` walks everywhere where there is not a `Wall` (including `Blank`). Implement `isOk` so that `Blank` tiles are not ok.

You might find it helpful to define a list `allDirections :: List Direction` and use `mapList` and `filterList` when implementing `adjacent`.

<!--
With the following function you can visualize a list of booleans:

```haskell
pictureOfBools :: List Bool -> Picture
pictureOfBools xs = translated (-fromIntegral k /2) (fromIntegral k) (go 0 xs)
  where n = listLength xs
        k = findK 0 - - k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ Empty = blank
        go i (Entry b bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)
```

Let `exercise3 :: IO ()` be the visualization of `isClosed` applied to every element of `extraMazes`. Obviously, `mapList` wants to be used here.

<iframe width="400" height="400" src="https://code.world/run.html?mode=haskell&dhash=DcmaPotzuWThcbL2VAHBRMw"></iframe>
-->

Multi-Level Sokoban
-------------------

Extend your game from the last exericse (or start with [the example solution](EDIT(code/03ex-complete.hs)) to implement multi-level sokoban.

  * Extend the `State` with a field of type `Integer`, to indicate the current
    level (start counting at 1).
  * The initial state should start with level 1. The initial coordinate is
    obtainedr from the entry in `mazes`.
  * Your `handle` and `draw` functions will now need to take an additional
    argument, the current maze, of type `Coord -> Tile`, instead of refering to
    a top-level `maze` function. Any helper functions (e.g. `noBoxMaze`) will
    also have to take this as an argument. This requires many, but
    straight-forward changes to the code: You can mostly, without much
    thinking:

      * Check the compier errors for an affected function, say `foo`.
      * Add `(Coord -> Tile) ->` to the front of `foo`’s type signature,  .
      * Add a new first parameter `maze` to `foo`
      * Everywhere where `foo` is called, add `maze` as an argument.
      * Repeat.

    To get the current maze, use `nth` from exercise 1. Of course, make sure you
    never use `nth` with a too-short list. A variant `nthMaze :: Integer ->
    (Coord -> Tile)` that gets the maze component of the corresponding entry in
    `mazes` will also be handy whenever you have the `State`, but need a `maze
    :: Coord -> Tile`.

  * If the level is solved and the current level is not the last (use
    `listLength` from above) the space bar should load the next level.

    There is some code to be shared with the calculation of the initial state!
    Maybe the same function `loadLevel :: Integer -> State` can be used in both
    situations.
  * If the level is solved and the current leve is the last, show a differnt
    message (e.g. “All done” instead of “You won”).

Congratulations, you have now created a fully functional multi-level Sokoban game, which should look like this (and you can [look at the code here](EDIT(code/04ex-sokoban.hs))):

RUN(code/04ex-sokoban.hs)
