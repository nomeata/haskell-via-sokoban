✍️ Exercises {.exercises}
=========

You are free to use the [code from above](EDIT(code/activity-keypress.hs)) and from your own
solutions to the first set of exercises. Some of the exercises below reference types and
functions from there. In particular, if you have nicely drawn tiles, you should
use them instead of my ugly ones.

You learned about local definitions and lambda expressions. Use them when
appropriate!

The small guy (or girl) moves
-----------------------------

We hope to have a complete game soon, so let us work towards that.

Create a value `player :: Picture` that draws your figure.

Create a value `main :: IO ()` that calls `activityOf` with suitable
arguments that:

 * the player is drawn on top of the maze,
 * it starts in a position where there is ground (you can hard-code that position),
 * the cursor keys move the figure around (while the maze stays in a fixed position),
 * the player moves only on tiles of type `Ground` and `Storage`. Trying to move it
   into any other position will simply leave it in place.

It might yield nicer code to change the type of `maze` to `Coord -> Tile`. If
you find that, do not hesitate to make that change.

RUN(code/ho-ex-1.hs)


Look the right way!
-------------------

This is a continuation of the previous exercise. We want the figure to look the way it is
going. So change the type of `player` to `player :: Direction -> Picture` that draws the
figure in four variants.

Then extend the code from above so that after the player has tried to move
in some direction, it looks that way.

Hint: Think about types first (e.g of your state), and then about the implementation.

RUN(code/ho-ex-2.hs)

Reset!
------

It would be nice to be able to start a game from the beginning. This is
generally useful functionality, no matter what the game, so let us implement it
generally.

Write a function
```haskell
resetableActivityOf ::
    world ->
    (Event -> world -> world) ->
    (world -> Picture) ->
    IO ()
```
which has the same type as `activityOf`.

This function will behave almost the same as `activityOf` (which it
internally uses, of course), but when `Esc` is pressed, this event is not
passed on, but rather the state of the program is reset to the given initial state.

Style hint: An idiomatic definition of `resetableActivityOf` does not require any other top-level definitions, but likely local functions and/or lambda expressions.

Let `exercise3` be like `exercise2`, but using `resetableActivityOf` instead
of `activityOf`.

RUN(code/ho-ex-3.hs)

New level (optional)
--------------------

Create your own maze. It should

 * fit within the screen (i.e. use coordinates from -10 to 10),
 * it should be connected (i.e. starting on a ground tile, and disregarding boxes, the player should be able to reach all ground tiles),
 * it should be closed (i.e. the player should not be able to reach blank tiles), and
 * it should be solvable (recollect [the rules](https://en.wikipedia.org/wiki/Sokoban#Rules) if necessary).
