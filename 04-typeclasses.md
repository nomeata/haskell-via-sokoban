Type classes
============

In the last set of exercises, you have developed a working implementation of Sokoban. The resulting code might look like [this example solution](EDIT(code/03ex-complete.hs)).

While implementing it, some of you might have wished to be able to use the equality operator (`(==)`) on your own data types, such as `Coord` or `Tile`. But you could not and had to work around it using `case` expressions, helper functions or functions like `eqCoord`.

But you may have already observed that `(==)` can be used with multiple types: `Integer`, `Double`, `Bool`. (Although you should not use it with `Double` – at least not before you have read [What Every Computer Scientist Should Know About Floating-Point Arithmetic](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)!). And I claimed that `Bool` is not special. So there must be a way of using `(==)` with, say, `Coord`.

Maybe the error message that we see if we try to use it, can shed some light on this:
```
No instance for (Eq Coord) arising from a use of ‘==’
```

It seems that we need some kind of instance. Before we talk about instances, though, we have to talk about classes.

You can use the Hoogle search engine to [search for `==`](https://hoogle.haskell.org/?hoogle=%3D%3D), and the first entry will tell you about the type of the `(==)` operator:
```
Prelude> :t (==)
(==) :: Eq a => a -> a -> Bool
```
On the right of the `=>` arrow, we have the function type that we know: The operator takes two arguments of some type, and returns a boolean. On the left of the `=>` arrow, we have a *type class constraint*. This says that `(==)` can only be used on types that are members of this type class.

The Eq type class
-----------------

If you click on that link, you’ll reach the the documentation of
[the `Eq` class](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#t:Eq). Among other things, you’ll learn about the type class head, namely
```
class Eq a where
```
and its methods, namely
```
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

The documentation obscures this a bit, but if you put the two code snippets above together you get what you would write to define such a class yourself.

Note that methods are given with *just* their type signature – the
implementation will be in the class instances.  Also, the type is given without
the `Eq` constraint that you saw in the type signature for `(==)` earlier; this is added automatically.

The documentation also shows that there are many many _instances_ for this class:
```
instance Eq Bool
instance Eq Char
instance Eq Double
…
instance Eq a => Eq [a]
…
```

The `Eq Coord` instance
------------------------

Now that we know which methods the class has, we can write our own instance for `Coord`:
```haskell
instance Eq Coord where
  C x1 y1 == C x2 y2 = x1 == x2 && y1 == y2
  c1 /= c2 = not (c1 == c2)
```
So after the `instance` keyword, we name the type class that we want to instantiate and the type for which we want to define an instance. Then, after `where` and indented, we give function definitions for the methods. We can use multiple patterns, guards, and all that as usual. We do *not* give a type signature, because the type of these methods is already determined by the class definition and the *instance head*.

Now our code compiles again.

Default implementations
-----------------------

Note that I was lazy and did not really implement `(/=)`; I just referred to the implementation of `(==)`. Obviously, that is fair game for *every* instance of `Eq` that one might want to implement.

Therefore, the `Eq` class already comes with a *default implementation* of `(/=)` in terms of `(==)` and we can simply skip the definition in our code.

How do we know that we can leave out `(/=)`? We can try (the compiler will warn us about missing instances), or we can check out [the documentation](http://hackage.haskell.org/package/base/docs/Prelude.html#t:Eq) or [the source](http://hackage.haskell.org/package/ghc-prim-0.6.1/docs/src/GHC.Classes.html#Eq) (which are a bit harder to find for `Eq` because it is so basic).

By making a function a method of the class with an default implementation, the authors of an instance have the option of implementing it (for example if a more efficient implementation is possible), but they do not have to.

The `Eq Tile` instance
------------------------

So great: We can now use `==` on `Coords`. We also want it on `Tiles`. That is easy to write:

```haskell
instance Eq Tile where
  Wall == Wall = True
  Ground == Ground = True
  Storage == Storage = True
  Box == Box = True
  Blank == Blank = True
  _ == _ = False
```
Now that works, but I am sure you can immediately tell me why this is not
satisfying: That is a lot of code to write, it is repetitive, and if we add
another constructor to `Tile`, the code will be wrong.

Luckily, at least for some of the basic type classes, the compiler can write the instance for us. We just have to instruct it to:

```haskell
data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
```
(If are are playing around with this locally, then pass `-ddump-deriv` to the compiler to see that it actually creates the same code that we wrote above.)

Equality is not for everyone
----------------------------

So great, we get to use `(==)` on all our types! All our types? Unfortunately not. Last week we defined the type `Activity`. Some fields of this data type are functions, and equality on functions is, in general, undecidable. If we tried to use `deriving Eq` on `Activity`, we would get the error message
```
No instance for (Eq (world -> Picture))
```

Benefits of type classes
------------------------

So great, we get to use `(==)` on _almost_ all our types! This is also called *overloading*, and is a nice feature of type classes, but not the only and probably not the most important one. The important features are:

### Overloading of names

As just discussed.

### Laws!

When you see `(==)` and `(/=)`, you immediately assume that `a /= b`
is `True` if and only if `a == b` is `False`. By convention, every instance
of `(==)` fulfills this law, and the derived instances do as well. For other
type classes, the laws are more interesting, but that is a topic for some other time.

Note that such laws are not checked by the compiler (it cannot do that, in
general), and nothing is stopping you from implementing a bogus instance.
But if you do, do not complain if things break in weird ways.

### Generic algorithms

Have a look at the definition of `moveFromTo`:
```haskell
moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo c1 c2 c | c1 == c   = c2
                   | otherwise = c
```

There is nothing `Coord`-specific going on any more! And indeed, if we remove the type signature, and let the compiler propose one to us, we get the suggestion to write
```haskell
moveFromTo :: Eq a => a -> a -> a -> a
moveFromTo c1 c2 c | c1 == c   = c2
                   | otherwise = c
```

This means that `moveFromTo` is a very generally usable function. We only write it once and can use it with many different types.

Now, this function is not very impressive, but we can implement very complex
functionality in a generic and reusable way this way, especially when the type class in question comes with laws (see above).

### Instance resolution

When using overloaded functions, the compiler has to find the relevant instance for the type you are using it. With polymorphic types, this instance might require another one (as we will see shortly). With some trickery and/or some language extensions supported by GHC this *instance resolution* process can be a powerful machinery that not only relieves you from writing some tedious code, but can actually solve some puzzling problems for you. In a way, it is a small logic programming language embedded in the type system.

I will demonstrate this in a moment, but let's continue with the general remarks first.

### Coherence

Haskell guarantees that for a particular type and a particular type class, there is at most one instance. If we would try to define `instance Eq Coord` again, the compiler will bark at us.

This means that the meaning of an overloaded function depends only on the concrete type it is used with, but not in what context it is used. This is used by the library implementation of search trees, which uses the `Ord` instance of the type of keys to build the tree, and this would go horribly wrong if you build the tree with one particular ordering, and then search in it using a completely different ordering.

Use case: Undo stacks
---------------------

Let us demonstrate how we can make use of instance resolution, by implementing generic undo functionality. When testing your solution, you surely moved your box onto the wall and wished you did not have to start over again. So let us implement this, in generic way, as a `Activity`-modifying function:

```haskell
data WithUndo a = WithUndo a (List a)

withUndo :: Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = WithUndo state0 Empty

    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of Entry s' stack' -> WithUndo s' stack'
                      Empty           -> WithUndo s Empty
    handle' e              (WithUndo s stack)
       = WithUndo (handle e s) (Entry s stack)

    draw' (WithUndo s _) = draw s
```

This code ([open on CodeWorld](EDIT(code/04-undo-bad.hs))) looks good, but if we we use it (by using `withUndo` in `main`), it does not seem to work. What went wrong?

The problem is that we push the state to the stack on every event. That includes mouse moves, that includes button releases. What we really would like to do is push a change only on to the stack if the event actually had an effect!

So we need to check that in the `handle'` function, before we push a new state onto the stack:

```haskell
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of Entry s' stack' -> WithUndo s' stack'
                      Empty           -> WithUndo s Empty
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (Entry s stack)
      where s' = handle e s
```

This does not work yet, the compiler complains:
```
No instance for (Eq a) arising from a use of ‘==’
```
which makes sense: In order to compare the state of the interaction we are wrapping, the state needs to be comparable! So we have to extend the type signature:
```haskell
withUndo :: Eq a => Activity a -> Activity (WithUndo a)
```

But the compiler is still not satisfied:
```
No instance for (Eq State) arising from a use of ‘withUndo’
```
We have not yet defined an `Eq` instance for `State`! But we know how to do that quickly: Using `deriving Eq`. This will, as one might expect, also ask for `Eq` instances for `Direction` and `List`, which we give the same way.

Yay, that works ([open on CodeWorld](EDIT(code/04-undo-with-eq.hs))):

RUN(code/04-undo-with-eq.hs)

Now what if we swap the use of `withUndo` and `withStartScreen`? We get an error message saying
```
No instance for (Eq (SSState State))
```
So we should give an `Eq` instance for `SSState`.  We could use `deriving`, but lets do it by hand to learn something.

We have introduced `withStartScreen`, and hence `SSState`, to be polymorphic and work with any possible wrapped state. So likewise, we do not want to write an instance just for `SSState State`, but rather `SSState s` for any type `s`. So lets write that:

```haskell
instance Eq (SSState s) where
  StartScreen == StartScreen = True
  Running s == Running s' = s == s'
  _ == _ = False
```

This does not work yet, we get an error message:
```
No instance for (Eq s) arising from a use of ‘==’
```

This makes sense: If the underlying state type does not support equality, then the extended type cannot do it either. But where do we add the constraint that this `Eq` instance does only work if `s` itself is a member of the `Eq` typeclass? We add this to the instance head:

```haskell
instance Eq s => Eq (SSState s) where
  StartScreen == StartScreen = True
  Running s == Running s' = s == s'
  _ == _ = False
```
This works, and now pressing U gets us back to the start screen. ([Try it on codeworld](EDIT(code/04-undo-start.hs))).

What is happening here? As we compose functions that transform `Activity`s, the type of the state of these interactions grows. And then, when there is a function like `withUndo` that has an `Eq` constraint, the compiler uses the existing instances of `Eq` to break this complex type down again, and this way constructs, on the spot, a way to equate values of this complex type.

Naturally, this example has been very small, but I hope it gave you an impression at what might be possible.

<!-- this state: https://code.world/haskell#PZ2vR0V_ZaWGU8XKe-54seA -->

**Question**: Are there other designs that do not require an explicit equality check to implement undo? Which one would you prefer?


Type classes vs. object-oriented classes
----------------------------------------

Many of you might have experience with object-oriented programming languages like Java. You are in danger! Do not fall in the trap of confusing type classes (in Haskell) with classes (in Java)! They are not very similar, and you do not use them to solve the same problems.

If anything, type classes correspond to *interfaces* in Java: Both contain methods without implementation and their type signatures, and instances provide the implementation.

Classes and objects as in Java do not have a direct correspondence in Haskell, and that is ok, because problems are approached differently. But the `Activity` type that we defined above week is, in some sense, an approximation of a class. The concrete `Activity`s that we defined are instances of this class, and functions like `withStartScreen` relate to inheritance (or maybe to the decorator pattern).

Other type classes you should know about
----------------------------------------

Besides `Eq`, you should know about these type classes:

 * [`Ord`](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#t:Ord),
   with methods like `(<=)`, `min` and others, for types that can be (totally)
   ordered. Can be derived.
 * Many numeric type classes:
    * [`Num`](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#t:Num),
      with methods like `(+)`, `(*)`, `abs` and `fromInteger`, for basic numeric
      types. Speaking mathematically, this type class captures the operations of a
      *ring*.  Instances for `Int`, `Integer`, `Double` and others.
    * [`Integral`](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#t:Integral)
      with methods like `div` and `mod`, for integral types that allow these operations.
      Instances for `Int`, `Integer`, and others.
    * [`Fractional`](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#t:Fractional)
      with method `(/)` for anything that can properly be divided. In particular, `Double`.
    * [`Floating`](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#t:Floating)
      with methods like `pi`, `exp`, `sin`, `(**)` which require floating point
      numbers. Instance for `Double`.
    * [`RealFrac`](http://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#t:RealFrac)
      with methods like `round`, `truncate` etc. that convert from floating point
      numbers to integral numbers.

