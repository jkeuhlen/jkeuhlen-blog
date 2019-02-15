---
layout: post
title: "Put Arrow in your Quiver"
date: 2019-02-15
tags: haskell arrows introductions
id: 3
---

# What is an Arrow? 

First off, let's talk about what it means if something is an arrow. 

In general, an arrow is a generalization of a function. It can also be thought of as another abstraction model for thinking about computations, but the first idea is easier to grab onto for many people. Let's start with functions and build up from there! 

When thinking about a function `f(x) = x + 1`, we can go back to the grade-school model of functions as boxes of computation. 

`x ===> | + 1 | ===> result `

As it turns out, this is a great way to think about arrows as well! An arrow is a description of the entire block of computation. In other words, it encapsulates the input, the body, and output of a function. In Haskell, the simplest type that has an `Arrow` instance is the function type constructor `(->)`. We'll come back to this in more detail later. 

As a Haskell package, [`Control.Arrow`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Arrow.html) contains everything you need to know about the [`Arrow`](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Control.Arrow.html#Arrow) typeclass:

```haskell
class Category a => Arrow a where
    {-# MINIMAL arr, (first | (***)) #-}

    -- | Lift a function to an arrow.
    arr :: (b -> c) -> a b c

    -- | Send the first component of the input through the argument
    --   arrow, and copy the rest unchanged to the output.
    first :: a b c -> a (b,d) (c,d)
    first = (*** id)

    -- | A mirror image of 'first'.
    --
    --   The default definition may be overridden with a more efficient
    --   version if desired.
    second :: a b c -> a (d,b) (d,c)
    second = (id ***)

    -- | Split the input between the two argument arrows and combine
    --   their output.  Note that this is in general not a functor.
    --
    --   The default definition may be overridden with a more efficient
    --   version if desired.
    (***) :: a b c -> a b' c' -> a (b,b') (c,c')
    f *** g = first f >>> arr swap >>> first g >>> arr swap
      where swap ~(x,y) = (y,x)

    -- | Fanout: send the input to both argument arrows and combine
    --   their output.
    --
    --   The default definition may be overridden with a more efficient
    --   version if desired.
    (&&&) :: a b c -> a b c' -> a b (c,c')
    f &&& g = arr (\b -> (b,b)) >>> f *** g
```


So if an arrow is *just* a function, why is it useful? Well first, in most cases it isn't *just* a function. It's a generalization that encapsulates more information and allows for data annotation. Second, arrows have helpful combinators that can simplify many patterns that arise with regular functions. 

# Encapsulating Effects 

So far, we've focused on arrows as a pure functional block; however, there is a special kind of arrow that can be used to encapsulate monads as well. In category theory, Kleisli arrows are a category of arrows that also include effectful computations. In Haskell, you can think of them as a combination of a monad and an arrow. 

```haskell
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }
```

You could also think of it as an arrow promotion of the monadic function `Monad m => a -> m b`. The key difference is that the arrow laws allow for easy combination of these arrows in any context, where the monadic function could be difficult to combine. 

These arrows are critical for deeper abstractions around programmatic assembly of effects. I've talked about the design pattern behind that [elsewhere]({{ site.baseurl }}{% link about/index.html %}). 

Just like the monad's `do` syntactic sugar, arrows have their own special syntax enabled through the `Arrow` language extension. 

```haskell
{-# LANGUAGE Arrow #-}

mySpecialArrow = proc x -> do 
  y <- arrowOne -< x 
  arrowTwo -< y 

```
This case is equivalent to the simpler combination `arrowOne >>> arrowTwo`; however, `proc` syntax can allow you to build more complex interactions of arrows that would be difficult to read if you only used the combinators exposed in `Control.Arrow`. 


# Where should arrows be used? 

Arrows as a core design pattern don't appear too often throughout the Haskell ecosystem, but they do come up from time to time. 

For example, [Opaleye](http://hackage.haskell.org/package/opaleye) is a SQL database library that utilizes arrows to express queries. Arrow notation allows for queries to be examined and compiled into type-safe SQL code. 

The `ArrowLoop` typeclass, which we haven't discussed here, fits nicely into the Functional Reactive Programming model, and several libraries utilize this to build powerful tools based on arrow combinators. [Yampa](https://github.com/ivanperez-keera/Yampa/) is one such example.

Another strong use case for arrows is in the modeling and analysis of circuitry. Since arrows allow you to structure both the input and output of a transformation, it easily allows for analysis of various connection points. 

What each of these examples shares, is a desire to either: 
1) Express programs through combinators  
or 
2) Analyze and optimize whole programs of effects for optimization 

If your use case fits in with either of these patterns, arrows might make sense as your core design pattern. 

# Normal programming 
Returning to the function type constructor `(->)`, let's look at its arrow instance. 

```haskell
instance Arrow (->) where
    arr f = f
    (***) f g ~(x,y) = (f x, g y)
```

The first declaration for `arr` here is the easiest to understand: a function *is* an arrow in Haskell. This is extremely useful for writing terse and readable code using the standard arrow combinators. 
For example, 

```haskell 
f >>> g = g . f 
f &&& g = \x -> (f x, g x)
f ||| g = \x -> case x of 
                  Left l  -> f l 
                  Right r -> g r 
first f = \(x,y) -> (f x, y)
second g = \(x,y) -> (x, g y)
f *** g - \(x,y) -> (f x, g y)
```

These operators are used to simplify many pure functions to these shorter forms. 

# Related Reading 
[John Hughes' original paper titled "Generalizing Monads to Arrows" is a great insight into the arguments for arrows as a programming pattern.](http://www.cse.chalmers.se/~rjmh/Papers/arrows.pdf)
http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Arrow.html
https://wiki.haskell.org/Arrow_tutorial
