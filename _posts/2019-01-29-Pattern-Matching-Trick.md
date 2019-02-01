---
layout: post
title: "Pattern Matching Trick"
date: 2019-01-29
tags: haskell pattern-matching
id: 2
---

About a month ago, I stumbled into on odd case of pattern matching. I had a set of data types like the following:

```haskell
data Color = Red | Blue | Green | Black ...
data Car = Car { color :: Color,  ... }
``` 
I wanted to write a function that could take two cars and guarantee that they were the same color while doing other operations. A simple way to do this is to have an `Eq` instance for Color and then pull out the color from the car and check against it. 

```haskell
priceCompare :: Car -> Car -> Maybe ...
priceCompar car1 car2 = 
  if color car1 /= color car2 
    then Nothing 
    else ...
```

Now this technically *works*, but it doesn't feel quite idiomatic. Typically in Haskell, when we want to have a guarantee about the structure of our data, we pattern match. Pattern matching can also be combined with guards to make a nice, and arguably more readable, function. 

We're also going to make one more efficiency update for this pattern. `Eq` instances *could* be slow depending on their implementation. Instead, we want to compare the data constructors like we do in a pattern match. There isn't really a faster way to accomplish what we want. To do this, we'll add a `deriving Data` to our definitions, and then be good to go. In order to compare the constructors, we'll use a simple function utilizing [`Data.Data`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Data.html)

```haskell
constrEq :: (Data a) => a -> a -> Bool
constrEq x y = toConstr x == toConstr y
```

Then, we rewrite our function with our pattern matching + guards 
```haskell 
priceCompare :: Car -> Car -> Maybe ...
priceCompare (Car color1 ...) (Car color2 ...) | color1 `constrEq` color2 = ...
priceCompare _ _ = Nothing 
```

Now we know that the cars in our first line of pattern matching always have the same color! 