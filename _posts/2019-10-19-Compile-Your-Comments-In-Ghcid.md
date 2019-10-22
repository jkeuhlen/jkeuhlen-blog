---
layout: post
title: "Compile your comments in ghcid"
date: 2019-10-19
tags: haskell ghcid
id: 4
---
# ghcid
If you've been using [ghcid](https://github.com/ndmitchell/ghcid) to work on your haskell projects, you've probably come to love the quick response times to typechecking your programs and the green `All good` that tells you you've finally satisfied the compiler. 

If you haven't been using ghcid but want to get started, check out [Matt Parsons excellent introduction](https://www.parsonsmatt.org/2018/05/19/ghcid_for_the_win.html).

One of my main issues with ghcid in the past has been the need to spin up a second terminal window for GHCi when I want to play with some code I've written to check correctness. Luckily, thanks to [@isovector](https://github.com/ndmitchell/ghcid/pull/248) that's no longer the case! 

# What's new? 

With a recent addition to ghcid, there's a new way to interactively play with code without having to have multiple windows open or jump back and forth between terminals. Remember, ghcid is a wrapper over GHCi so it should be able to do everything GHCi can, especially typechecking, kind exploration, or running simple functions! 

Due to a random bit of feedback in slack, I ended up tagged on the PR and got to follow along with the development of this new flag.

## So what is it?
In the new release of ghcid, 0.7.5, there is a new flag for starting the program:
`--allow-eval`. This allows the evaluation of arbitrary commands in your underlying GHCi instance.

Top level comments in your code that start with `-- $>` will be picked up by this new flag and be executed against the loaded instance. 

This provides the obvious value of using ghcid in a similar way to a ghci session for playing with code, exploring types, etc. Let's look at some examples! 

# Running `ghcid --allow-eval`

For this example, I'll simply be adding comments to a basic stub of a new project created with `stack init`. 

```haskell
module Lib
    ( someFunc
    ) where

-- $> someFunc 
someFunc :: IO ()
someFunc = putStrLn "someFunc"
```

If you add the comment here, and boot up ghcid in the most basic way you'll be able to see the magic at work:
`ghcid --command "stack ghci" --allow-eval`

You should be greeted with a nice green `All good` as well as ghcid telling you where it found your comment to run and the output of dropping that command into ghci:
```
All good (2 modules, at 10:09:27)

C:\\Users\Jake\workspace\compile-your-comments\src\Lib.hs:5:1
$> someFunc
someFunc
```

Since ghcid is smart about which files it actually typechecks for you, not every comment in every file will be run on each underlying `:reload`. For example, if we add a new comment to `Main.hs` and resave: 
```haskell
module Main where

import Lib

main :: IO ()
main = someFunc
-- $> putStrLn "Main just compiled"
```
We get: 
```
All good (2 modules, at 10:12:26)

C:\Users\Jake\workspace\compile-your-comments\app\Main.hs:7:1
$> putStrLn "Main just compiled"
Main just compiled
```
But if we close ghcid and restart so that everything has to reload we'll see: 
```
All good (2 modules, at 10:15:16)

C:\\Users\Jake\workspace\compile-your-comments\src\Lib.hs:5:1
$> someFunc
someFunc

C:\Users\Jake\workspace\compile-your-comments\app\Main.hs:7:1
$> putStrLn "Main just compiled"
Main just compiled
```

Remember, since we're just dropping everything after the `-- $>` into GHCi, we can also use exploratory features like `:t` or `:k` :

```
$> :t main
main :: IO ()
$> :k IO
IO :: * -> *
```

This new flag also doesn't try to protect you from yourself (which is why the feature is gated behind the run time flag in the first place). For example, you can stop ghcid from starting by adding: 
```haskell
-- $> :quit
```
to anywhere in your code path. Then, as soon as ghcid evalutes the comment it will close the underlying GHCi session and quit. As you saw earlier, it can also evaluate arbitrary `IO` actions so make sure to avoid any irreversible actions.
```
-- $> launchTheNukes
```

