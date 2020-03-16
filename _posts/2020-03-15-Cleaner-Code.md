---
layout: post
title: "Cleaner Code"
date: 2020-03-15
tags: haskell stack hpack
id: 5
---
# Cleaner Code
When I'm writing Haskell for fun, either learning something new or writing programs for hobby projects, I typically don't pay much attention to keeping my code clean. I'm sure I'm not alone in this habit, which can be undeniably described as a "bad" one. I don't put my hobby code through a rigorous code review process, so it tends to be bloated and my `-- TODO: Refactor this` or `-- TODO: Terrible hack` comments tend to live longer than any production examples that slip through. My code will inevitably end up peppered with unused language extensions here, shadowed variable names there, and mountains of unused imports I have to climb over to find out which package that random function was pulled from. Luckily, there is a really easy way to clean all of this up, and the sooner you enable it the easier it will be to keep even your "fun" code more readable. 

## Turning on Warnings
My colleague, Max, wrote an excellent piece awhile back on [Enabling All the Warnings](https://medium.com/mercury-bank/enable-all-the-warnings-a0517bc081c3) (2018) GHC has to offer. In it, he provides a copy-pastable list of flags to specify in `ghc-options`. If you haven't seen it before, you should check out that list! 

I only have one additional flag that I turn on because I know that if it ever fires, I've definitely made a mistake. As of GHC 8.2.1, you can turn specific GHC warnings into error messages. This allows you to prevent your code from compiling in instances where you don't want something to slip through the cracks as a warning! 
```
  -Werror=incomplete-patterns
```

## Setting it up

If you've never added your own `ghc-options` before, make sure that you are adding them in the right place. Many Haskell apps are structured with `src/` being the library code and `app/` being the application code. If this is the case, you'll likely want to turn on these warnings in both places. If you use `hpack` in your project, this can be done by specifying your extra `ghc-options` at the top level:   
*package.yaml*
```
default-extensions:
...
dependencies:
- base >= 4.7 && < 5
...
ghc-options:
- -Weverything
- -Werror=incomplete-patterns
- ...
```

This ends up generating a `.cabal` file that includes the specified options in both your `libary` and `executable` build sections ensuring your warnings appear everywhere you build your app.

## Conclusion 
By enabling all of these warnings, no matter what workflow you use, you'll get some extra help along the way from GHC helping you to write better, and cleaner code.

The moral of the story is: turn on warnings! Your future self will thank you.