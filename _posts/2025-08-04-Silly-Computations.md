---
layout: post
title: "Silly Computations"
date: 2025-08-04
tags: [haskell, performance, backend, design]
id: 12
published: true
---

Lately I’ve been doing a lot of performance work at Mercury, and I keep running into the same kind of problems. I call them **silly computations**.

These are computations that aren't just unnecessary; they’re **obviously duplicative**. They're the kind of thing where when it's laid out directly, it's immediately obvious that it’s a waste. 

Think: 
```haskell
getTheTime :: IO UTCTime
getTheTime = do
  _time <- getCurrentTime
  time <- getCurrentTime
  return time
```

When you're working on a single function, it’s easy to see the duplication. But when it’s spread across a call stack, it’s not always obvious.

```haskell
getTheTime :: IO UTCTime
getTheTime = do
  time <- getCurrentTime
  return time


getTheWeather :: IO Weather
getTheWeather = do
  time <- getCurrentTime
  weather <- getWeatherAtTime time
  return weather

doSomethingElse :: IO ()
doSomethingElse = do
  weather <- getTheWeather
  time <- getTheTime
  return (time, weather)

main :: IO ()
main = do 
  (time, weather) <- doSomethingElse
  liftIO $ putStrLn $ show "The weather at " ++ show time ++ " was " ++ show weather
```

From looking at `main` alone, it's not obvious we're performing the same computation twice. Similarly, just looking at `doSomethingElse`, it’s not clear we’re fetching the time twice. Now imagine five, six, or ten layers deep.

Most often, I've been seeing this problem creep in with database lookups.

## Database Costs

Let’s say a request comes in to our backend at `/organizations/:orgId`. 
For a Yesod-backed application like ours, the processing of the request looks something like this:
- First, run any pre-request middleware actions.
  - In these, authorization checks are common and we'll want to ensure the current user has access to the organization specified in the URI. 
- Then we'll run the handler action which contains all of our business logic for that route.
- Finally, we'll run any post-request middleware actions and send the response to the client.

Now, given the specific organization ID, multiple portions of this request handling might want access to the full `Entity Organization`. Every time we fetch those details we: 
- `select * from organizations where id = ?`
- Serialize the response object on our database
- Transmit it over the network.
- Deserialize it on our backend 
- Parse it into a Haskell data type.

Each of these blocks takes around 1-3ms every time it happens. In a well-built application, we might do this 3-5 times per request given the flow I described above. Specifically with Yesod, the various contexts are disconnected from each other. Requests that you made in your authorization checks are disconnected from the ones you make in your handler. Then, within the application logic there are infinite opportunities for silly work, especially when multiple engineers are operating on the same codebase every day. 

## Two Solutions

There are two broad ways I've found to mitigate these silly computations.

### 1. Per-request Caching

We use Yesod’s built-in per-request cache. 

```haskell
getCachedOrganization :: OrganizationId -> Handler (Maybe Organization)
getCachedOrganization orgId = do
  let bytes = UUID.toASCIIBytes $ unOrganizationKey orgId
  cachedBy bytes (runDB . get $ orgId)
``` 

(`cachedBy`'s particular implementation is less interesting though it's fairly straightforward to build on top of Yesod's specific cache or to make your own within your `Handler` monad.)

This allows us to share the results of database lookups within a single request across the lifecycle of the request. Without it, there's really no way to share data between your middleware and your handler contexts. 

### 2. Just Pass the Whole Thing

The simpler fix, and the one I wish we’d use more, is: **stop throwing away the result.**

If you fetch the `Organization` early, just pass the whole entity through the stack. Even if some functions only use the ID, or ignore it entirely.

This might feel wasteful. Isn’t it inefficient to keep passing data we don’t use?

Not really. Haskell is lazy. If no one touches the data, it won’t be evaluated. And compared to a full DB round-trip and deserialization, the overhead is tiny.

More importantly, it prevents a common failure mode: someone changes a feature to need more fields, doesn’t see the data is already available, and adds a fresh DB call. We see this constantly.

Passing the entity makes the cost of that data **obvious** and **free**. It's much easier to see a duplicate call when you already have the results in context (and since we often name things similarly, e.g. `org` or `orgId`, the compiler will warn you too!)

## Functional Core, Imperative Shell

This all fits into the broader design philosophy of **Functional Core, Imperative Shell** which is a pithy idea I heard [a long time ago](https://www.destroyallsoftware.com/screencasts/catalog/functional-core-imperative-shell) which is only recently starting to make more sense to me. 

The outer layers of your system should deal with as many side effects as it can: HTTP, database IO, caching. That’s where you pull in data, validate sessions, perform checks.

The core should be pure and testable. To keep it that way, **pull in everything you need early**, and **pass it through**.

That means fewer lookups, fewer surprises, and better overall performance. And most importantly: **less silly work.**
