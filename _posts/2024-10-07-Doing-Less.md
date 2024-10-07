---
layout: post
title: "Doing Less"
date: 2024-10-07
tags: programming engineering
id: 10
published: true
---

<div class="callout callout-note">
  <p class="callout-title">Note</p>
  <p class="callout-content">This is the essay form of a talk I delivered for Mercury's Engineering Office Hours on 2024-09-27 — a weekly ~random discussion forum for our engineering team. </p>
  <p>If this resonates with you and you want to come work with us, check out mercury.com/jobs or reach out to me directly. </p>
</div>

When I had my first programming job in college, I was lucky enough to have an old-school sys-admin and perl monk as my mentor; Tim taught me a lot of different things that continue to influence my work to this day. 

One of the many guiding principles he helped me develop is that good programmers are lazy. This is often a somewhat paradoxical moniker but seemingly fitting in social descriptors at least. If we dive past the obvious connotations, we can tease out some practical ideas from it. 

Laziness is often used to defend approaches like DRY programming; why write many functions when one does trick? But I think the spirit goes deeper. I'd posit that you should stop _doing_ as your first line of action and instead spend more time thinking about your problem space. Then, do the smallest possible thing to move the needle on your problem. 

Working in this way often helps break down work into smaller chunks. As an example, I might not want to solve this massive memory problem that requires fixing bugs in GHC and spending months profiling complex systems. But I can identify a single bottleneck and apply a bandaid to that portion of the problem. Some might argue this is sweeping the problem under the rug. I agree that it is, but I want to argue that, sometimes, that is a good thing! 

Our job as engineers is not to solve all of the world's problems. It is not to make the best open source tooling; to improve the ecosystem we work in; or to find the perfect abstractions for our code. Our mission is to build systems that provide value to our customers. In exchange, our company gets to capture a portion of that value in exchange and uses it to pay us, our peers, and keeps the growth rocket ship fueled so our equity is worth something someday. 

There are tradeoffs to this approach. Knowing when to be lazy and when to aggressively go after the hard problems is itself a Hard Problem. This is why we hedge our bets as an organization by staffing ecosystem work or make longer-term bets on critical features. But you don't have to be responsible for every possible element of improvement. When you have a chance to deliver value to customers, do it! Approach this type of area with the mindset of "what's the least I can do to deliver the most value the fastest?"

When you spend more time thinking about the problem, you get to understand those trade-offs more intuitively. It helps you to start mapping the domain to know where the limitations are, what levers we have, and when we need to push more on the hard problems instead of the easy ones.

In this way, laziness is akin to being **observant**. So how can you be more observant? Some practical tips: 

1) Run the app locally. Use the feature your customer would experience.  
2) Poke around in prod on Mercury or MTS. Open the network tab. See what it does. See how long things take. Get a feel for what your end users experience every day.  
3) Use honeycomb to get basic insights.  
### Lazy programmers are observant. Seek to understand the details within the boundaries of your system.

> If you get into the mental habit of relating what you're reading to the basic underlying ideas being demonstrated, you gradually accumulate some wisdom. —Charlie Munger

One way to aggressively deliver more value to customers is to make sure the systems they rely on are lazy as well. Lazy systems are something that are less frequently discussed, but make sense conceptually. If lazy programmers are observant, what's the analog for lazy systems? 

When you go to a server, would you rather it compute the whole world or just the piece of data you care about in that moment? Just because we _can_ compute pi to the millionth digit in every http request doesn't mean we should! 
### Lazy systems solve one key problem at a time. Don't boil the ocean. 

Obviously, no one is going to intentionally build systems with the common joke of 

```haskell 
postSlowRequestR :: Handler () 
postSlowRequestR = do 
  liftIO $ threadDelay 10_000_00
  pure () 
```

left in just to be removed later to much fan-fare. 

The problem with lazy systems is that systems _grow_. Much like our growth teams, they _accrete_, in the raw sense of the world, multiple pieces of functionality and features. So while we might not have intentional thread delays, we _do_ often have code like this. 

```haskell 
postSlowRequestR :: Handler (JSONResponse ResponseObject) 
postSlowRequestR = do 
  x <- runDB getValueX 
  y <- runDB getValueY 
  res <- runDB getValuesXYZ
  pure . JSONResponse $ ResponseObject { 
	  foo = x, 
	  bar = y, 
	  baz = grabZ res 
  }
```

Or, we end up with two different endpoints, both called by the same page load that do this in two steps! 

```haskell 
postSlowRequestR :: Handler (JSONResponse ResponseObject1) 
postSlowRequestR = do 
  x <- runDB getValueX 
  y <- runDB getValueY 
  pure . JSONResponse $ ResponseObject1 { 
	  foo = x, 
	  bar = y
  }

postSlowRequest2R :: Handler (JSONResponse ResponseObject2) 
postSlowRequest2R = do 
  res <- runDB getValuesXYZ
  pure . JSONResponse $ ResponseObject2 { 
	  x = grabX res,
	  y = grabY res, 
	  z = grabZ res 
  }
```

This kind of duplicated work is hard to see immediately and often hard to test for. It requires a certain amount of constant vigilance to notice it creep in to the system and then the prune it again later.

Solving a single problem at at time is both a Product decision as well as an engineering one. You need to be comfortable pushing back on a UX that overly complicates customer actions. Most humans are single threaded. Give them a single task at a time and make it as fast and frictionless as possible to accomplish that task before moving on to the next one. 

Lazy systems help here. When each part of the system is doing the bare minimum, there is less opportunity for degenerate combinations of state. There is less opportunity for slowness to creep into systems, etc. 

As an example, most users don't want to see all 100,000 transactions on their account at once. At most, they want some metadata around the aggregate numbers and a selection of those transactions. Rather than return everything, we should just return the minimum needed to accomplish the high level task. 

This makes our system more resilient in a couple of ways.  
1) We'll spend less time in computing data that won't be used.  
2) We'll spend less time passing large chunks of data around that users don't need.  
3) We'll be able to avoid thundering herds of people requesting huge amounts of data at the same time.  

### When at odds, prefer a lazy system over skipping work yourself. 

Leave the code better than you found it. Delete unnecessary functions you stumble across. Remove duplicates. Simplify wherever possible. 


# Takeaways 
1) Write less; read more.  
	1) Set up personal alerts in honeycomb, poke around in prod, shadow a coworker, talk to a customer.  
	2) Do less coding; build better product.  
2) Cut ruthlessly  
	1) Remove accretions that no longer serve us. Simplify endpoints. Spend the time to simplify to make future changes smoother and higher quality.  
	2) Have less stuff; marie kondo up the codebase.  

<div class="callout callout-note">
  <p class="callout-title">Note</p>
  <p class="callout-content">After this, we walked through a couple of examples of this in action that only really make sense with the context of Mercury's systems. The generally applicable approach is to ensure you both HAVE telemetry data for your systems and you actually look at it. </p>
</div>
