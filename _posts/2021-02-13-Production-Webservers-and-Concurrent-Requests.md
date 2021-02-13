---
layout: post
title: "Production Webservers and Concurrent Requests"
date: 2021-02-13
tags: haskell yesod concurrency production
id: 6
---
### Pre-release copy
Hi there! If you're reading this, it means this post isn't quite through the final editing process yet. It should be finished up soon!

## Concurrency basics

If you've been programming in Haskell long enough, one of the many truisms you've probably picked up is that Haskell is fantastic for concurrent programming. From [Software Transactional Memory](https://www.microsoft.com/en-us/research/publication/composable-memory-transactions/) to our strong concurrency libraries like [async](https://hackage.haskell.org/package/async), Haskell has an amazing foundation for doing large concurrent operations. One of the other great strengths of concurrent Haskell is provided by GHC: green threading.

For those unfamiliar with the concept, [green threads](https://en.wikipedia.org/wiki/Green_threads) are computational threads provided by the runtime environment, rather than the host operating system. Green threads are light weight and don't suffer from the same context switching penalties that host threads have. This benefit means it's extremely easy and efficient abstract over multiple independent computations. There is a lot more that can be and [already](https://www.fpcomplete.com/blog/2017/01/green-threads-are-like-garbage-collection/) has been written about green threads in Haskell, but there is one less often written aspect: using them in production.

> GHC’s user threads are lightweight; modern computers can run 100,000 user threads smoothly.
> - https://www.aosabook.org/en/posa/warp.html

As Haskell developers, we are lucky that we don't have to worry about doing any extra work to handle thousands of requests concurrently. In other languages, there is often a lot of work that goes into system design to ensure similar levels of concurrency.

As a developer and end-user, we get this power for free in each of the major Haskell web frameworks. However, it seems that quite often, the things we get for free are the things we take for granted. While OS context switching or thread limitations do not bottleneck these tasks because of how lightweight green threads are, other fundamental system resources are still limited.

## resource exhausted (Too many open files)

Through a convoluted set of circumstances, I recently found myself in a situation where the CPU on my company's production machines was entirely throttled. This lead to response times per request skyrocketing and subsequently the number of HTTP requests waiting for responses simultaneously increased. Normally, we'd think there would be no problem after getting the CPU in check. Each green thread should be able to spin off without much pressure and await scheduling by the run time system, which should happen soon after the CPU spike decreases. Then the main HTTP response threads will quickly reply.

So I quickly identified the CPU issue and resolved it, then waited for everything to iron itself out. That's when the server died.


`Network.Socket.accept: resource exhausted (Too many open files)`


Those experienced with linux may understand what happened here immediately, but it's something that is really hard to find written about within the Haskell community. For most developers, it likely isn't immediately clear if the problem is in the code they wrote, in the configuration of their application, in the configuration of their server, or maybe something in a library upstream of their application. In this case, it's important to realize that, more often than not, we run our code on linux systems that have their own resource constraints and default limits. Let's dive into what caused this error.

## File descriptors
The system error here is a common linux process error where either the process has been constrained by the OS because it has too many open files, or there are too many open files across the entire system. The quick way to [fix it](https://unix.stackexchange.com/questions/84227/limits-on-the-number-of-file-descriptors) is to either (a) increase the process file descriptor limit or (b) increase the system-wide limit. But what exactly is a file descriptor? The simplest explanation I've been able to find is in [this StackOverflow](https://stackoverflow.com/a/5256705/3806046) answer where [Tayyab](https://stackoverflow.com/users/647992/tayyab) writes

> it is just an integer number that uniquely represents an opened file in operating system

So what do files have to do with webservers? Most of the time, web server developers aren't saving files to the server or hosting an unlimited number of files alongside their process executables. This is where the old linux mantra of "everything is a file" comes in to play. Sockets are one of the seven identified file types in the POSIX standard. Because linux systems used to be designed for sharing resources among a large number of users on a single server, most distributions still carry limits aligned with that vision. Per-process limits also prevent nefarious processes from eating up all the system memory by forcing the OS to manage a huge number of open files. On a dedicated machine, these defaults get directly in the way of running a production server.

## Haskell servers

At the base of every Haskell webserver you'll likely find the `Network.Socket` library used for opening new socket connections. Each of these sockets is responsible for the actual communication between the server and the client, and will require a new file descriptor to be added for it to run.

In my recent case running Yesod, once we hit 1024 concurrent active sockets (give or take, there is some additional descriptor overhead for just running the process), the main thread died, no new requests could be established, and the whole executable stopped once the last socket was closed.


## Don't forget the little things
Potentially all of this was tribal knowledge by the most experienced in our community, but for me it was really hard to find people talking about these kinds of issues in practice. I know that our native threads definitely allow massive concurrency, so why was my code hitting this fatal error? Almost every discussion on concurrency seemed to forget a simple reminder: don't forget about the lesser-remembered limitations of your production environments.

I did find this comment in a random [reddit thread](https://www.reddit.com/r/haskell/comments/10bg0m/improving_the_performance_of_warp_caching_file/) about caching file descriptors in warp 8 years:

> One hairy thing about caching file descriptors is that OS limits around them tend to be low; I believe that 1024 is the default descriptor limit for most Linux systems right now. A production server can, and probably should, have that limit significantly raised just so that it can handle huge numbers of concurrent requests

I just wish I had found it sooner.