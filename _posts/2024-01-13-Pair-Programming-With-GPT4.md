---
layout: post
title: "Pair Programming With GPT4"
date: 2024-01-13
tags: ai gpt4
id: 9
published: true
---

Whenever I take time off work for an extended period of time (more than one week), I inevitably end up tinkering with various side projects. Last Christmas break, I decided to build a [job board](momjob.org) that I thought should exist in some fashion for a while now but I had never gotten around to building. (Caveat: I got the site live, but it's still quite rough around the edges, and I'm not sure if I'll continue working on it immediately.)

Instead of my usual tech stack, I reverted to something I haven't used in a long time: rails. I had two main reasons for this: 
1) I've had a pretty poor experience trying to use LLMs for programming in haskell. I don't think there's enough data in their sets to work well with it right now.
2) I've been thinking a lot about scaffolded/scripted work for kicking off projects. We don't have anything really like that in haskell, but rails is well known for it's scaffolding and it makes things pretty fast. I wanted to play with that kind of tooling again to see how well it actually worked for me. 

I'll start with my big takeaways from this experience, and then dive into some other thoughts. So first up, 

* GPT4 made it really easy to work with a framework and language that I haven't used in a very long term. 
* It's a fairly powerful augmentation to a developer, but it isn't able to put the whole picture together yet. 
* It's pretty bad at proactively suggesting things I need to do next (e.g. what I normally call Product Thinking). I had to be very explicit about what I wanted to do. 
* I'm definitely going to try it again. 

I used one long chat window to build my entire project, starting with having it walk me through creating the initial repo. Building up that context over time was really helpful. I could keep exploring certain ideas deeper and then ask it to regenerate code it had made 3 or 4 steps back with a new approach applied. That being said, I also had to make sure to only ask it to do one thing at a time. Whenever I asked it to generate say 4 new fields on a model, it would drop random parts of the second or third one and it forced me to double check all of the work rather than just copy/pasting and moving on quickly. 

It's not quite as fast as repl-driven-devlopment, but it was able to drop a lot of mental load and allow me to focus on what to build rather than how to build it. That being said, the feedback time was sometimes long enough that I'd find myself opening up another tab or picking up my phone. As the length of the chat grew, I found it failing to generate any response a lot more often.

GPT4 was really helpful for basic styling. For example, I would take a screenshot of how the current code was rendering and say something vague like "make this look better"; it would then re-generate erb templates with better styling applied. Since I had picked to use bootstrap early on, it remembered that context and I never had issues with hallucinating or pulling in styles from another framework. 

It's worth noting that there were a few things that it was a net-negative contributor on. Copy generation was especially bad, though I found it slightly more helpful than just dumping in Lorem Ipsum text. Sometimes, it would randomly change the order of form fields (Form label, Form control) -> (Form control, Form label) with no apparent notice or reasoning. It also failed at some easy tasks like "make these two elements in the DOM appear next to each other, rather than as separate rows." (It just needed to look at the code and suggest a missing flex class.) Whenever these things broke, it sucked me out of my flow and forced me to debug the code it wrote. Debugging someone else's code is always slower than debugging my own. 

It also still makes plenty of mistakes. GPT4 was often bad at keeping data in sync over time or remembering that we migrated to a slightly different model. When I moved from storing job post descriptions as plain strings to rich text, it just totally fell over and I had to back out, read the docs, and do it right. It also isn't always aware of the easiest way to accomplish something. With Rails, there's often a gem that does exactly what you want without building it from scratch. As an example, it picked a pagination gem for me but wanted me to do all of the bootstrap styling myself instead of using the complementary gem that already handled it. I'll also add that a common hallucination-like failure pattern I ran into was GPT4 suggesting to add a gem to my project and then just not use it at all. For instance, when I started the Stripe integration it suggested adding an extra gem that then was never used. 

Occasionally, it was faster than looking things up in the docs. I'm used to using hoogle which is way faster than even GPT lookups, but since I'm not as familiar with Rails/ruby GPT was often faster (but still not always).

The most helpful aspect of working this way was probably that it forced me to rubber duck all of my plans. I made myself be very detailed about what I'm trying to build as I explained it to GPT4, which helped clarify my thoughts in a way I normally don't do when I'm working on a solo side project. I also was using Github's Copilot in VSCode as I worked through this and I found that the two tools complemented each other nicely. Copilot picked up the slack in the places I didn't mention my code changes to GPT4. As one example, I wanted to update my seeds file every time I added new fields. GPT didn't know I had added that, but copilot made it a breeze to update with Faker every time. 

As a final note, I tried to use two different prompts to edit this post. The first was laughably bad. 

> Help me edit this blog post for clarity. If I should move any paragraphs around or edit phrasing, be specific about the suggested changes rather than just typing out a new version: 

It made a ton of suggestions to move paragraphs to their current positions and just kind of spat platitudes "Your blog post has a lot of insightful content..." without any actual constructive suggestions. 

The second prompt was slightly better.

> Pretend you are grammarly and fix this text: 

This time it just edited the text in place and sent it back to me. This kind of works, but I obviously don't want to copy/paste it and I needed to read both copies of the text side by side to see what changed. It changed the tone to be a lot more "professional" rather than keeping things in my voice and just helping shape or simplify my thinking. I could probably create a better prompt to get this to work the way I'd prefer, but for now I'll just keep editing my own posts.
