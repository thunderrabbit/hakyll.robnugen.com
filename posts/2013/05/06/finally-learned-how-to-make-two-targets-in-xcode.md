---
comments: true
date: 2013-05-06 22:07:39
layout: post
slug: finally-learned-how-to-make-two-targets-in-xcode
title: finally learned how to make two targets in xcode
wordpressid: 1086
tags: journal,programming
---

##22:07 Monday 06 May 2013

**9:55pm Monday 6 May 2013**

The first few apps we made were all separate projects.  The apps were all similar, so I had a huge mess of code for making updates.  Specs were all confuzzed in my head; code was all over the place, possibly changed, possibly not.  Basically making updates wasn't fun.

The most recent app started its life as a mess of blocks, which confuzzed iOS enough to keep crashing the app.  I rewrote the whole thing using Matt's suggestion that the object instantiation look like a tree, not spaghetti.   The code was lovely and the app is pretty much finished but we're going to wait for Tariq's company iOS dev account to be created before we submit it.

However, the next app will be something like this app plus xyz, so I looked up on SO how to do it and came up with a github repo that shows [multiple targets in xcode](https://github.com/brennanMKE/MultipleTargets).  Thanks, Brennan!

So now one target does pronunciation practice then listening practice, while the other jumps straight into listening practice.  Yay!!


