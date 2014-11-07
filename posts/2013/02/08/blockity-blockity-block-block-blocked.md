---
comments: true
date: 2013-02-08 20:39:27
layout: post
slug: blockity-blockity-block-block-blocked
title: blockity blockity block block blocked
wordpressid: 845
tags: programming
---

##20:39 Friday 08 February 2013

So I was using [Ray Wenderlich's Snap! tutorial](http://www.raywenderlich.com/12735/how-to-make-a-simple-playing-card-game-with-multiplayer-and-bluetooth-part-1) as the basis for our new card game.   Unfortunately, the app kept having random memory crashes.  Tariq was sure it must be due to some too big images or corrupted sound files.  I was sure it had to do with my naive use of blocks.  Unfortunately, I was right.

I compared my code with the code in the tutorial and found that at no place did they use blocks in calls between classes.  In my app, blocks between classes was a defining feature...  I still honestly don't know why that should make the app crash; in each case, I am sure the calling class still existed.  It didn't seem to be a case of zombie blocks either, where ARC keeps the block alive which keeps the caller alive, which keeps the block alive fo-eva-eva.

But crash it did, and now I'm off to find some more block tutorials; I gotta get this stuff sorted as blocks seem to be a staple for iOS apps these days.

In my mind, the crashes were happening because the memory got all twisted up.  It reminds me of a lecture from one of my astronomy professors at UH.  He was explaining the rotation of the sun at its equator is faster than the rotation at the poles.  This causes a twisting of the magnetic fields which ultimately ends up with a reversal of the sun's poles.  But he said, "we don't know exactly why the sun's magnetic poles reverse; the best we can tell is that the magnetic field lines get so twisted up that the sun just gets **mad** and reverses its poles."
