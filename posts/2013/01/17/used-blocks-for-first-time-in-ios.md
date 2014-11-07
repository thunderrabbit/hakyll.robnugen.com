---
comments: true
date: 2013-01-17 07:51:23
layout: post
slug: used-blocks-for-first-time-in-ios
title: used blocks for first time in iOS
wordpressid: 779
tags: journal,programming
---

##07:51 Thursday 17 January 2013

I wrote this on [750words](http://750words.com/) this morning at 4am.

Just for fun (and because I need to head to work), I'll leave it unedited:

I got a really good set of results from coding in iOS; I used blocks for the first time, incluidng typedef to make them more palatable syntax whise. I've got a view with a model and it all kinda started with Ray Wenderlich's Snap example or whoever wrote that, but on his site, and I'm really thankful for the free stuff, so I believe I shall open source this thing Im making now, a countdown timer that does a visual color change while counting down and while ticking off the ticks by calling blocks sent by the insantiating objected. The thing so far is three objects deep, with the MultiTimerView being the top, and then it instantiates a MultiTimer, which instntiates a plurality of Pausable Timers which themselves instantiate an NSTimer, and I don't know how deep it goes from there. but I am thinking I'll put these guys on github as soon as I have the , um, the shrinking aspect of the image down. I'm hoping that I can just add one more timer and an animation of shrinkitude and have it work, but I might have to shrink and change colors with separate timers per each point of time. Not sure how the thing will react to two different threads asking the UIView waht to do.
