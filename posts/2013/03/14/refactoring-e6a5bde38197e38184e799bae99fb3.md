---
comments: true
date: 2013-03-14 22:17:13
layout: post
slug: refactoring-%e6%a5%bd%e3%81%97%e3%81%84%e7%99%ba%e9%9f%b3
title: refactoring 楽しい発音
wordpressid: 922
tags: programming
---

##22:17 Thursday 14 March 2013

My crappy code featuring blocks was too convoluted for my brain to figure out how to make it all work, so now the project features a nice tree of classes with a ViewController at the top.

The Game class and GameView class don't even know the other exists.  Everything goes through the ViewController.  GameView has an array of CardViews.  Game has an array of Cards.   The CardViews and Cards do not know the others exist.

Now that I've got that sorted, it's much easier in my mind to figure out what's next.  So far, at least.

I just need to add a game over image and I could fuckitship.it, but I'm sure we'll polish it up a lot more.
