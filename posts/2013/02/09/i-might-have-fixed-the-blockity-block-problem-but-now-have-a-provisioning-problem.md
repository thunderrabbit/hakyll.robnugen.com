---
comments: true
date: 2013-02-09 23:08:20
layout: post
slug: i-might-have-fixed-the-blockity-block-problem-but-now-have-a-provisioning-problem
title: I *might* have fixed the blockity block problem but now have a provisioning
  problem.
wordpressid: 859
tags: programming
---

##23:08 Saturday 09 February 2013

I thought I already had __block before variables that needed it, but I did not.

Adding __block in two (2) places seems to have been the magic touch I needed.

However, I have to deal with Apple's lovely provisioning process; I added Jimmy's phone to the available devices and now the app won't validate with Apple.  gah

I get this super precise incredibly unhelpful error message:

[![Screen Shot 2013-02-09 at 11.04.25 PM](/images/2013/02/Screen-Shot-2013-02-09-at-11.04.25-PM.png)](/images/2013/02/Screen-Shot-2013-02-09-at-11.04.25-PM.png)

I won't quote it because I don't have a solution, but I'll be looking for the solution soon enough. (I can still submit ad hoc versions for our dev testers so I don't care until it's time to submit the app to Apple.)
