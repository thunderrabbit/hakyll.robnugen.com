---
comments: true
date: 2013-04-26 08:00:47
layout: post
slug: ajax-at-work
title: ajax at work
wordpressid: 1059
tags: work
---

##08:00 Friday 26 April 2013

**7:04am Friday 26 April 2013**

I found some code online in Perl that showed an awesome fast way to do clustering of pins on a map.  I'll probably post the code on [SO](http://stackoverflow.com/) where many people have been asking how to do it.   That might make up for the points I lost yesterday by asking a question.

When a map marker is clicked, I load the units associated with it via ajax.  To display the units, I was using underscorejs and a crappy little template, but then I realized I should use the same template as is used on the front page.

But on the front page, I load the list of units with a full page refresh.   So I had to tweak the template to use javascript and then realized I should make the front page do the same.  So today I plan to make the front page units also load via ajax, which will make the site more responsive and consolidate a bit of code!
