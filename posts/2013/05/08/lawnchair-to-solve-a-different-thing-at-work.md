---
comments: true
date: 2013-05-08 19:27:32
layout: post
slug: lawnchair-to-solve-a-different-thing-at-work
title: lawnchair to solve a different thing at work
wordpressid: 1102
tags: journal,work
---

##19:27 Wednesday 08 May 2013

**7:10pm Wednesday 8 May 2013**

I was thinking recently how I didn't use [lawnchair](http://brian.io/lawnchair/) to keep track of which items were on a page when coming back to it.

But today I did use lawnchair for a different thing, mostly because I was already using lawnchair for a similar issue.  There's a page which uses [Datatables](http://datatables.net/) to display lists of units in the system.  When clicking to view a particular unit, there are previous and next buttons on the page.  Lawnchair keeps a big-ass list of all the unit ids so it knows which is the previous and which is the next.  I know there's some <del>optimation</del> optimization to be done here, but haven't figured out how yet.

A long time ago, a request was made by the client to select all the records in the list.  This needed to be backed by the database (`users_2_selections`) and I couldn't figure a good way to "re-search" the units according to what search parameters were just done.  I told them "it's a really hard problem" and put it off for months.

THEN, today, when looking at a different request, I realized that selecting all the records in the list could solve it (more easily than another way to solve it).  So I re-visited this problem and realized "derp! I have all the unit ids in Lawnchair!"

When they click on the thing (what's the word for a span that's not a link and not a button but responds to a click event?), it says "hey lawnchair, give me that list of ids!" and then ajaxes  "hey server, they wanna select all these ids!" and the server is like "cool" and slaps them into the users_2_selections table and all is good.

EXCEPT for one detail: it seems that if there's more than 1000 unit ids, the server doesn't get them all.  I'll have to look at that one later, possibly tomorrow.

dinner time now!
