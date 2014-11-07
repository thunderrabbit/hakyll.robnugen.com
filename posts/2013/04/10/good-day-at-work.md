---
comments: true
date: 2013-04-10 21:45:17
layout: post
slug: good-day-at-work
title: good day at work
wordpressid: 1013
tags: work
---

##21:45 Wednesday 10 April 2013

**8:57pm Wednesday 10 April 2013**

Pretty good day; our new server on AWS had been barfing hourly errors like, "the MySQL server has gone away" or "could not connect to mySQL server" even though 99.n% of the connections were fine.  We had been using MySQL 5.6, and certainly I may not have had it set up right.  Once I instantiated a server with 5.5 (and reverted the DB to MyISAM tables for those with FULLTEXT indices), it has worked perfectly fine.

Today Jonas started tweaking the front page of AB new frontend; it was fun for me to hit reload on the test server throughout the day and see it taking shape.  All that CSS and design shit still eludes me.  Or maybe I just have no interest.

I wrote a new class that will just be some static functions that do custom queries of the DB.  I might look into writing functions that live in the DB, but I'm not sure if it will really make a big difference.  I was seeing 0.0003 seconds per (cached) query in my class; that should be enough for now.

**9:41pm Wednesday 10 April 2013**

I looked into STORED FUNCTIONS and was like no way ferget that, and then I remembered oh [MySQL VIEWs](http://dev.mysql.com/doc/refman/5.5/en/create-view.html) are what I want.  Nice.  Compiled queries so they're even faster.  0.0002 seconds nice.

