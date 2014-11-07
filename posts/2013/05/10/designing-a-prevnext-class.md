---
comments: true
date: 2013-05-10 18:28:24
layout: post
slug: designing-a-prevnext-class
title: Designing a prevnext class
wordpressid: 1119
tags: journal
---

##18:28 Friday 10 May 2013

Today at work I had a brilliant idea to have prev(ious) and next buttons work as expected. 

The blog entry index allows people to view e.g. only this category or that category or just this month or just last years entries.

Once they click the link to view a specific entry, the site has no idea what order nor what subset of entries was in the index.  I've solved the problem in the past by storing in lawnchair a list of all the ids, in order, resulting from the search.  With 60,000 entries, that makes the index page slow because all that data is being searched and processes in the background.  

While looking for a faster way to solve it, I realized an ingenious solution!!   I'll store in lawnchair the *query* and then just run the query with appropriate OFFSET and LIMIT to get the prev/next entries! 

I got the query saved in lawnchair and was just about to AJAX the query to the server upon arrival at the blog entry page and I realized, OH CRAP!!  What an excellent system for running arbitrary SQL on the server!!!

(For the lay reader, this is a Very Bad Thing)

Crap.  

So I scrapped my code and went back to my flexible A4 sized drawing board. 

I dug around on SO a bit looking for insight and pretty much decided there's no way to do it without storing the query.  So the prevnext table was born. 

Essentially, it's user_id and query_key as a grouped index, then a query as the query.

I've got a class that talks to that table, and I think I'm pretty close to having it work.  If it does work as planned, I'll probably port it to the system with 60,000 entries and make its index wonderfully faster.  

I probably would have finished it today, but it's Friday night and Lin and I are going out with Nonie in Tokyo.
