---
comments: true
date: 2012-07-24 17:57:48
layout: post
slug: fun-coding-but-moe-joe-its-all-proprietary
title: fun coding, but moe joe, it's all proprietary!
wordpressid: 454
tags: journal
---

##17:57 Tuesday 24 July 2012

Moe and Joe are the names of two race horses in a long joke I used to tell in YRUU. It includes the line "moe joe moe joe moe joe moe joe moe joe" several times in the joke.

 

I think Lin and I used "moe, joe, moe, joe" as a sort of chant while hiking up Mt Fuji, and it came to mean something was challenging or disliked.

 

Well, I like programming, but I dislike having to keep it all on this side of the server! I've got some fun (?) boxes that I can click and they talk to the database and create more boxes, and then click again and delete said boxes, all with the magic of jquery and css. But, I can't show you. Moe Joe.

 

This is for my AB client who we were going to meet today, but they cancelled. We'll try again next week.

 

Speaking of AB, yesterday I got a RUSH ticket from them saying they couldn't save some data in the DB. It was weird because I could see the data had been saved (to the change log), but sure enough it wasn't being displayed on the screen (because it wasn't in the DB).

 

The good news: I tried it on our test server and had the same problem. Yes! I opened up the data class that saves to the DB and looked for all the instances of the table to which it's supposed to be saved. All looked fine.

 

Then I just happened to notice my mistake, and it was due to a hack I had implemented last week. Fortunately it was an easy fix, and I moved the corrected file to the live server.

 

The broken code had been broken for a week, so I consulted the logs to see what damage had been done. 49 records had been edited.  Hmmm. I checked a bit more and found only 13 of them had the subrecords anyway, so only 13 lost their subrecords. Whew!! not too terrible! Instead of trying some magical SQL query to fix them all, I just went through them by hand and re-did the edits to re-add the subrecords.

 

That was last night, and fortunately it wasn't too bad so only made me 20 minutes late going home.

 

This morning I prepared a report and got it ready to print with our other stuff for the meeting.. and meeting cancelled. Awesome! I ddn't have to put on shoes!

 

Today is approximately my 90th consecutive shoeless day in Tokyo.
