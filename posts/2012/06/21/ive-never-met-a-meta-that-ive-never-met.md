---
comments: true
date: 2012-06-21 14:16:42
layout: post
slug: ive-never-met-a-meta-that-ive-never-met
title: I've never met a meta that I've never met
wordpressid: 425
tags: journal
---

##14:16 Thursday 21 June 2012

At work recently I've been writing code that writes code based on files templates and database structure for the objects in the system.  Basically this will allow me to just define the database tables and then run a process to write the CRUD code for those tables.

 

But this just isn't any old [CRUD](http://en.wikipedia.org/wiki/Create,_read,_update_and_delete).  The CRUD includes being able to merge records that were duplicated in the system.  It allows clicking previous and next to see the next record.  Even keeps track of the sort order of the list so when we go back to the list it's sorted as it had been.

 

And now today I'm looking at the field types and displaying the records in different ways depending on the type.

 

So if it's a field of type text, I display just the first 100 characters.  If it's an image_code, I'll display the image instead of the code.

 

But first I have to incorporate a bit of Alphonzo's code that I imagine he's going to ask me to add.

 

- - - -

 

Also, Happy Burpday Fred!!!!!!
