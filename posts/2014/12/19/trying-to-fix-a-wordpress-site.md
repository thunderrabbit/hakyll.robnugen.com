---
title: Trying to fix a wordpress site
tags: debug
author: Rob Nugen
date: 2014-12-19
---

##19:19 Friday 19 December 2014 JST##

JB asked me to show him Apple Memory, our iOS app.  He was like, "you wrote this?"

"Yes"

"Well, can you fix my website?"

"Maybe!"

##1am Saturday 20 December 2014

Took a while, but I did it.  (almost)

    Okay, I have learned a lot about Wordpress!!
    
    I had to remove the pagination from the bottom of the customizations
    index.  We can add it back later, but I have to do more research to
    figure out the proper way to add it back. (*)
    
    All of the Customizations are visible on the site, but the index only
    shows the first ten.  If you open the last one on the list
    http://www.example.com/custom/66/ and then click the 前 button, you
    can see the previous entry.
    
    (*) It looks like we will have to change the URL for
    example.com/custom/ to be example.com/customizations/ to show the
    index.  Each entry will keep its existing URL,
    e.g. example.com/custom/65/
