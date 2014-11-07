---
comments: true
date: 2012-04-10 19:09:42
layout: post
slug: work-4
title: work
wordpressid: 346
tags: journal
---

##19:09 Tuesday 10 April 2012

Today at work I modified the way pickups are stored in the system. According to the original specs, it seemed likely that we'd only need to have a comma separated list of units per user, but now they want to ensure that units are not deleted if they've been picked up by anyone in the system. So instead of tokenize and scan all the results, I can just ask the DB if the unit is in the pickups_2_units table.

 

Then got a request from client to further split the copy to units system, which allows copying of sets of fields from the units to other units in the system. I think I got all the changes finished but didn't move them to the live server just before leaving; I'll test it again and move to live server tomorrow morning.

 
