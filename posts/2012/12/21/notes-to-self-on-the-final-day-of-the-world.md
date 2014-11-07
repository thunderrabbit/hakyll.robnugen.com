---
comments: true
date: 2012-12-21 18:43:04
layout: post
slug: notes-to-self-on-the-final-day-of-the-world
title: notes to self on the final day of the world
wordpressid: 645
tags: journal,work
---

##18:43 Friday 21 December 2012

because Monday is a holiday, I figure my brain will need some notes to reload its current state.  Some notes on my desk point to a URL that will result in the notes here, spewed out by code in progress:


    
    in code above line 64, we need to fill in the default email 
    from site_text factory (into array shown below)



    
    Array
    (
        [renter_id] => 23
        [to_name] => doug e fresh
        [to_email] => doug@e.fresh.net
        [subject] => These properties may be of interest
        [body] => data from site_text factory goes here
        [from_name] => Rob Nugen
        [from_email] => robnugen@company.co.jp
    )



    
    above is info from set 1


after save, display the email_set template or an error message. this will be the initial data entry for admin (but is now broken because template is trying to read data from DNE variables). we HAVE NOT sent the email yet

when admin enters data, need to figure out how to deal with new record (I think the sent_email class will create new record just fine)

now once data is POSTED by admin, we will be in section above marked with HAPPY NEW MAYAN CALENDAR
