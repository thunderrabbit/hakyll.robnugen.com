---
title: Moving hakyll.robnugen.com to dreamhost
tags: aws
author: Rob Nugen
date: 2014-12-10
---

##13:36 Wednesday 10 December 2014 JST##

I eventually want to have a cloud server on which I have root access,
but right now, especially for this site, I don't need it.

On Dreamhost, I killed the DNS entry pointing hakyll.robnugen.com to
my EC2 instance, then asked Dreamhost to host hakyll.robnugen.com, but
the DNS doesn't seem to have updated yet.

    HI!
    
    I had a custom DNS record pointing hakyll.robnugen.com to an IP on Amazon AWS server. I want to use Dreamhost instead of Amazon for the server, so I killed the custom DNS record, and asked Dreamhost to host hakyll.robnugen.com
    
    On https://panel.dreamhost.com/index.cgi?tree=domain.manage
    I see a record of hakyll.robnugen.com in my domain listing: "Fully Hosted with PHP 5.4 User: hakyll"
    
    However, after the happy Dreamhost robot created the new user `hakyll` and the domain, I noted there was *NOT* a directory created on Horus at /home/hakyll/hakyll.robnugen.com
    
    I created the directory by hand and populated it with data.
    
    Unfortunately, http://hakyll.robnugen.com/ says there's no site there.
    
    Do I need to tweak something? Do you need to tweak something?
    
    Please let me know, or make the tweaks and enjoy a great day!
    
    - Rob
