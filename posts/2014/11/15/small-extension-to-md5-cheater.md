---
title: Small extension to md5 cheater
tags: code, crap, md5
author: Rob Nugen
date: 2014-11-15
---

##22:16 Saturday 15 November 2014 JST##

On the way home from Charu and Toru's house now.  Lin is sleeping
beside me because her phone is dead (because I used a lot of its
battery power to have Wifi while at their house).

I'm adding some not-very-extendable code to the md5 cheater thing: it will allow selecting which characters to convert. It's not extendable because I've just hard coded three sets:

    [A-Z] -> [a-z]
    [a-z] -> [A-Z]
	[ ] -> []

I eventually want to have fine grained control over each letter, but
my current method of coding it would require an if then chain so long
that if you laid it out end-to-end, then it would be too long to
maintain easily:

    if(document.getElementById('bigToSmall') && document.getElementById('smallToBig') && document.getElementById('spaceToNone')) {
        r = merge_objects(merge_objects(bigToSmall,smallToBig),spaceToNone);
    } else if(document.getElementById('bigToSmall') && document.getElementById('smallToBig')) {
        r = merge_objects(bigToSmall,smallToBig);
    } else if(document.getElementById('bigToSmall') && document.getElementById('spaceToNone')) {
        r = merge_objects(smallToBig,spaceToNone);
    } else if(document.getElementById('smallToBig') && document.getElementById('spaceToNone')) {
        r = merge_objects(smallToBig,spaceToNone);
    } else if(document.getElementById('bigToSmall')) {
        r = bigToSmall;
    } else if(document.getElementById('spaceToNone')) {
        r = spaceToNone;
    } else if(document.getElementById('smallToBig')) {
        r = smallToBig;
    } else {
        r = {};
    }


If I figure out a smarter way to code it, then I will.

##22:22##

Arriving in Kawasaki now.

##23:09

Oooh I have an idea. I can use JS to create all the checkboxes and
loop through each one to see which are checked, adding its from -> to
info to the replacement object.
