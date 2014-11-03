---
title: Trouble to get iOS app validated
tags: ios, moe joe
author: Rob Nugen
date: 2014-10-31
---

##02:32 Saturday 01 November 2014 JST##

Went to Tariq's tonight to try getting progress on uploading an app
using Testflight just so we can test it during development.

![That app name doesn't exist, and yet it does.](/images/2014/10/here-is-A.-A-does-not-exist.png)

And today, I couldn't even get that far.

![Codesign hell](/images/2014/10/Error-creating-IPA-ffffffffuuuuuuu.png)

##12:50 Monday 03 November 2014 JST

Wow!

I got a call from Apple today.  Harumi explained that I need to create
the app in itunesconnect in order to deploy it over TestFlight.  This
is different than before Apple bought TestFlight.

Fortunately, and unfortunately-pleasantly-surprisingly, I do *NOT*
have to give a bunch of meta data for the app before deploying it via
TestFlight.  Just App Name, Bundle ID, SKU, and maybe one other thing
so trivial I don't remember it.
