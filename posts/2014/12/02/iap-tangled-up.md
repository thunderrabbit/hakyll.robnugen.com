---
title: IAP tangled up
tags: iap, ios
author: Rob Nugen
date: 2014-12-02
---

##12:28 Tuesday 02 December 2014 JST##

I'm trying to test the In-App Purchases for our new app, but for some
reason all the descriptions are off.  When I click this one it buys
that one and when I buy that one it buys the other one.  I thought I
had all this shit worked out.

##13:02

Got it worked out.  Apparently Apple returns the list of products in
alphabetical order instead of by the order I list them in my code.
