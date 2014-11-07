---
comments: true
date: 2012-02-09 20:10:48
layout: post
slug: cherrypicking-from-git-reflog
title: cherrypicking from git reflog
wordpressid: 286
tags: journal
---

##20:10 Thursday 09 February 2012

I've been using [git](http://en.wikipedia.org/wiki/Git_%28software%29) for over a year, and I've enjoyed its awesomeness more and more as I use it. In the past couple of weeks, I've made great use of git reflog, which keeps steady track of all the changes that affect the files.

 

Usually I use it to reset my local workspace to the last point at which I uploaded all the files to the live server (which isn't yet using git; we'll fix that next week).

 

Today, I had my files from with changes from A - Z, but Z was unstable. I needed to move everything up through change Y to the server.

 

So I committed change Z and then looked at the reflog:

 

**git reflog**

 

32b4064 HEAD@{0}: commit: unstable fixing stateSave for unit list  
b98e645 HEAD@{1}: commit: keypress keyup ---> keyup  
0918839 HEAD@{2}: pull : Fast-forward  
213ce30 HEAD@{3}: commit (amend): v1.2.22 Neg is correct instead of YES on pdf  
e1f1f55 HEAD@{4}: commit: v1.2.22 Neg is correct instead of YES on pdf  
4ecc614 HEAD@{5}: merge arc2: Fast-forward  
e2f3f83 HEAD@{6}: checkout: moving from arc2 to master  
4ecc614 HEAD@{7}: commit: v1.2.21 ARC changes online

 

Then, and I don't know if this is the best way to do it, I reset to the last stable change

 

**git reset b98e645**

 

And then tossed out the unstable changes by stashing them.

 

**git stash**

 

I moved all the code to the live server, made a couple more changes to the code, pulled from github, and then wanted to get the unstable changes back so I can start working on them.

 

**git cherry-pick 32b4064**

 

Cherry picking right out of thin air; the commit wasn't on any branch, but git awesomely lets me get it anyway. Thank you, Linus and everyone who helped make git get great.

 
