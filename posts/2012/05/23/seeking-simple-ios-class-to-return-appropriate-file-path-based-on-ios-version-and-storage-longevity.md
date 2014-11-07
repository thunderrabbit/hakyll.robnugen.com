---
comments: true
date: 2012-05-23 20:41:35
layout: post
slug: seeking-simple-ios-class-to-return-appropriate-file-path-based-on-ios-version-and-storage-longevity
title: seeking simple iOS class to return appropriate file path based on iOS version
  and storage longevity
wordpressid: 397
tags: journal
---

##20:41 Wednesday 23 May 2012

 

My latest SE question:

 

[http://programmers.stackexchange.com/questions/149806/seeking-simple-ios-clas...](http://programmers.stackexchange.com/questions/149806/seeking-simple-ios-class-to-return-appropriate-file-path-based-on-ios-version-an)

 

 

 

In my first iPhone project, I've got this code essentially repeated in several places across my few classes:

 
    
    // taken from http://ipgames.wordpress.com/tutorials/writeread-data-to-plist-file/ NSArray *paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES); NSString *documentsDirectory = [paths objectAtIndex:0]; NSString *path = [documentsDirectory stringByAppendingPathComponent:filename];

  

Obviously I don't want to repeat this code all over the place. I think I should create a nice little class to return a full path given a filename and one of the four storage types ("Critical", "Cached", "Temporary" or "Offline") listed in [Data Handling Categories](https://developer.apple.com/library/ios/#qa/qa1719/_index.html)

 

Then I realized the class should handle different iOS versions appropriately, returning, for example, a path in the Cache for 5.0 and below, and a [do not back up] file in Documents for 5.0.1 and above.

 

Then I realized someone has probably done it already, better than I could. Do you know of a class that will abstract the above logic into one line of code?

 

 

 
