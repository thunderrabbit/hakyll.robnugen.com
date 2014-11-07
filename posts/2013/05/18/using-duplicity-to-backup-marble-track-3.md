---
comments: true
date: 2013-05-18 07:04:17
layout: post
slug: using-duplicity-to-backup-marble-track-3
title: using duplicity to backup Marble Track 3
wordpressid: 1146
tags: MT3
---

##07:04 Saturday 18 May 2013

**10:46pm Friday 17 May 2013**

I had been using [Cyberduck](http://cyberduck.ch), but it has to update the entire huge file each time so takes too long and cannot recover from network errors.

I found and even installed [expandrive](https://www.expandrive.com/) but it's not free, so I kept looking

I looked at [boar](https://code.google.com/p/boar/) which would probably do just fine, but I did a couple more searches

I found [git-bigfiles](http://caca.zoy.org/wiki/git-bigfiles) which would probably also do fine,

Then I found a quite recent article with several of my preferred keywords: [Backing up to DreamObjects with Duplicity](http://dreamhost.com/dreamscape/2013/02/11/backing-up-to-dreamobjects-with-duplicity/)

Everything went pretty smoothly with a couple caveats.  Note: the following isn't meant to be a tutorial so much as a reminder to myself how I installed Duplicity.

> brew install librsync python gpg ncftp

this failed until I ran

> brew update

Now back on track, but you should use [latest version of Duplicity](http://duplicity.nongnu.org/)

> tar xvzf duplicity-0.6.21.tar.gz

> pip install boto httplib2 oauth

> cd duplicity-0.6.21

> sudo python setup.py install

Now save the following into ~/.boto

    
    [Credentials]
    aws_access_key_id = xxxxxxxx your access key xxxxxxxxx
    aws_secret_access_key = xxxxxxxxxxxx your secret key here xxxxxxxxxxxxxxx


Yay ready to go

> duplicity --allow-source-mismatch ~/Documents/duplicity/ s3://objects.dreamhost.com/duplicity

duplicity spooked me at first when it responded with this:

    
    Local and Remote metadata are synchronized, no sync needed.
    Last full backup date: none


I was like "gah! it didn't backup!!!" but just give it some time(*) it should work.

(*) it took less time than for me to find another possible candidate: [s3cmd](http://s3tools.org/s3cmd)
