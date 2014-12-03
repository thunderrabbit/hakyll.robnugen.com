---
title: ppp with linux
tags: 
author: Rob Nugen
date: 1998-06-15
---

<title>got ppp working with Linux!!</title>

<p class=date>8:35 Monday 15 June 1998</p>

<p>Last night at midnight I shut down windows 95 after getting
help from my friend Ned on Linux.  I had been unable to get ppp
to work.  He told me to try the Networking icon in the control 
panel in Linux once I start XWindows.

<p>I did that and still had no luck.

<p>With a little poking around I found that I had to be logged in
as root for that to work; otherwise I got a bunch of zombied
processes.  Hmmmm

<p>With a bit more poking and lots of reading <em>SAMS Teach Yerself
Linux in 24 Hours</em> I found how to run a script called ppp-on
that would dial the modem and connect.

<p>I had to shutdown and reboot Win95 to find the phone number to
call my ISP, so I did that and rebooted into Linux.  I editted
the script and updated the resolv.conf file so it knew the IP
address of the primary and secondary DNS..

<p>After a few tries, I realized I had forgotten to chmod the
scripts to make them executable.  Then *bing* it worked!
It was 1:00am.

<p>I downloaded using Lynx the 3.2 meg Mosaic file and I could 
not find it on my system.. I have no idea where it went.
<br>So I went to bed around 1:35.

<p>7am *bing* I woke up and wrote down my dream and decided I'll
do laundry tomorrow instead of today (I promise!) and hopped back
onto Linux.

<p>I used ftp instead of lynx to ftp the Mosaic browser to my
machine and this time it was there.  (I tested with a small file
first)

<p>un gzipped Mosaic and ran it and wow!  it worked and I felt
the thrill of victory!

<p>Even more thrilling, I went to my homepage and the browser 
crashed and the terminal congratulated me with the message

<pre>
Congratulations, you have found a bug in
NCSA Mosaic 2.7b5 on Linux.

If a core file was generated in your directory,
please do one of the following:

  % dbx /path/to/Mosaic /path/to/core
  dbx> where

OR

  % gdb /path/to/Mosaic /path/to/core
  gdb> where

Mail the results, and a description of what you were doing at the time,
(include any URLs involved!) to mosaic-x@ncsa.uiuc.edu.

We thank you for your support.

...exiting NCSA Mosaic now.
</pre>

<p>I did as it asked (I hope emacs mailed it correctly; I think
so because the modem blinked when I sent it).

<p>I tried again to connect to my site and it worked!  And here I
am writing a happy successful journal entry from my Linux box.
</p>
