---
title: installed windows 98
tags: windows 98
author: Rob Nugen
date: 1998-07-20
---

<title>Installed Windows 98</title>

<p class=date>20 July 1998</p>

<p>About a week ago, I got a Windows 98 CD and a new hard drive
onto which to install it .  My goal was to have three separate
bootable partitions:  Windows 95, Windows 98, and linux.

<p>Each time I tried to install Windows 98, it tried to overwrite
Windows 95.  I didn't want that because I don't trust Microsoft 
to have written a good enough OS that I'll want to keep it.

<p>I physically removed the two drives that have Win95 and linux on
them and installed Win98 on the remaining drive.

<p>That worked, but when I reinserted the two drives, it didn't
work.  I used linux partition manager to allow me to choose Win95
or linux or Win98.  The first two worked, but Win98 would halt
the system when it tried to load.

<p>I guessed that was because I changed the drive letter by 
removing and adding hard drives.  Okay, that makes sense, and I
didn't get terribly pissed off at Microsoft.

<p><hr align="left" width="25%">

<p>So last night I had thought of a new idea..  Install Win95 on
the third drive, then boot with that Win95 and install Win98 on 
top of *that* Win95 partition!     Delightful!

<p>I installed Win95 on the partition with no problem.  I assume
it was on that partition because I explicitly selected D:\Windows
as the directory into which to install, and had to confirm that
this drive designation was okay and changing this should only be
done by expert users.  yes yes yes it's fine.

<p>I was not too surprised when after the install I found Win95 had
squashed the linux partition
manager so I would have to reinstall it to allow me to boot Linux
or Win95 or Win95[2].

<p>Since I had selected drive D: so carefully, I assumed the system
was booting the Win95 partition on drive D.  Right?  

<p>I installed Win98. This time, it gave me the option of which
drive to install on. I chose drive D (short for "Duh, why didn't
you give me this option last time???") and it installed.

<p>Foreshadowing: (The procedure for installing Windows98 takes "between 30 and 
60 minutes, depending on the speed of your computer."  It took 
mine 47 minutes.) I mention this only to give you an idea of the
frustration level coming up.

<p>I rebooted and it came up Windows98 automatically. No surprise.

<p>So I started with my Linux boot.img disk and added installed
the linux partition manager.  I labeled the partitions Win95, 
linux and Win98.

<p>Rebooted and the partition manager prompt came up:

<p><pre>LILO:
</pre>

<p>This means "which partition should I load?"

<p>I entered "linux" and it booted linux beautifully.

<p>I rebooted and this time entered Win95.  Windows 98 came up.

<p>Oh shit.  Oh wait. Maybe I got the two switched around.

<p>I rebooted and this time entered Win98.  The system halted.

<p>Microsoft I hate you.

<p>From a DOS prompt, it appears that all my Win95 stuff is still
there, but it sure is not available from Win98. I don't know how
to make it available either.  What a pisser.

<p>I went to bed, and at some point had a dream that I got Win98 
uninstalled and all was good.

<p>This morning I woke up lamenting my loss and trying to figure
out how to resolve everything.  I booted from my Win98 DOS prompt
disk and read the help file and it said to unistall Win98, simply
type uninstall at the DOS prompt.

<p>It didn't say from what directory, so I found the executable 
and the executable said it could only be run from within Windows98.

<p>Fine. So I booted Windows98 and ran uninstall and it gave me
some ominous warning, something like,

<p><pre>"Microsoft writes crappy code. Continue?"</pre>

<p>I did not continue.  I'll call my friend Thomas who is Microsoft 
Certified and see if he has any ideas.

<p>So for now, I'm using Linux to access the Internet and write
my journal entries.
</p>
