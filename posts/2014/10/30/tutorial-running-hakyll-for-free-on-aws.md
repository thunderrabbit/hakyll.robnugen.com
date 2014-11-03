---
title: Tutorial: running Hakyll for free on AWS
tags: tutorial, hakyll, aws
author: Rob Nugen
date: 2014-10-30
---

##20:20 Thursday 30 October 2014 JST##

These notes should work for *nix, including OS X, which I'm using.  Windows users may have to adapt when it comes to local stuff.

Easy as 

1. get server with root access
2. set up Nginx on server
3. set up Hakyll locally

##1. sign up for AWS micro (free tier available)##

I will probably go with all their defaults:

####Step 1:####

For this, I'm using ```Amazon Linux AMI 2014.09.1 (HVM) - ami-b5a7ea85``` because it's at the top of the AMI list when creating a new EC2 instance, offers a free tier, and includes Docker, which I've heard about more than twice, but know nothing of it yet.

####Step 2:####

t2.micro (free tier eligible)

####Step 3 (Configture Instance details)####

leave defaults

####Step 4 Add Storage:####

Root /dev/xvda snap-62cf04aa  8GiB  ~~Magnetic~~ gp2  Not Encrypted

####Step 5 Tag Instance:####

Anything you like is fine.  For me:

```
Name : hakyll.robnugen.com
```

####Step 6 Configure Security Group:####

Create a new security group.  Any name is fine.

Security group name: hakyll.robnugen.com

Make sure it has the following two rules:

```
SSH from My IP
HTTP from anywhere
```

####Review and Launch####

I got a popup saying I can use SSD for free, so I'm selecing that.  (I had selected Magnetic in step 4 above)

####Save the PEM####

The very final step results in a popup asking which PEM you want to
use to access the server.  If you don't have a PEM file, download and
save the file in your ~/.ssh directory.  After saving, set its
permissions to 400:

```
cd ~/.ssh
chmod 400 your-pem-filename.pem
```

##2. set up Nginx on server##

####Step 1, connect to server:####

Set up a config file in your local ```~/.ssh``` folder:

```
cd ~/.ssh
nano config
```

And make it look something like this:

    Host *
    ServerAliveInterval 120
    
    Host hakyll
        HostName your.ip.addr.ess
        User ec2-user
        IdentityFile ~/.ssh/your-pem-file.pem

Now, connecting to the server is simple:

```
ssh hakyll
```

####Step 2, set up nginx:####

Install nginx on server:

```
sudo yum update
sudo yum install nginx
sudo chown ec2-user /usr/share/nginx/html
```

##3. Set up Hakyll locally##

We've done all the groundwork; the rest is easy!

Back on your local computer:

####Step 1:####

```
git clone git@github.com:thunderrabbit/hakyll-journal.git journal
cd journal
ghc --make -threaded site.hs
./site rebuild
./site deploy
```

Finished!   Check out my journal on your server!  Feel free to wipe the sub-directories in ~/journal/post and write your own.  ;-)

After each change, you can update the site like this:

```
./site rebuild
./site deploy
```
