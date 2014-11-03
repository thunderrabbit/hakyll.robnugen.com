---
title: Running Hakyll on AWS
tags: hakyll, aws
author: Rob Nugen
date: 2014-10-28
---

Running Hakyll on ~~Dreamhost~~ AWS

UPDATE: [Run Hakyll for free on AWS](/2014/10/30/tutorial-running-hakyll-for-free-on-aws/) is a better tutorial than the mess below.

I gave up trying to install Hakyll on Dreamhost when I realized I probably wouldn't be able to install Haskell at all. I have a ticket in to DH now, but I bet they'll say "sorry, not on shared hosting; why don't you try our VPS?"

The following is based on notes I took from [http://marcanuy.com/en/notes/18-deploying-a-laravel-website-with-git](http://marcanuy.com/en/notes/18-deploying-a-laravel-website-with-git) (currently 404, RIP old link), and 
[http://chrisdone.com/posts/hakyll-and-git-for-you-blog](http://chrisdone.com/posts/hakyll-and-git-for-you-blog)

This is a work in progress; better go elsewhere instead of follow the potholed minefield ahead:

Easy as 

1. get server with root access
2. set up Hakyll
3. set up git

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

Name : hakyll.robnugen.com

####Step 6 Configure Security Group:####

Create a new security group

Security group name: hakyll.robnugen.com

SSH from My IP

####Review and Launch####

I got a popup saying I can use SSD for free, so I'm selecing that.  (I had selected Magnetic in step 4 above)

##2. Set up Hakyll on server##



##3. Set up git on server##

On Server, set up bare repo:

```
cd ~/
mkdir bare
mkdir bare/hakyll.git
cd bare/hakyll.git 
git init --bare
```

Create hook for compiling the site after push:

    emacs ~/bare/hakyll.git/hooks/post-update

Make it look like this:

    #!/bin/sh
    work_tree=~/robnugen.com/hakyll
    GIT_WORK_TREE=$work_tree git checkout -f
    ghc --make hakyll
    ./hakyll build


// cd $work_tree


