---
title: first rsync of hakyll journal
author: Rob Nugen
date: 2014-10-29
---

In my ```~/.ssh/config``` file

```
Host hakyll
    HostName hakyll.robnugen.com
    User ec2-user
    IdentityFile ~/.ssh/thunderrabbitonAWS.pem
```

From local machine, login simply with ```ssh hakyll```

On the machine:

```
sudo yum update
sudo yum install nginx
sudo chown ec2-user /usr/share/nginx/html
```

On local machine


```
git clone git@github.com:thunderrabbit/hakyll-journal.git journal
cd journal
ghc --make -threaded site.hs
./site rebuild
./site deploy
```


