---
title: install.sh
tags: install, git
author: Rob Nugen
date: 2014-11-26
---

##20:35 Wednesday 26 November 2014 JST##

I spent most of the day working on installing a website from git; it
has symlinks to a config directory and a vendor directory. We're not
just updating an existing directory, but cloning the repo from git
each time.

I made a little ```install.sh``` file:

    #!/bin/bash
    read -p "This will install the latest version from git!!  Are you sure? (n) " -n 1
    if [[ ! $REPLY =~ ^[Yy]$ ]]
    then
        exit 1
    fi
    
    echo
    echo installing new site....
    echo
    
    oldTZ=$TZ
    export TZ='Asia/Tokyo'
    DATE=$(date +%Y-%m-%d-%H%M%S)
    export TZ=$oldTZ
    
    ROOT_DIR=/var/www/example.com
    VERS_DIR=$ROOT_DIR/versions
    DATE_DIR=$VERS_DIR/$DATE
    
    sudo chmod 777 $VERS_DIR
    
    git clone --depth 1 git@gitexample.org:path/to/repo.git $DATE_DIR
    
    if [ ! -d "$DATE_DIR" ]; then
        echo "no folder created; did git clone work?"
        exit 1
    fi
    
    sudo chmod 755 $VERS_DIR
    
    sudo rm $DATE_DIR/config/ -R
    sudo ln -s $ROOT_DIR/config $DATE_DIR/config
    sudo ln -s $ROOT_DIR/vendor $DATE_DIR/vendor
    sudo chown www-data:www-data $DATE_DIR -R
    sudo chmod 750 $DATE_DIR -R
    
    sudo service monit stop
    sudo service apache2 stop
    if [ -f $ROOT_DIR/revert ]; then
        sudo rm $ROOT_DIR/revert
    fi
    sudo mv $ROOT_DIR/current  $ROOT_DIR/revert
    sudo ln -s $DATE_DIR $ROOT_DIR/current
    sudo service apache2 start
    sudo service monit start
    
    read -p "Does it work??  y to keep; n to revert to previous version: (n) " -n 1
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        sudo rm $ROOT_DIR/revert
        sudo chmod 755 $VERS_DIR
        echo
        echo
        echo installed NEW VERSION of site
        echo
        echo
    else
        sudo service monit stop
        sudo service apache2 stop
        sudo rm $ROOT_DIR/current
        sudo mv $ROOT_DIR/revert  $ROOT_DIR/current
        sudo chmod 755 $ROOT_DIR/versions/
        sudo service apache2 start
        sudo service monit start
        echo
        echo
        echo REVERTED to PREVIOUS version of site
        echo
        echo
    fi
