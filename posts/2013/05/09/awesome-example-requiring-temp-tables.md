---
comments: true
date: 2013-05-09 13:35:12
layout: post
slug: awesome-example-requiring-temp-tables
title: awesome example requiring temp tables
wordpressid: 1108
tags: programming
---

##13:35 Thursday 09 May 2013

**1:26pm Thursday 9 May 2013**

I've realized that the obtuse requirements for a certain report will be a lot easier to create with temporary tables.

While digging around for info on temp tables, I found this [gem of an DB example](http://blog.brianhartsock.com/2009/02/04/using-mysql-temporary-tables-to-save-your-brain/) by Brian Hartsock:


    
    
    CREATE TABLE `dogs` (
      `id` INT(11) NOT NULL AUTO_INCREMENT,
      `name` VARCHAR(255) DEFAULT NULL,
      PRIMARY KEY (`id`)
    )
    CREATE TABLE `legs` (
      `id` INT(10) UNSIGNED NOT NULL AUTO_INCREMENT,
      `dog_id` INT(10) UNSIGNED NOT NULL,
      PRIMARY KEY (`id`)
    )
    CREATE TABLE `nails` (
      `id` INT(10) UNSIGNED NOT NULL AUTO_INCREMENT,
      `leg_id` INT(10) UNSIGNED NOT NULL,
      `last_clipped` datetime NOT NULL,
      PRIMARY KEY (`id`)
    )
    



Hooray for toenail ids!

