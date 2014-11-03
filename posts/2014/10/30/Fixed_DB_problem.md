---
title: Possible bug in MySQL
tags: mysql
description: 
author: Rob Nugen
date: 2014-10-30
---

##11:20 Thursday 30 October 2014##

I'm not ~~ready~~ likely to file a bug report, but I had to change a field to UNSIGNED before I could use it in a math operation that resulted in a negative value.

I wasn't trying to store a negative value, just

WHERE 2014 - `field` > 0

The field was unsigned smallint with value 2015.
