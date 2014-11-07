---
comments: true
date: 2012-11-21 13:14:14
layout: post
slug: enjoying-backbone-js-quite-a-lot
title: enjoying backbone.js quite a lot
wordpressid: 515
tags: journal
---

##13:14 Wednesday 21 November 2012

I wish I could show the code I wrote, but I can show [Jonahlyn's backbone drag-and-drop tutorial](http://jonahlyn.heroku.com/blog/2011/12/02/backbone-drag-and-drop) from where I got ideas for the drag and drop part.

Making the reporting system for AB, I've got backbone driving the data to the server with each change made by the user.

User can click [buildings] or [units] button to add where components to the search.  The buttons will create a select list with all the fields of that table, plus save the where component to disk.  When the select field is selected, it changes to the name of the field + an operator dropdown + an empty field, then writes the field selection to disk.  When the operator is selected, it gets written to disk.  When the user enters data in the field, it gets written to disk.  When user sorts the where components, the changes are written to disk.

[Backbone](http://backbonejs.org/), I think I love you.

**update 5:39pm**

Now I'm starting to look at [Marionette](https://github.com/marionettejs/backbone.marionette).  I found a [tutorial on Marionette](http://davidsulc.com/blog/2012/05/06/tutorial-a-full-backbone-marionette-application-part-1/); apparently it can make a [neat little bookshelf webapp](http://davidsulc.github.com/backbone.marionette-atinux-books/#search/Neuromarketing).  As I start digging into the tutorial, [I wonder if there's a CDN for Marionette](http://webmasters.stackexchange.com/questions/38073/is-there-a-cdn-for-backbone-marionette).


