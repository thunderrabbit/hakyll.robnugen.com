---
comments: true
date: 2012-11-27 21:45:25
layout: post
slug: workin-on-my-backbone
title: workin on my backbone
wordpressid: 540
tags: work
---

##21:45 Tuesday 27 November 2012

Today at work I stopped trying to convert my code into a [Marionette](http://marionettejs.com) app; I just don't seem to have enough smerts to do it yet.  I'm gonna try again tomorrow, making a Marionette app from scratch.  Well, from a semi blank js slate, so hopefully it will go a bit more easily.

The code I've got so far uses [Backbone](http://backbonejs.org) to do some pretty sweet ajax compared to writing all the ajax queries in [jQuery](http://jquery.com).  The page lets the user create WHERE clauses for a report.  Buttons along the top have table names on them.  Selecting one (hmmm maybe I should make them a select list, cause there will be several(?) other tables added)..
Selecting one tells the page to create a dropdown list of most of the fields in the table.  Selecting a field from the dropdown list converts the View to the selected field name, an appropriate operator dropdown, and an empty text field for the user-supplied value.

Each of these are the components of my WHERE clause.  Each change is saved automagically to disk, so reloading the page reloads all the elements in their same states.  I don't yet have a GUI for grouping fields for OR and AND within the WHERE clause, but the backend supports this through something I've called groups.  Each same-numbered group are all joined with ORs and then distinct groups are connected with ANDs.

Tomorrow's page will allow the selection of fields for the SELECT portion of the query.  It's this page that I intend to create as a Marionette app from the start.
