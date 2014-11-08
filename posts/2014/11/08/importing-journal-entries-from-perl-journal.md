---
title: Importing journal entries from Perl journal
tags: journal, import
author: Rob Nugen
date: 2014-11-08
---

##23:06 Saturday 08 November 2014 JST##

I've spent a few hours today processing my old school journal entries
for inclusion in my hakyll journal. I have been doing it all in emacs,
with some macros to speed up the process (of processing them).

The first macro redacts email address domains from comments:

    (fset 'rn-00-redact-emails-in-comments
       [?\C-s ?c ?o ?m ?m ?e ?n ?t return return ?\C-s ?@ return ?\C-k ?  ?* ?r ?e ?d ?a ?c ?t ?e ?d ?* escape ?b escape ?b return ?< ?p ?> ?< ?b ?> ?E ?m ?a ?i ?l ?: ?< ?/ ?b ?> ?  ?\C-a ?\C-p ?\C-k ?\C-k ?\C-x ?\C-s ?\C-x ?k return ?n])

The second macro embeds each comment (which were stored in separate
files in the old system) in the journal entry for which the comment
was written.

    (fset 'rn-01-embed-comments
       [?\C-s ?. ?c ?o ?m ?m ?e ?n ?t return return ?\C-  escape ?> ?\C-w ?\C-x ?\C-s ?\C-x ?k return ?p return escape ?> escape ?> return ?# ?# ?# ?c ?o ?m ?m ?e ?n ?t ?s return ?\C-y ?\C-x ?\C-s ?\C-x ?k return ?n ?n])

The third macro creates files that will be the meta data for each entry

    (fset 'rn-02-create-yaml-files
       [?\C-  ?\C-e escape ?w ?\C-x ?\C-f ?\C-y ?. ?y ?a ?m ?l return ?\C-y return ?\C-x ?i ?~ ?/ ?j ?o ?u ?r ?n ?a ?l ?. backspace ?- ?r ?n ?. ?c ?o ?m ?/ ?y ?a ?m tab return ?\C-p ?\C-f ?\C-f ?\C-k backspace backspace ?\C-d ?\C-n ?\C-e ?\C-y backspace backspace backspace backspace backspace ?\C-n ?\C-n ?\C-n ?\C-x ?\C-s ?\C-x ?b ?* ?M ?e ?s ?s ?a ?g ?e ?s tab return ?\C-p ?\C-s ?. ?c ?o ?m ?\C-s return ?\C-f ?\C-  ?\C-f ?\C-f ?\C-f ?\C-f ?\C-f ?\C-f ?\C-f ?\C-f ?\C-f ?\C-f escape ?w ?\C-x ?k return ?\C-y ?\C-b ?\C-b backspace ?- ?\C-b ?\C-b ?\C-b backspace ?- escape ?< escape ?x ?r ?e ?p ?l tab ?s ?t tab return ?_ return ?  return ?\C-a ?\C-x ?\C-s ?\C-x ?k return ?n])

There are a couple of macros to place tags in the meta data, based on file names:

    zzzz  -> sleepy
    dream -> dream
    etc

Then the last macro embeds the meta data in the files:

    (fset 'rn-06-merge-yaml-files
       [?\C-  ?\C-e escape ?w return return ?\C-p ?\C-x ?i ?\C-y ?. ?y tab return ?\C-x ?\C-s ?\C-x ?k return ?R ?\C-y backspace backspace backspace backspace ?m ?d return ?n ?d])

So far, I have embedded comments and created meta-data files for files from 1985 down through the middle of 2003.

