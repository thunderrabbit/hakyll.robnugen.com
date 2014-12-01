---
title: Fixed default values when creating new entries
tags: emacs, interactive
author: Rob Nugen
date: 2014-12-01
---

##14:15 Monday 01 December 2014 JST##

I updated the ```hakyll-new-post``` definition to give default values
for the date when I creat a new journal entry. Now instead of having
to look up the date and enter it, I just hit enter (three times) and
it enters the date components.

Thanks to [this answer](http://stackoverflow.com/a/9646268/194309) on SO, my code features ```read-string``` which can take default values:

    (defun hakyll-new-post (title tags yyyy mm dd)
      "Create a new Hakyll post for today with TITLE and TAGS."
      (interactive (list
    		(read-string "Title: ")
    		(read-string "Tags: ")
                    (read-string (format "Year (%s): " (format-time-string "%Y")) nil nil (format-time-string "%Y"))
                    (read-string (format "Month (%s): " (format-time-string "%m")) nil nil (format-time-string "%m"))
                    (read-string (format "Date (%s): " (format-time-string "%d")) nil nil (format-time-string "%d"))
    		)
    	       )
      (let (
    	(file-name (hakyll-post-title title))
    	(file-path (hakyll-post-path title yyyy mm dd))
    	)
        (set-buffer (get-buffer-create file-path))
        (insert
         (format "---\ntitle: %s\ntags: %s\nauthor: Rob Nugen\ndate: %s-%s-%s\n---\n\n%s\n\n"
                 title
    	     (downcase tags)
    	     yyyy
    	     mm
    	     dd
    	     (format-time-string "##%H:%M %A %d %B %Y %Z##")
            ))
        (write-file
         (expand-file-name file-path (concat hakyll-site-location "posts")))
        (switch-to-buffer file-name)
        ))
    

I created a gist for the rest of the code to
[create markdown journal entries in emacs](https://gist.github.com/thunderrabbit/453c1e1c4a959f113d10/#file-hakyll-el)
