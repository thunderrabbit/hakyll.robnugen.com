---
title: Hmm.. how can I insert images easily?
tags: emacs
author: Rob Nugen
date: 2014-11-01
---

##13:40 Saturday 01 November 2014 JST##

I've installed Prelude Emacs, hoping that it will support my desire to
easily embed images in my journal entries.  To insert an image, I need to insert soemthing like this:

```
![alt text](/path/to/image.png)
```

The idea is that I type some keys, in this case ```C-x !``` and I
interactively am asked for the alt text and then can quickly get a
list of images based on the year/month of my current journal entry, in
this case 2014/11.

Based on [hakyll.el](https://gist.github.com/thunderrabbit/453c1e1c4a959f113d10), I got the first part sorted:

    (defun hakyll-insert-image (alt-text)
      "Insert an image from /images/yyyy/mm to current buffer /posts/yyyy/mm/dd/....md"
      (interactive "sAltText: ")
      (let ((current-buffer-filename (buffer-file-name)))
        (insert
         (format "![%s](%s)"
    	     alt-text
    	     current-buffer-filename
    	     ))
        )
      )

So instead of inserting the current buffer filename, I want to parse
the current buffer filename for ````yyyy/mm```` and then select a file
from ```~/journal/images/yyyy/mm``` and insert (part of) *its* name instead.

I made a [Trello list of things I want to do on this journal](https://trello.com/b/ZsZU8NFz/hakyll-robnugen-com)
