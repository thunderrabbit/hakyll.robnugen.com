---
title: A reasonable(?) way to insert images
tags: images, emacs, hakyll
author: Rob Nugen
date: 2014-11-07
---

##10:02 Friday 07 November 2014 JST##

Though I still have to copy the image names by hand, once they are in
the correct directories, I have a pretty okay way of inserting them
into my journal pages.

For most images, I've settled on a convention of putting the main
image in a directory, and then thumbnails for the image(s) in a
```thumbs/``` directory.

For most of *those* images, I place them in ```images/yyyy/mm/```
directories for the year and month they were taken.  Images that are taken
as part of a project that lasts more than a month will get their own
directory at a deeper level, probably at ```images/project-name```.

Given that my journal entries are also in dated directories, I can
take the date of the current filename and use that to determine the
likely name of the directory in which the images for that entry are
located.

So, with this keystroke in emacs,

```
C-x !
```

I run this code in [my version of
hakyll.el](https://gist.github.com/thunderrabbit/453c1e1c4a959f113d10/50f674b040070c5d7af43f1c8ec455010b13387f)
(appropriate code snippet copied here):

    (defun hakyll-insert-image (alt-text)
      "Insert an image from /images/yyyy/mm to current buffer /posts/yyyy/mm/dd/....md"
      (interactive "sAltText: ")
      (let ((yyyymm (substring buffer-file-name 35 42)))
        (insert
         (format "![%s](/images/%s/thumbs/"
    	     alt-text
    	     yyyymm
    	     ))
        )
      )

This shortcut is named after the fact that markdown uses ! in its code to display an image:

```
![alt text](/path/to/image.png)
```

The 35 and 42 in the ```substring``` function are quite specific to my
system.  I had had a longer set of functions including ```nthcdr```,
```split```, ```concat``` (I think), and something else, so I decided
to just lobotomize it. 1% chance I'll put them back to make it
(vaguely) more portable sometime in the mysterious future.

Anyway, the 35 and 42 are designed to extract "2014/11" from
"/Users/thunderrabbit/journal/posts/2014/11/07/a-reasonable-way-to-insert-images.md"

- - - -

So, to insert an image, I find the image in Finder and copy the
filename and extension.

In emacs, after starting a journal entry (with ```C-x j```), I insert
an image with ```C-x !```.

A prompt asks for a string to use as "alt text" for the image, and, if
I enter "Izuku in Shinagawa", the function will insert this into my
file buffer:

```
![Izuku in Shinagawa](/images/2014/11/thumbs/
```

To which I append my paste buffer and ")" for ```Izuku in Shinagawa 2 Nov 2014.png)```

and end up with:

```
![Izuku in Shinagawa](/images/2014/11/thumbs/Izuku in Shinagawa 2 Nov 2014.png)
```

Which is rendered as:

![Izuku in Shinagawa](/images/2014/11/thumbs/Izuku in Shinagawa 2 Nov 2014.png)

*Then*, I ```C-a``` to the beginning of the line and type ```C-x @```
 (named because @ is after ! on my keyboard), which runs the following
 macro:

    (fset 'markdown-add-real-image-to-thumb
       [?\[ ?\C-s ?\] return ?\C-k ?\C-y ?\] ?\C-y ?\C-x ?\C-x return backspace ?\C-s ?t ?h ?u ?m ?b ?s return backspace backspace backspace backspace backspace backspace backspace ?\C-a ?\C-n])

Thereby converting this line:

```
![Izuku in Shinagawa](/images/2014/11/thumbs/Izuku in Shinagawa 2 Nov 2014.png)
```

to this:

```
[![Izuku in Shinagawa](/images/2014/11/thumbs/Izuku in Shinagawa 2
Nov 2014.png)](/images/2014/11/Izuku in Shinagawa 2 Nov 2014.png)
```

Which renders as this:

[![Izuku in Shinagawa](/images/2014/11/thumbs/Izuku in Shinagawa 2
Nov 2014.png)](/images/2014/11/Izuku in Shinagawa 2 Nov 2014.png)

(which looks the same, but now can be clicked for a bigger image.)
