---
title: Imported all entries from my Wordpress into Hakyll
tags: journal, wordpress, export, hakyll
author: Rob Nugen
date: 2014-11-08
---

##22:41 Friday 07 November 2014 JST##

Got all the text as far as I can tell, though the tags sometimes have capital letters.  BUT the much bigger problem is that none of the images were imported.  Their links are embedded in the entries, but the actual files are on robnugen.com/blog/wordpress/whatsit

##10:13am Saturday 8 November 2014 JST

Okay, so last night I forked and tweaked [mloskot/exitwp-hakyll](https://github.com/mloskot/exitwp/tree/exitwp-hakyll)
so now I have my own [hakyll version of exitwp](https://github.com/thunderrabbit/exitwp/tree/exitwp-hakyll)

I tweaked the code so it would use ".md" as the file extension, insert the entry date inside the body of the text the way I like, and I upated the README a bit.

(I also made it say "tags" instead of "categories" because I don't plan to have categories in my Hakyll journal, at least not yet)

After

    pip install pyyaml

Then

    python exitwp-hakyll.py

processed the xml file perfectly but I was like, "hey where are the images?"

Turns out the images are not included in the export from Wordpress, which just produces an XML file of the textual data.  Links to images are simply HTML links within the body of the entries.

Part of me wants to solve it in code:

    for each file in the output
       regex search for links to images
       for each regex found
          determine new URL (path)
          download the file from old URL to new path
          update the link in the text file

But it will be simpler overall to just migrate the whole file structure via scp like Martin did in May: [From Wordpress to Jekyll](http://blog.8thcolor.com/en/2014/05/migrate-from-wordpress/)

So here's what I did:

    mkdir ~/journal-images
    cd ~/journal-images
    scp -r robnugen.com:~/robnugen.com/blog/wp-content/uploads/* .

Then with Finder moved the images from ```~/journal-images``` to where they should be in my local hakyll repo. (I used Finder because some of the directory names overlap, and many of the directories were empty!)

Search and replace over all new markdown files: ```http://robnugen.com/blog/wp-content/uploads/``` → ```/images/```

