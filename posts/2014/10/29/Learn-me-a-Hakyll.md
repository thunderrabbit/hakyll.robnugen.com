---
title: Learn me a hakyll
author: Rob Nugen
date: 2014-10-29
---

##12:38pm Wednesday 29 October 2014##

Here is what I started to write to Danny Su on his entry about [using Hakyll 4](http://dannysu.com/2013/03/20/hakyll-4/).

    Hi Danny

    I have decided to take the plunge to a static site, and want my previous journal entries to be stored in yyyy/mm/dd folders as you have on your dannysu.github.com repo.   Have you written about your blogging process?

    Given that almost every folder/file in dannysu.github.com repo is updated 

See the thing I can't grok is why nearly all the files in dannysu.github.com are updated each time the repo is updated.

But I can't formulate the question in a way I think would be easy to understand coming from some random guy online, so I'll hold it for now.

- - - 

##13:17##

There are several [sample blogs using Hakyll 4](http://jaspervdj.be/hakyll/examples.html).  Most of them just pile all the posts into the posts folder, but I want them organized in folders by date:  yyyy/mm/ddpostname (as my original journal) or yyyy/mm/dd/postname.

So, going through the list, I've come across a few with that seem to have that quality, based on the URLs of the site:

[http://dannysu.com/](http://dannysu.com/)
[http://www.skybluetrades.net/](http://www.skybluetrades.net/)
[https://xinitrc.de/](https://xinitrc.de/)
[http://abizern.org/](http://abizern.org/)

I've cloned the repos and will figure out which one is closest to what I'm trying to do:

1. have source files organized in yyyy/mm folders
2. have pagination
4. have a calendar to go back through entries
3. have tags

brb

- - - -

##13:38##

Aha!  I understand Danny Su's process.  He writes entries in posts, and then has a [Rakefile](https://github.com/dannysu/hakyll-blog/blob/23343c1d669e309f70f64ef4865962bd5fa7c6b1/Rakefile) which runs hakyll on the site and copies the files from _site to dannysu.github.com.

BUT, his file structure is now out of the running; he's got all his source files in one giant folder.

- - - -

abizern.org has a layout closest to what I like, but all source files are in one directory.

However, he's got an [emacs macro to create journal entries](http://abizern.org/2014/01/05/hakyll-new-post-with-emacs/)!!

- - - - 

xinitrc.de has all blog source files in one directory

- - - -

skybluetrades.net seems to be a winner!  The source files are in yyyy/mm/dd directories, and he's got photos embedded.

[http://www.skybluetrades.net/blog/posts/2011/10/10/whaleboat-island/index.html](http://www.skybluetrades.net/blog/posts/2011/10/10/whaleboat-island/index.html)

Now, the big difference is he's got each entry in its own subfolder inside the date folders, like yyyy/mm/dd/entry-name/index.markdown

I can probably deal with that seamlessly once I have a macro to create journal entries either in emacs or Sublime.

##13:50##

Okay, so I'm going to clone Ian's skybluetrades.net to see if I can make it work.  If so, I'll fork the repo.

---

Hmm he's got a [LICENSE file](https://raw.githubusercontent.com/ian-ross/blog/dc92eb67542de14afdd97cce91a3c9784bb78957/build/LICENSE).

I guess it means I can use it so long as I do so respectfully.

I'll send him an email.

##14:59##

I started to compose an email, but decided not to; the email just sounded like whining cause after cloning the site, I was unable to get the site to build at all.

Instead, I went through the rest of the list and found what I think I can use without Licensing questions.

[http://hub.darcs.net/DarkFox/DarkFox-blog](http://hub.darcs.net/DarkFox/DarkFox-blog)

It uses darcs instead of git.

##15:09##

    cabal update
    cabal install darcs-2.8.5
    darcs get --lazy http://hub.darcs.net/DarkFox/DarkFox-blog
    cd DarkFox-blog
    ghc --make -threaded site.hs
    ./site build
    ./site watch

http://localhost:8000

Hooray!  Hooray!  it works!

Okay, so I guess I need to learn me some darcs.

