About
=====

hakyll-journal is a blank website generator using Hakyll.  It stores its posts in yyyy/mm/dd folders.  It does not (yet) have pagination.  Pull requests are welcome.

This project is based on [code] from [DarkFox].

[Hakyll] is a Haskell library for generating static sites, mostly aimed at small-to-medium sites and personal blogs. It is written in a very configurable way and uses an xmonad-like DSL for configuration.

Installation
============


###get code

```
git clone https://github.com/thunderrabbit/hakyll-journal.git --single-branch journal
cd journal    # see 'modify site.hs if needed' below
cabal sandbox init
cabal install --only-dependencies
```

Installing on the dependencies takes about 20 minutes.   During that time, open a new terminal and set up ```~/.ssh/confg```:

###set up deploy info

```
cd ~/.ssh
touch config
```

Append something like this to ```~/.ssh/config```, using appropriate [ssh info for your website](https://encrypted.google.com/search?q=passwordless+login+ssh):

    Host hakyll
        HostName your.haskyll.site
        User username

Make sure config is 0600:

```
cd ~/.ssh
chmod 600 config
```

Note, ```hakyll``` (and the path to which this site is designed to be remotely deployed) is hardcoded in the [```site.hs```](https://github.com/thunderrabbit/hakyll-journal/blob/master/site.hs#L41).  If desired, change it before running ```cabal install```:

###modify site.hs if needed

While/after ```cabal install --only-dependencies``` finishes, optionally edit ```~/journal/site.hs```:

Lines 40 and 41 specify where the site will be deployed.  Currently it's deployed to ~/public on local machine, and to a server identified as "hakyll" via ```~/.ssh/config```.

Line 205 specifies that the site lives at ~/journal``` on your local machine.  Because this installation knows where it lives, ```site build``` and ```site deploy``` can be run from anywhere.

###finish compilation

```
cd ~/journal
cabal install
```

###add it to your PATH

While ```cabal install``` finishes, add ```~/journal``` to your PATH, by appending the following to ```~/.bash_profile```

    #adding next line for hakyll-journal (to do site deploy and site build without typing ./site)
    export PATH="$HOME/journal/.cabal-sandbox/bin:$PATH"


###build and deploy

Now we can build and deploy the site (from anywhere on your local machine!):

```
site build
site deploy
```

If you have no (other) local webserver set up, just

```
site watch
```

and visit [your site locally](http://localhost:8000).

###your personal blog

Update your blog files in ```~/journal/posts/yyyy/mm/dd directories```

Update .md files in ~/journal/ to include your personalized info.

After making your changes, build and deploy to see them on your site.

Use the directory ```~/travels/``` and its children as a template for other non-dated "pages" on your site

Adding other page directories will require changing ```site.hs```

After changing ```site.hs```, do ```cabal install``` then build and deploy.

###theme

The theme is taken from [[DarkFox]], though I replaced his cool fox logo with a crappy logo that you can have free.  Change it by replacing ```~/journal/images/logo.png``` and optionally searching for 'logo.png' in ```~/journal/css/default.css```.


[code]:                 http://hub.darcs.net/DarkFox/DarkFox-blog  "DarkFox's Hakyll blog"
[DarkFox]:              http://blog.darkfox.id.au/                 "DarkFox"
[Hakyll]:               http://jaspervdj.be/hakyll/                "Hakyll"
