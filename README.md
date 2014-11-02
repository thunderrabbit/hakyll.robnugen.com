This Site
=========

This site is built with [Hakyll] based on [code] by [DarkFox].

Installation
============

```
git clone https://github.com/thunderrabbit/hakyll-journal.git journal
cd journal    # see note about directory name
cabal sandbox init
cabal install --only-dependencies
```

Installing on the dependencies takes about 20 minutes.   During that time, set up a ```~/.ssh/confg```

```
cd ~/.ssh
touch config
```

Append something like this to your config file, using appropriate [ssh info for your website](https://encrypted.google.com/search?q=passwordless+login+ssh):

    Host hakyll
        HostName your.haskyll.site
        User username
    
After ```cabal install --only-dependencies``` finishes, we build the journal:

```
cabal install
```

After ```cabal install``` finishes, we can build and deploy the site:

```
site build
site deploy
```
If you just want to deploy locally, do:

```
site watch
```

and visit [your site locally](http://localhost:8000).

###note about directory name

The directory name ```journal``` is hardcoded in [```site.hs```](https://github.com/thunderrabbit/hakyll-journal/blob/master/site.hs#L205).  If desired, change it before running ```cabal install```.


[Hakyll]:               http://jaspervdj.be/hakyll/                "Hakyll"
[code]:                 http://hub.darcs.net/DarkFox/DarkFox-blog  "DarkFox's Hakyll blog"
[DarkFox]:              http://blog.darkfox.id.au/                 "DarkFox"

