---
title: Installing journal with Travis
tags: travis, journal
author: Rob Nugen
date: 2014-10-31
---

##14:04 Friday 31 October 2014 JST##

Travis helped me (into and) out of Cabal Hell on my mac. heehee

```
cd ~
rm -rf .ghc
rm -rf .cabal
rm -rf ~/Library/Haskell
cabal update
cabal install cabal-install
```

Building cabal took a few minutes, even on my SSD-filled MBP. bah.

```
mkdir journal
cd journal
cabal sandbox init
```

Now need to create ```journal.cabal``` like this:


    name:           journal
    version:        0.1.0.0
    build-type:     Simple
    cabal-version:  >=1.10
    
    executable site
        main-is:            site.hs
        build-depends:      base == 4.*
                          , hakyll == 4.5.*
                          , filepath >= 1.3
                          , tagsoup >= 0.13
        ghc-options:        -Wall -O2 -threaded
        default-language:   Haskell2010


```
cabal install --only-dependencies
```

Installing on the dependencies took about 20 minutes.  (?)

After ```cabal install --only-dependencies``` finishes, we build the journal (which uses Hakyll)

```
cabal install
```

And now I can remove site, site.o etc from ```.gitignore```, and just add dist/.  ```.gitignore``` now looks like this:

    _cache/
    _darcs/
    _site/
    dist/
    .cabal-sandbox/

Now I can compile my site from anywheres.  :-)  Thanks, Travis!!!
