---
title: Learn me some darcs
author: Rob Nugen
date: 2014-10-29
---

##15:40pm Wednesday 29 October 2014##

(Adapting from my previous entry)

    cabal update
    cabal install darcs-2.8.5
    darcs get --lazy thunderrabbit@hub.darcs.net:thunderrabbit/journal
    cd journal
    ghc --make -threaded site.hs
    ./site build
    ./site watch

