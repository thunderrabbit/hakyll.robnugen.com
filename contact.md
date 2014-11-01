---
author: DarkFox
date: 2013-02-12
title:  Contact
---

Being able to contact me may be something that you would like to do, so below
I give you multiple methods that you can have a chat with me.

Email & IRC
===========

There isn't enough information for a table to be worthy so I have decided to
wrap my main contact methods with a little haskell. If anyone writes up a little
tool to take this kind of format and present specific details, please tell me
because otherwise I might. ;-)

``` {.haskell .showall}
data IRC         = IRC String String [String] deriving Show
data ContactInfo = ContactInfo
    { email :: [String]
    , irc   :: [IRC]
    } deriving Show

darkFox :: ContactInfo
darkFox = ContactInfo
    { email = [ "archeydevil@gmail.com"
              , "df@darkfox.us.to"      -- Currently not valid
              ]
    , irc   = [ IRC "irc.freenode.net" "DarkFox" freenodeChannels
              , IRC "silverirc.com"    "DarkFox" silverChannels
              ]
    }
  where
    freenodeChannels :: [String]
    freenodeChannels =
        [ "##security"
        , "#archlinux-offtopic"
        , "#archlinux"
        , "#hackerhaven-offtopic"
        , "#hackerhaven"
        , "#hakyll"
        , "#haskell-blah"
        , "#haskell"
        ]
    silverChannels :: [String]
    silverChannels =
      [ "#darkfox"
      , "#randomz"
      ]
```

PGP Key
=======

You can fetch my GPG key from [/DarkFox.pgp][MyPGP] so you can confirm to
a point that a message was from me as I sign all my mail.

``` haskell
~ >> gpg --list-keys --fingerprint darkfox
   | grep fingerprint
  Key fingerprint = C1E2 CD0F AC92 5360 CD82  8456 B19F 5C9A 7156 0F92
```

Hosting & supported services
============================

Although I have already got http access on my father's server, I don't depend on
his hosting nor services. Instead I host what I can freely and work with
different services, some closer than others. I'd just like to dedicate a set of
links for these services.

- [Hackerhaven], project that started through IRC, my IRC client remains there
  active today.
- [turtil.net], web and shell, I'm one of the main administrators.
- [freedns], the free `.us.to.` domain. This service limits you to having
  5 records on your account for free; this for me is more than enough. For the
  case that it isn't, you can just add a NS record to your own DNS running on
  your own server. Unlimited records and still-free domain.

darkfox.us.to is a DNS round robin between both [Hackerhaven] and [turtil.net].

 [MyPGP]:             /DarkFox.pgp          "My PGP Key"
 [Hackerhaven]: http://hackerhaven.net/     "Hackerhaven"
 [turtil.net]:  http://turtil.net/          "turtil.net"
 [freedns]:     https://freedns.afraid.org/ "FreeDNS"
