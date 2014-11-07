---
comments: true
date: 2005-09-29 05:06:40
layout: post
slug: 000-to-999-in-emacs
title: 000 to 999 in emacs
wordpressid: 10
tags: my code
---

##05:06 Thursday 29 September 2005

There have been times that I've needed lists of numbers in order.  Mostly for programming things.  Today I created such a list using emacs via a keyboard macro.

To use it, do M-x edit-kbd-macro and then choose a key combo.  (I always use C-x e).  Then replace whatever is in the buffer with this text:


    
    
    ;; Keyboard Macro Editor.  Press C-c C-c to finish; press C-x k RET to cancel.
    
    Command: last-kbd-macro
    Key: none
    
    Macro:
    
    C-SPC			;; set-mark-command
    0			;; self-insert-command
    RET			;; newline
    1			;; self-insert-command
    RET			;; newline
    2			;; self-insert-command
    RET			;; newline
    3			;; self-insert-command
    RET			;; newline
    4			;; self-insert-command
    RET			;; newline
    5			;; self-insert-command
    RET			;; newline
    6			;; self-insert-command
    RET			;; newline
    7			;; self-insert-command
    RET			;; newline
    8			;; self-insert-command
    RET			;; newline
    9			;; self-insert-command
    C-x r k			;; kill-rectangle
    C-x C-x			;; exchange-point-and-mark
    C-x r y			;; yank-rectangle
    C-b			;; backward-char
    C-x r t			;; string-rectangle
    0			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    C-b			;; backward-char
    C-x r t			;; string-rectangle
    1			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    C-b			;; backward-char
    C-x r t			;; string-rectangle
    2			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    C-b			;; backward-char
    C-x r t			;; string-rectangle
    3			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    C-b			;; backward-char
    C-x r t			;; string-rectangle
    4			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    C-b			;; backward-char
    C-x r t			;; string-rectangle
    5			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    C-b			;; backward-char
    C-x r t			;; string-rectangle
    6			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    C-b			;; backward-char
    C-x r t			;; string-rectangle
    7			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    C-b			;; backward-char
    C-x r t			;; string-rectangle
    8			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    C-b			;; backward-char
    C-x r t			;; string-rectangle
    9			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    C-SPC			;; set-mark-command
    ESC < ;; beginning-of-buffer
    C-x r k			;; kill-rectangle
    C-x r y			;; yank-rectangle
    2*C-b			;; backward-char
    C-x r t			;; string-rectangle
    0			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    2*C-b			;; backward-char
    C-x r t			;; string-rectangle
    1			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    2*C-b			;; backward-char
    C-x r t			;; string-rectangle
    2			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    2*C-b			;; backward-char
    C-x r t			;; string-rectangle
    3			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    2*C-b			;; backward-char
    C-x r t			;; string-rectangle
    4			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    2*C-b			;; backward-char
    C-x r t			;; string-rectangle
    5			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    2*C-b			;; backward-char
    C-x r t			;; string-rectangle
    6			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    2*C-b			;; backward-char
    C-x r t			;; string-rectangle
    7			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    2*C-b			;; backward-char
    C-x r t			;; string-rectangle
    8			;; self-insert-command
    RET			;; newline
    C-e			;; end-of-line
    RET			;; newline
    C-x r y			;; yank-rectangle
    2*C-b			;; backward-char
    C-x r t			;; string-rectangle
    9			;; self-insert-command
    RET			;; newline
    



C-c C-c to compile the code and then switch to an empty buffer.  C-x e and in a couple seconds: bang
000
001
002
003
...
997
998
999

Magic.
