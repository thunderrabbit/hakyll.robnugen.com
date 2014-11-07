---
comments: true
date: 2013-05-08 06:57:09
layout: post
slug: first-try-at-a-scoring-mechanism
title: first try at a scoring mechanism
wordpressid: 1096
tags: journal
---

##06:57 Wednesday 08 May 2013

**6:43am Wednesday 8 May 2013**

This morning I added a score calculator that takes into consideration the number of cards remaining. I figure if only a few cards remain, then it's easier to find the correct one.

It also makes use of prime numbers and floats in an attempt to ensure people do not often get the same high score.


    
    
    /* prime numbers:
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
    73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151,
    157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233,
    239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317,
    331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419,
    421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499 */
    
    NSTimeInterval timeElapsed = [[NSDate date] timeIntervalSinceDate:self.roundStartedDate];
    double bestScoreTime = self.numCardsRemain * 0.1 + 0.3;
    double goodScoreTime = self.numCardsRemain * 0.21 + 0.69;
    
    if(timeElapsed < bestScoreTime) {
         self.scoreMultiplier = 337;
         self.score += self.scoreMultiplier * (bestScoreTime - timeElapsed);
    }
    if(timeElapsed < goodScoreTime) {
         self.scoreMultiplier = 113;
         self.score += self.scoreMultiplier * (goodScoreTime - timeElapsed);
    }
    self.scoreMultiplier = 10;
    self.score += self.scoreMultiplier;
    
    NSLog(@"%d cards %f seconds %d",self.numCardsRemain,timeElapsed,self.score);
    
