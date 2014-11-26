---
title: stress test
tags: 
author: Rob Nugen
date: 1997-10-02
---

<p>
A super funny/awesome thing just happened here at work.
<p>
I'm writing a new application that allows users to view their
invoices and print them in either text or postscript format.
Printing the text invoices is easy; each invoice is already stored as an individual file, and I just queue it to the printer.
<p>
Because of the screwed up way we do the postscript invoices, I have to extract from a huge file of invoices the individual postscript invoice, write it to a file, print that file and then delete the file.  Fine.
<p>
The current application that my code is replacing will break if you print three postscript invoices in quick succession. I made sure my code would not have that problem, but I didn't really want to test that because it would waste a lot of paper.
<p>
Today I made some modifications to my code to nicely display error messages to the user. So I'm testing my code and purposely defined a logical incorrectly so it couldn't print. Each time I pressed 'P', it would say, "cannot create file BLAHBLAHBLAHn. Call Customer Service" or something like that.  The n was a number that was incremented each time 'P' was pressed.
(for those technically inclined, this was simply to give each file a unique name). I held down 'P' and watched the number happily increase with each failed attempt to create the file. 
To test the robustness of my program, I used my phone receiver to hold down the 'P' key. It had counted up to 2000 when I left to go talk to Pam.
<p>
I showed Pam my new error message setup-- how it was much more user friendly and informative. To show her how it could handle three invoices being printed at time, I fixed the logical and hit 'P' three times in a row. We went to the printer and waited for them to come up (had to put more paper in the printer tray) and it printed all three. No sweat. Yay!
<p>
Came back to my cube and my computer was having a heart attack! Beepbeepbeepbeepbeepbeep incessant beepbeepbeepbeep!!!!  What the heck??? It's printing!!  I ran over to the printer queue and it had printed about 20 invoices, and was still going. <em>Shit!</em> I paused the printer and came back to see what happened. I checked the printer queue and there were **hundreds** of identical jobs ready to print!  Ack!!!
<p>
I called Systems Management and had them kill the entire queue and recreate it. They did, and problem solved. But what had happened???
<p>
When I was at Pam's desk, and fixed the logical, my computer was able to successfully create the files! It had created and queued 750+ files in about 5 minutes! Yay!!! My program blows away the current program!! Inadvertant stress testing, and my code passed with no problem.
<p>
