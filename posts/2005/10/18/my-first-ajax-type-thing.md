---
comments: true
date: 2005-10-18 06:23:25
layout: post
slug: my-first-ajax-type-thing
title: my first AJAX type thing
wordpressid: 22
tags: my code
---

##06:23 Tuesday 18 October 2005

This code is very very very stolen from [Wikipedia](http://en.wikipedia.com).

I don't know why it doesn't work here, but it works on my [journal](http://robnugen.com/cgi-local/journal.pl?type=all&date=2005/10/16#zz_AJAX)

















![](/images/travel/Pb/49th_May-Aug_2005/diving_in_acapulco/diving_in_acapulco-Thumbnails/3.jpg)









    
    
    <script>
    // Wikipedia JavaScript support functions
    
    function showTocToggle() {
      if (document.createTextNode) {
        // Uses DOM calls to avoid document.write + XHTML issues
    
        var linkHolder = document.getElementById('toctitle')
        if (!linkHolder) return;
    
        var outerSpan = document.createElement('span');
        outerSpan.className = 'toctoggle';
    
        var toggleLink = document.createElement('a');
        toggleLink.id = 'togglelink';
        toggleLink.className = 'internal';
        toggleLink.href = 'javascript:toggleToc()';
        toggleLink.appendChild(document.createTextNode(tocHideText));
    
        outerSpan.appendChild(toggleLink);
    
        linkHolder.appendChild(document.createTextNode(' '));
        linkHolder.appendChild(outerSpan);
    
         toggleToc();
      }
    }
    
     function changeText(el, newText) {
       // Safari work around
       if (el.innerText)
         el.innerText = newText;
       else if (el.firstChild && el.firstChild.nodeValue)
         el.firstChild.nodeValue = newText;
     }
      
    function toggleToc() {
      var img = document.getElementById('toc').getElementsByTagName('img')[0];
      var toggleLink = document.getElementById('togglelink')
      
     	if(img && toggleLink && img.style.display == 'block') {
    	    changeText(toggleLink, tocShowText);
    		img.style.display = 'none';
    	} else {
    	     changeText(toggleLink, tocHideText);
     		img.style.display = 'block';
    	}
    }
    </script>
    
    <table>
    <tr>
    <td id='toc'>
    <div id='toctitle'>
    </div>
    
    <img src='/images/travel/Pb/49th_May-Aug_2005/diving_in_acapulco/diving_in_acapulco-Thumbnails/3.jpg' />
    </td>
    </tr>
    </table>
    <p><script type='text/javascript'>
     if (window.showTocToggle) { var tocShowText = 'show'; var tocHideText = 'hide'; showTocToggle(); } 
    </script></p>
    
