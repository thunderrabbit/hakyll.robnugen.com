---
title: Search
---

<form action="/search/" method="get">
<label for="q">Search: </label><input type="text" id="q" name="q" />
</form>
<noscript>
Sorry, this search is powered by Google and requires javascript to work inplace.  Please feel free to
[manually make your search][google].
</noscript>
<gcse:searchresults-only>Loading..</gcse:searchresults-only>
<script>
(function() {
 var cx = '008707464785815396126:zjg35ybgesa',
     gcse = document.createElement('script'),
     s = document.getElementsByTagName('script')[0],
     r = {};
 location.search.slice(1).split('&').forEach(
   function (p) {
     p = p.split('=');
     r[p[0]] = p[1] || '';
   });
 document.getElementsByTagName('input')[0].value = unescape(r.q);
 gcse.type = 'text/javascript';
 gcse.async = true;
 gcse.src = document.location.protocol + '//www.google.com/cse/cse.js?cx=' + cx;
 s.parentNode.insertBefore(gcse, s);
 })();
</script>

 [google]: https://encrypted.google.com/search?q=site:darkfox.hackerhaven.net
