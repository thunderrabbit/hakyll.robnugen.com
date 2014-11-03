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
    var cx = '009348858810432874853:wrhfyzzqknu';
    var gcse = document.createElement('script');
    gcse.type = 'text/javascript';
    gcse.async = true;
    gcse.src = (document.location.protocol == 'https:' ? 'https:' : 'http:') +
        '//www.google.com/cse/cse.js?cx=' + cx;
    var s = document.getElementsByTagName('script')[0];
    s.parentNode.insertBefore(gcse, s);
  })();
</script>
<gcse:search></gcse:search>

 [google]: https://encrypted.google.com/search?q=site:robnugen.com
