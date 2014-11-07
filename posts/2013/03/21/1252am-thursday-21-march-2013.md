---
comments: true
date: 2013-03-21 00:58:17
layout: post
slug: 1252am-thursday-21-march-2013
title: 12:52am Thursday 21 March 2013
wordpressid: 953
tags: my code
---

##00:58 Thursday 21 March 2013

**12:52am Thursday 21 March 2013**

Sweet!  I was just able to put together a child theme (called "Rob's theme is more for functionality than design") with a function that puts the current date at the top of each entry.

This code is in my new theme's functions.php file:

    
    <?php
        add_filter( 'default_content', 'my_editor_content' );
        function my_editor_content( $content ) {
            $dateTimeZoneJapan = new DateTimeZone("Asia/Tokyo");
            $dateTimeJapan = new DateTime("now", $dateTimeZoneJapan);
            $content = "<strong>" . $dateTimeJapan->format("g:ia l j F Y") . "</strong>";
            return $content;
        }
    ?>
