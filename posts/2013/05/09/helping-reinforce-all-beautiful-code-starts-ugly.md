---
comments: true
date: 2013-05-09 18:06:50
layout: post
slug: helping-reinforce-all-beautiful-code-starts-ugly
title: helping reinforce 'all beautiful code starts ugly
wordpressid: 1111
tags: journal,programming
---

##18:06 Thursday 09 May 2013

**5:52pm Thursday 9 May 2013**

"[All beautiful code starts ugly](http://nbsoftsolutions.com/blog/all-beautiful-code-starts-ugly)" was my mantra today as I hacked in some changes to the abstract search class.  It now features a new function


    
    
        public function setRidiculousStationJoin($bool = true) {
            $this->ridiculous_station_join = $bool;
        }
    



The search() function went from this


    
    
         protected function search($search_text = "", $limit = 0, $page_num = 1){
             return $this->runSearchLimitOffset($search_text, $limit, $limit * (($page_num > 0) ? $page_num - 1 : 0));
         }
    



to this


    
    
         protected function search($search_text = "", $limit = 0, $page_num = 1){
             if($this->ridiculous_station_join) {
                 $this->creating_temp_table = true;
                 $this->runSearchLimitOffset($search_text, $limit, $limit * (($page_num > 0) ? $page_num - 1 : 0));
             }
             $this->creating_temp_table = false;
             if($this->ridiculous_station_join) {
                 $this->join_temp_table = true;
             }
             return $this->runSearchLimitOffset($search_text, $limit, $limit * (($page_num > 0) ? $page_num - 1 : 0));
        }
    



AND, inside runSearchLimitOffset are a buncha if statements asking if $this->create_temp_table / $this->join_temp_table are true, and behaving differently accordingly.

However, for the ugly hackiness, it works pretty well.

My sincere hope is one of two things:

1) we can convince the client that the request doesn't result in a good UX (*)  (30% likely)

2) I can clean up the code, or at least make it a bit more generalized

- - - - 

(*) The request is to add another option to the select that tells what sort order to show results.

They want to add a "group by" option...  it makes no sense!  A user should be able to group by something *and* sort at the same time, but with this request, they can only do one or the other.


