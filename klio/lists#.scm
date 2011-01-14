;; lists#.scm - Srfi-1 list library
;;
;; Copyright (c) 2010 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.

(namespace
 ("lists#"
                                        ; constructors
  xcons
  cons*
  make-list
  list-tabulate 
  list-copy
  circular-list
  iota
                                        ; predicates
  proper-list?
  circular-list?
  dotted-list? 
  not-pair?
  null-list?
  list=
                                        ; selectors

  first
  second
  third
  fourth
  fifth
  sixth
  seventh
  eighth
  ninth
  tenth
  car+cdr
  take
  drop
  take-right
  drop-right
  take!
  drop-right! 
  split-at
  split-at! 
  last
  last-pair
                                        ; miscellaneous
  length+
  concatenate
  append!
  concatenate!
  reverse!
  append-reverse
  append-reverse!
  zip
  unzip1
  unzip2
  unzip3
  unzip4
  unzip5
  count
                                        ; fold, unfold & map
  map
  fold
  unfold
  pair-fold
  reduce 
  fold-right
  unfold-right
  pair-fold-right
  reduce-right 
  append-map
  append-map!
  map!
  pair-for-each
  filter-map
  map-in-order
                                        ; filtering & partitioning
  filter
  filter!
  partition
  partition!
  remove
  remove! 
                                        ; searching
  member
  find
  find-tail 
  any
  every
  list-index
  take-while
  take-while!
  drop-while
  span
  break
  span!
  break!
                                        ; deleting
  delete
  delete!
  delete-duplicates 
  delete-duplicates!
                                        ; association lists
  assoc
  alist-cons
  alist-copy
  alist-delete
  alist-delete!
                                        ; set operations on lists
  lset<=
  lset=
  lset-adjoin
  lset-union
  lset-union!
  lset-intersection
  lset-intersection!
  lset-difference
  lset-difference!
  lset-xor
  lset-xor!
  lset-diff+intersection
  lset-diff+intersection!
  ))

