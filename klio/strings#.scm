(namespace
 ("strings#"
                                        ; predicates
  string-null?
  string-every
  string-any
                                        ; constructors
  string-tabulate
                                        ; list & string conversion
  reverse-list->string
  string-join
                                        ; selection
  substring/shared
  string-copy!
  string-take
  string-take-right
  string-drop
  string-drop-right
  string-pad
  string-pad-right
  string-trim
  strimg-trim-right
  string-trim-both
                                        ; comparison
  string-compare
  string-compare-ci
  string<>
  string=
  string<
  string>
  string<=
  string>=
  string-ci<>
  string-ci=
  string-ci<
  string-ci>
  string-ci<=
  string-ci>=
  string-hash
  string-hash-ci
                                        ; prefixex & suffixes
  string-prefix-length
  string-suffix-length
  string-prefix-length-ci
  string-suffix-length-ci
  string-prefix?
  string-suffix?
  string-prefix-ci?
  string-suffix-ci?
                                        ; searching
  string-index
  string-index-right
  string-skip
  string-skip-right
  string-count
  string-contains
  string-contains-ci
                                        ; alphabetic case mapping
  string-titlecase
  string-titlecase!
  string-upcase
  string-upcase!
  string-downcase
  string-downcase!
                                        ; reverse & append
  string-reverse
  string-reverse!
  string-concatenate
  string-concatenate/shared
  string-append/shared
  string-concatenate-reverse
  string-concatenate-reverse/shared
                                        ; fold, unfold & map
  string-map
  string-map!
  string-fold
  string-fold-right
  string-for-each
  string-for-each-index
                                        ; replicate & rotate
  xsubstring
  string-xcopy!
                                        ; miscellaneous: insertion, parsing
  string-replace
  string-tokenize
                                        ; filtering & deleting
  string-filter
  string-delete 
                                        ; low-level procedures
  string-parse-start+end
  string-parse-final-start+end
  let-string-start+end

  check-substring-spec
  substring-spec-ok?

  make-kmp-restart-vector
  kmp-step
  string-kmp-partial-search
  ))
