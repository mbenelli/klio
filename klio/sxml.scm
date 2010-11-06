; (using
;   andlet
;   prelude ; assert cout cerr nl
;   (lists    : filter)
;   (charsets : char-set-complement char-set:whitespace)
;   (strings  : string-prefix-ci? string-prefix? string-index-right
;     string-tokenize string-contains)
;   sxml)

(##namespace ("sxml#"))
(##include "~~lib/gambit#.scm")
(##include "prelude#.scm")
(##include "lists#.scm")
(##include "charsets#.scm")
(##include "strings#.scm")
(##include "sxml#.scm")

(declare
  (standard-bindings)
  (extended-bindings)
  (block)
  (not safe)
  (fixnum))

(include "sxml/sxml-tools.scm")
(include "sxml/serializer.scm")
(include "sxml/xpath-parser.scm")
(include "sxml/txpath.scm")
(include "sxml/sxpath-lib.scm")
(include "sxml/sxpath-core.scm")
(include "sxml/sxpath-ext.scm")

