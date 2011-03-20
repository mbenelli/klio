;; kliows.scm - Klio web server dependencies, used for debugging purpose.
;;
;; Copyright (c) 2010, 2011 by Marco Benelli <mbenelli@yahoo.com>
;; All Rights Reserved.

(include "../klio/prelude.scm")
(include "../klio/http.scm")
(include "../klio/http-auth.scm")
(include "../klio/kws.scm")

(include "~~lib/gambit#.scm")

(##namespace ("http-auth#" add-realm-path add-user with-authentication))
(##namespace ("kws#" get-static kws))

(add-realm-path "/doc" 'doc)
(add-user "test" "test" 'doc)

(pp http-auth#*protected-paths*)
(pp http-auth#*authorized*)

(kws
  port-number: 8000
  multithread: #f
  dispatcher: (lambda () (with-authentication get-static))
  server-root: "./sandbox")