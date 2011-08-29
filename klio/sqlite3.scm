;; Gambit-c's sqlite3 binding.
;;
;; Copyright (C) 2008-2011 Marco Benelli <mbenelli@yahoo.com>
;;

(##namespace ("sqlite3#"))
(##include "~~lib/gambit#.scm")
(##namespace ("lists#" list-tabulate))

(c-declare #<<C-END

#include <sqlite3.h>

C-END
)

;;; Types

(c-define-type void* (pointer void))
(c-define-type void** (pointer void*))
(c-define-type char* char-string)
(c-define-type char** (pointer char-string))

(c-define-type sqlite3 (struct "sqlite3"))
(c-define-type sqlite3* (pointer sqlite3))
(c-define-type sqlite3** (pointer sqlite3*))
(c-define-type sqlite3-stmt (struct "sqlite3_stmt"))
(c-define-type sqlite3-stmt* (pointer sqlite3-stmt))
(c-define-type sqlite3-stmt** (pointer sqlite3-stmt*))

;;; Error messages

(define sqlite3-errcode
  (c-lambda (sqlite3*) int "sqlite3_errcode"))

(define sqlite3-errmsg
  (c-lambda (sqlite3*) char* "sqlite3_errmsg"))

;;; Database connection

(define sqlite3-open
  (c-lambda (char* sqlite3**) int "sqlite3_open"))

(define sqlite3-close
  (c-lambda (sqlite3*) int "sqlite3_close"))

;;; Executing SQL

;(define sqlite3-prepare-v2
;  (c-lambda (sqlite3* char* int sqlite3-stmt** char**) int
;            "sqlite3_prepare_v2"))

(define sqlite3-finalize
  (c-lambda (sqlite3-stmt*) int "sqlite3_finalize"))

(define sqlite3-reset
  (c-lambda (sqlite3-stmt*) int "sqlite3_reset"))

(define sqlite3-step
  (c-lambda (sqlite3-stmt*) int "sqlite3_step"))

(define sqlite3-column-count
  (c-lambda (sqlite3-stmt*) int "sqlite3_column_count"))

(define sqlite3-column-type
  (c-lambda (sqlite3-stmt* int) int "sqlite3_column_type"))

(define sqlite3-column-int
  (c-lambda (sqlite3-stmt* int) int "sqlite3_column_int"))

(define sqlite3-column-double
  (c-lambda (sqlite3-stmt* int) double "sqlite3_column_double"))

(define sqlite3-column-text
  (c-lambda (sqlite3-stmt* int) char* "sqlite3_column_text"))

(define sqlite3-column-blob
  (c-lambda (sqlite3-stmt* int) void* "sqlite3_column_blob"))

;;; Following functions have a modified API

(define %sqlite3-open
  (c-lambda (char-string) sqlite3*
#<<C-END
  sqlite3* db;
  int res = sqlite3_open(___arg1, &db);
  ___result_voidstar = db;
C-END
))

(define %sqlite3-prepare-v2
  (c-lambda (sqlite3* char*) sqlite3-stmt*
#<<C-END
  sqlite3_stmt *stmt;
  const char *rest;
  int res = sqlite3_prepare_v2(___arg1, ___arg2, -1, &stmt, &rest);
  ___result_voidstar = stmt;
C-END
))


;; High-level interface.


;; Constants

(define (sqlite-integer? x) (eq? x 1))
(define (sqlite-float?   x) (eq? x 2))
(define (sqlite-text?    x) (eq? x 3))
(define (sqlite-blob?    x) (eq? x 4))
(define (sqlite-null?    x) (eq? x 5))

(define (sqlite-busy?    x) (eq? x 5))
(define (sqlite-row?     x) (eq? x 100))
(define (sqlite-done?    x) (eq? x 101))

(define (open name)
  (let ((db (%sqlite3-open name)))
    (if (zero? (sqlite3-errcode db))
        db
        (raise (sqlite3-errmsg db)))))


;;; Result handling

(define (process-row query ncol fn seed)
  (let ((get-item (lambda (i)
                    (let ((x (sqlite3-column-type query i)))
                      (cond
                        ((sqlite-integer? x) (sqlite3-column-int    query i))
                        ((sqlite-float? x)   (sqlite3-column-double query i))
                        ((sqlite-text? x)    (sqlite3-column-text   query i))
                        ((sqlite-blob? x)    (sqlite3-column-blob   query i))
                        ((sqlite-null? x)    #f))))))
    (apply fn (cons seed (list-tabulate ncol get-item)))))


; Interface
; fn: seed col col ... -> continue? new-seed
(define (db-fold-left db fn seed query)
  (let ((x (sqlite3-step query)))
    (cond
      ((sqlite-done? x) (sqlite3-finalize query) seed)
      ((sqlite-row? x) (let ((ncol (sqlite3-column-count query)))
                         (call-with-values
                           (lambda ()
                             (process-row query ncol fn seed))
                           (lambda (continue? res)
                             (if (not continue?)
                                 res
                                 (db-fold-left db fn res query))))))
      ((sqlite-busy? x) (raise "sqlite-busy"))
      (else (raise (sqlite3-errmsg db))))))


(define (sqlite3 name)
  (let ((db (open name)))
    (values (lambda (fn seed query)
              (db-fold-left db fn seed (%sqlite3-prepare-v2 db query)))
            (lambda ()
              (sqlite3-close db)))))

