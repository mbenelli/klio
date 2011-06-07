;;; csv.scm - Parse a comma-separated-values file.
;;;
;;; Copyright (c) 2011 by Marco Benelli <mbenelli@yahoo.com>
;;; All Right Reserved.
;;;
;;; Author: Marco Benelli <mbenelli@yahoo.com>
;;;

(##namespace ("csv#"))
(##include "~~lib/gambit#.scm")
(##namespace
  ("lists#" iota any every drop-while map-in-order list-index filter))

(define field-separator (make-parameter #\,))

(define (split sep)
    (lambda (str)
      (call-with-input-string
        str
        (lambda (p)
          (read-all p (lambda (p) (read-line p sep)))))))

(define (non-empty-record? r)
  (and (not (string=? (car r) ""))
       (any (lambda (x) (not (string=? x ""))) r)))

(define (complete-record? r)
  (every (lambda (x) (not (string=? x ""))) r))

(define (parse-line line)
  (let* ((len (string-length line))
         (last-char (string-ref line (- len 1)))
         (fill (if (char=? last-char (field-separator))
                   (lambda (x) (append x '("")))
                   values)))
    (fill ((split (field-separator)) line))))

(define (read-csv filename)
  (with-input-from-file `(path: ,filename char-encoding: ISO-8859-1)
    (lambda ()
      (let loop ((line (read-line)) (res '()))
        (if (eof-object? line)
            (reverse res)
            (loop (read-line) (cons (parse-line line) res)))))))

(define (select-info rows skip? fields)
  (let ((rows (drop-while skip? rows)))
    (if (null? rows)
        '()
        (let* ((header (car rows))
               (idxs
                 (if fields
                     (map-in-order (lambda (x)
                                     (list-index (lambda (y)
                                                   (string-ci=? x y))
                                       header))
                       fields)
                     (iota (length header))))
               (make-record
                 (lambda (v)
                   (let loop ((idxs idxs) (record '()))
                     (if (null? idxs)
                         (reverse record)
                         (loop
                           (cdr idxs)
                           (cons
                             (vector-ref v (car idxs))
                             record))))))
               (data (map list->vector (cdr rows))))
          (map make-record data)))))

(define (test)
  (select-info
    (read-csv "plant_hw.csv")
    (lambda (x) (not (string=? "row Num" (car x))))
    '("row Num" "shortName" "nameEquipment" "idVariable" "ntw" "address"
      "tipo" "stato 0" "stato 1" "Device")))

