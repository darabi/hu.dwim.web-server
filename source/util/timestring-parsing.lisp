;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;; cl-irregsexp is 6 times faster than cl-ppcre in this
#+nil
(cl-irregsexp:match-bind (weekday ", " (day (integer :length 2)) " " month " " (year (integer :length 4)) " "
                          (hour (integer :length 2)) ":" (minute (integer :length 2)) ":" (second (integer :length 2)) " GMT")
    "Sun, 06 Nov 1994 08:49:37 GMT"
  (foo weekday day month year hour minute second))

(def function %parse-timestring/construct-timestamp (weekday second minute hour day month year)
  (bind ((timestamp (local-time:encode-timestamp 0 second minute hour day month year :timezone local-time:+utc-zone+)))
    (debug-only
      (local-time:with-decoded-timestamp (:day-of-week result-weekday :timezone local-time:+utc-zone+) timestamp
        (assert (equal weekday result-weekday))))
    timestamp))

(def constant +rfc1123-regexp+ "^(\\w{3}), (\\d{2}) (\\w{3}) (\\d{4}) (\\d{2}):(\\d{2}):(\\d{2}) GMT$")
(def special-variable *rfc1123-scanner* (cl-ppcre:create-scanner +rfc1123-regexp+))

(def (function o) parse-rfc1123-timestring (string &key (otherwise (list :error "Unable to parse ~S as a rfc1123 timestring" string)))
  (cl-ppcre:do-register-groups (weekday day month year hour minute second)
      (*rfc1123-scanner* string (handle-otherwise otherwise))
    (macrolet ((to-integer (&rest vars)
                 `(progn
                    ,@(iter (for var :in vars)
                            (collect `(setf ,var (or (ignore-errors (parse-integer ,var))
                                                     (return (handle-otherwise otherwise))))))))
               (lookup (var values)
                 `(unless (setf ,var (position ,var ,values :test #'equalp))
                    (return (handle-otherwise otherwise)))))
      (to-integer day year hour minute second)
      (lookup month local-time:+short-month-names+)
      (lookup weekday local-time:+short-day-names+))
    (return (%parse-timestring/construct-timestamp weekday second minute hour day month year))))

(def (function io) parse-http-timestring (string &key (otherwise (list :error "Unable to parse ~S as a http timestring" string)))
  (or (when (length= 29 string)
        (parse-rfc1123-timestring string :otherwise nil))
      ;; TODO according to http://www.ietf.org/rfc/rfc1945.txt we should understand all of these:
      ;; Sun, 06 Nov 1994 08:49:37 GMT    ; RFC 822, updated by RFC 1123
      ;; Sunday, 06-Nov-94 08:49:37 GMT   ; RFC 850, obsoleted by RFC 1036
      ;; Sun Nov  6 08:49:37 1994         ; ANSI C's asctime() format
      (handle-otherwise otherwise)))
