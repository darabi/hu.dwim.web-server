;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; CSV format

(def constant +csv-quote-char+ #\")

(def constant +csv-value-separator+ #\Tab)

(def constant +csv-line-separator+ #\NewLine)

(def special-variable *csv-stream*)

(def function escape-csv-char? (char)
  (member char '(#.+csv-value-separator+ #.+csv-line-separator+ #.+csv-quote-char+) :test #'char=))

(def function escape-csv-value (value)
  (with-output-to-string (escaped-value)
    (write-char +csv-quote-char+ escaped-value)
    (iter (for char :in-sequence value)
          (if (char= char +csv-quote-char+)
              (iter (repeat 2)
                    (write-char +csv-quote-char+ escaped-value))
              (write-char char escaped-value)))
    (write-char +csv-quote-char+ escaped-value)))

(def function write-csv-value (value)
  (if (or (string= value "")
          (whitespace? (elt value 0))
          (whitespace? (elt value (1- (length value))))
          (find-if #'escape-csv-char? value))
      (write-string (escape-csv-value value) *csv-stream*)
      (write-string value *csv-stream*)))

(def function write-csv-value-separator ()
  (write-char +csv-value-separator+ *csv-stream*))

(def function write-csv-line-separator ()
  (write-char +csv-line-separator+ *csv-stream*))

(def function write-csv-line (elements)
  (iter (for element :in elements)
        (unless (first-iteration-p)
          (write-csv-value-separator))
        (write-csv-element element)))

(def function write-csv-separated-elements (separator elements)
  (iter (for element :in elements)
        (unless (first-iteration-p)
          (write-char separator *csv-stream*))
        (write-csv-element element)))

(def generic write-csv-element (element)
  (:method ((element number))
    (write-csv-value (princ-to-string element)))

  (:method ((element string))
    (write-csv-value element)))
