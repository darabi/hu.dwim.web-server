;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def (special-variable e :documentation "The SERVER associated with the currently processed HTTP REQUEST.")
  *server*)

;; TODO rename to *broker-stack*
(def (special-variable :documentation "Holds the broker path while processing the rules. Whenever a rule provides a new set of rules, it is pushed at the head of the *BROKERS* list.")
  *brokers*)

(def special-variable *matching-uri-path-element-stack* '()
  "A stack of currently matching path elements while request handling is going deeper and deeper in the broker tree.")

(def special-variable *matching-uri-path-element-stack/total-length* 0
  "Optimization; keeps the current value of (reduce #'+ *matching-uri-path-element-stack* :key #'length).")

(def special-variable *matching-uri-path-element-stack/remaining-path* nil
  "Optimization; keeps NIL or the current value of (subseq (path-of (uri-of request)) *matching-uri-path-element-stack/total-length*).")

(def function remaining-path-of-request-uri (&optional (request *request*))
  "While matching the request uri, returns the remaining part of the path of the uri that have not yet been matched."
  (or *matching-uri-path-element-stack/remaining-path*
      (setf *matching-uri-path-element-stack/remaining-path*
            (subseq (path-of (uri-of request)) *matching-uri-path-element-stack/total-length*))))

(def with-macro with-new-matching-uri-path-element (path)
  (check-type path string)
  (bind ((*matching-uri-path-element-stack* (cons path *matching-uri-path-element-stack*))
         (*matching-uri-path-element-stack/total-length* (+ (length path) *matching-uri-path-element-stack/total-length*))
         (*matching-uri-path-element-stack/remaining-path* nil))
    (-body-)))
