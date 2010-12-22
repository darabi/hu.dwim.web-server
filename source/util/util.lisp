;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;;;;;
;;; Utils

(def (function io) maybe-make-xml-attribute (name value)
  (when value
    (make-xml-attribute name value)))

(deftype simple-ub8-vector (&optional (length '*))
  `(simple-array (unsigned-byte 8) (,length)))

(def (function i) coerce-to-simple-ub8-vector (vector &optional (length (length vector)))
  (if (and (typep vector 'simple-ub8-vector)
           (= length (length vector)))
      vector
      (aprog1
          (make-array length :element-type '(unsigned-byte 8))
        (replace it vector :end2 length))))

(def (function i) us-ascii-octets-to-string (vector)
  (coerce (babel:octets-to-string vector :encoding :us-ascii) 'simple-base-string))
(def (function i) string-to-us-ascii-octets (string)
  (babel:string-to-octets string :encoding :us-ascii))

(def (function i) iso-8859-1-octets-to-string (vector)
  (babel:octets-to-string vector :encoding :iso-8859-1))
(def (function i) string-to-iso-8859-1-octets (string)
  (babel:string-to-octets string :encoding :iso-8859-1))

(def (function i) utf-8-octets-to-string (vector)
  (babel:octets-to-string vector :encoding :utf-8))
(def (function i) string-to-utf-8-octets (string)
  (babel:string-to-octets string :encoding :utf-8))

(def generic encoding-name-of (thing)
  (:method ((self external-format))
    (encoding-name-of (external-format-encoding self)))

  (:method ((self babel-encodings:character-encoding))
    (babel-encodings:enc-name self)))

(def function get-bytes-allocated ()
  "Returns a monotonic counter of bytes allocated, preferable a per-thread value."
  #*((:sbcl
      ;; as of 1.0.22 this is still a global value shared by all threads
      (sb-ext:get-bytes-consed))
     (t
      #.(warn "~S is not implemented for your platform." 'get-bytes-allocated)
      0)))

(def (function io) make-adjustable-vector (initial-length &key (element-type t))
  (declare (type array-index initial-length))
  (make-array initial-length :adjustable #t :fill-pointer 0 :element-type element-type))

(def (function i) make-displaced-array (array &optional (start 0) (end (length array)))
  (make-array (- end start)
              :element-type (array-element-type array)
              :displaced-to array
              :displaced-index-offset start))

(def (function o) split-ub8-vector (separator line)
  (declare (type simple-ub8-vector line)
           (inline make-displaced-array))
  (iter outer ; we only need the outer to be able to collect a last chunk in the finally block of the inner loop
        (iter (with start = 0)
              (for end :upfrom 0)
              (for char :in-vector line)
              (declare (type fixnum start end))
              (when (= separator char)
                (in outer (collect (make-displaced-array line start end)))
                (setf start (1+ end)))
              (finally (in outer (collect (make-displaced-array line start)))))
        (while nil)))

(def (function i) is-lock-held? (lock)
  #*((:sbcl (debug-only (check-type lock sb-thread:mutex))
            (eq (sb-thread:mutex-owner lock) sb-thread:*current-thread*))
     (t #.(warn "~S is not implemented for your platform." 'is-lock-held?)
        (error "~S is not implemented for your platform." 'is-lock-held?))))

(def function mailto-href (email-address)
  (string+ "mailto:" email-address))

(def (function io) new-random-hash-table-key (hash-table key-length &key prefix)
  (iter (for key = (random-string key-length +ascii-alphabet+ prefix))
        (for (values value foundp) = (gethash key hash-table))
        (when (not foundp)
          (return key))))

(def (function io) insert-with-new-random-hash-table-key (hash-table value key-length &key prefix)
  (bind ((key (new-random-hash-table-key hash-table key-length :prefix prefix)))
    (setf (gethash key hash-table) value)
    (values key value)))

;;;;;;
;;; Debug on error

(def class* debug-context-mixin ()
  ((debug-on-error :type boolean :accessor nil)))

(def method debug-on-error? ((context debug-context-mixin) error)
  (if (slot-boundp context 'debug-on-error)
      (slot-value context 'debug-on-error)
      (call-next-method)))
