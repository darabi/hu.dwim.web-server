;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; API

(def (generic e) serialize/human-readable (object)
  (:documentation "Returns a human readable string representing OBJECT. The invariant (EQ STRING (SERIALIZE/HUMAN-READABLE (DESERIALIZE/HUMAN-READABLE STRING))) holds true."))

(def (generic e) deserialize/human-readable (string)
  (:documentation "Returns an object represented by the human readable STRING. The invariant (EQ OBJECT (DESERIALIZE/HUMAN-READABLE (SERIALIZE/HUMAN-READABLE OBJECT))) holds."))

;;;;;;
;;; Make

(def constant +human-readable-string-separator-character+ #\/)

(def function merge-human-readable-string (&rest elements)
  (with-output-to-string (stream)
    (iter (for element :in elements)
          (unless (first-iteration-p)
            (write-char +human-readable-string-separator-character+ stream))
          (write-string element stream))))

(def method serialize/human-readable (object)
  (do-namespace-namespace (name namespace)
    (maphash (lambda (key value)
               (when (eq value object)
                 (return-from serialize/human-readable (merge-human-readable-string (serialize/human-readable nil)
                                                                                    (symbol-name name)
                                                                                    (fully-qualified-symbol-name key)))))
             namespace)))

(def method serialize/human-readable ((context null))
  "")

(def method serialize/human-readable ((context (eql :file)))
  (merge-human-readable-string (serialize/human-readable nil) "FILE"))

(def method serialize/human-readable ((context (eql :class)))
  (merge-human-readable-string (serialize/human-readable nil) "CLASS"))

(def method serialize/human-readable ((context (eql :function)))
  (merge-human-readable-string (serialize/human-readable nil) "FUNCTION"))

(def method serialize/human-readable ((pathname pathname))
  (merge-human-readable-string (serialize/human-readable :file)
                               (namestring pathname)))

(def method serialize/human-readable ((function function))
  (merge-human-readable-string (serialize/human-readable :function)
                               (fully-qualified-symbol-name (swank-backend:function-name function))))

(def method serialize/human-readable ((class class))
  (merge-human-readable-string (serialize/human-readable :class)
                               (fully-qualified-symbol-name (class-name class))))

(def method serialize/human-readable ((slot direct-slot-definition))
  (merge-human-readable-string (serialize/human-readable (slot-value slot 'sb-pcl::%class))
                               "DIRECT-SLOT"
                               (fully-qualified-symbol-name (slot-definition-name slot))))

(def method serialize/human-readable ((slot effective-slot-definition))
  (merge-human-readable-string (serialize/human-readable (slot-value slot 'sb-pcl::%class))
                               "EFFECTIVE-SLOT"
                               (fully-qualified-symbol-name (slot-definition-name slot))))

;;;;;;
;;; Lookup

(def function split-human-readable-string (string)
  (split-sequence:split-sequence +human-readable-string-separator-character+ string :remove-empty-subseqs t))

(def generic deserialize/human-readable-in-context (context string))

(def method deserialize/human-readable ((string string))
  (deserialize/human-readable (split-human-readable-string string)))

(def method deserialize/human-readable ((strings list))
  (deserialize/human-readable-in-context nil strings))

(def method deserialize/human-readable-in-context (context (string string))
  (deserialize/human-readable-in-context context (split-human-readable-string string)))

(def method deserialize/human-readable-in-context (context (string null))
  context)

(def method deserialize/human-readable-in-context ((context null) (string null))
  nil)

(def method deserialize/human-readable-in-context ((context null) (strings cons))
  (deserialize/human-readable-in-context (switch ((car strings) :test #'equalp)
                                           ("FILE"
                                            :file)
                                           ("CLASS"
                                            :class)
                                           ("FUNCTION"
                                            :function)
                                           (t
                                            (when (length= 2 strings)
                                              (do-namespace-namespace (name namespace)
                                                (maphash (lambda (key value)
                                                           (when (equal strings (list (symbol-name name) (fully-qualified-symbol-name key)))
                                                             (return-from deserialize/human-readable-in-context value)))
                                                         namespace)))))
                                         (cdr strings)))

(def method deserialize/human-readable-in-context ((context (eql :file)) (strings cons))
  (pathname (car strings)))

(def method deserialize/human-readable-in-context ((context (eql :class)) (strings cons))
  (deserialize/human-readable-in-context (find-class (find-fully-qualified-symbol (car strings)))
                                         (cdr strings)))

(def method deserialize/human-readable-in-context ((context (eql :function)) (strings cons))
  (fdefinition (find-fully-qualified-symbol (car strings))))

(def method deserialize/human-readable-in-context ((class class) (strings cons))
  (deserialize/human-readable-in-context (eswitch ((car strings) :test #'equalp)
                                           ("DIRECT-SLOT"
                                            (class-direct-slots class))
                                           ("EFFECTIVE-SLOT"
                                            (class-slots class)))
                                         (cdr strings)))

(def method deserialize/human-readable-in-context ((elements list) (strings cons))
  (find (car strings) elements :key [fully-qualified-symbol-name (slot-definition-name !1)] :test #'equal))
