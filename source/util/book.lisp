;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Books

(def special-variable *books* (make-hash-table))

(def (function e) find-book (name)
  (gethash name *books*))

(def (function e) (setf find-book) (new-value name)
  (setf (gethash name *books*) new-value))

;;;;;;
;;; Text

(def class* text ()
  ((contents nil :type list)))

(def class* hyperlink (text)
  ((uri :type (or string uri))))

(def (macro e) link (uri &optional text)
  `(make-instance 'hyperlink :uri (parse-uri ,uri) :contents ,text))

;;;;;;
;;; Title mixin

(def class* title-mixin ()
  ((title nil :type string)))

;;;;;;
;;; Book

(def class* book (text title-mixin)
  ((authors nil :type list))
  (:documentation "A BOOK is a mostly textual description of something."))

(def (macro e) book ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'book ,@args :contents (list ,@contents)))

;;;;;;
;;; Chapter

(def class* chapter (text title-mixin)
  ())

(def (macro e) chapter ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'chapter ,@args :contents (list ,@contents)))

;;;;;;
;;; Paragraph

(def class* paragraph (text)
  ())

(def (macro e) paragraph ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'paragraph ,@args :contents (list ,@contents)))

;;;;;;
;;; Definer

(def (definer e :available-flags "e") book (name (&rest args &key &allow-other-keys) &body contents)
  `(setf (find-book ',name) (book ,args ,@contents)))
