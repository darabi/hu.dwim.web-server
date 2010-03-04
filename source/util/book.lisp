;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Namespace

(def (namespace e) book ((&rest args &key &allow-other-keys) &body contents)
  `(book (:name ',-name- ,@args) ,@contents))

(def (function e) find-user-guide (package-name)
  (awhen (find-symbol "USER-GUIDE"  package-name)
    (find-book it)))

;;;;;;
;;; Text

(def class* text ()
  ((contents nil :type list)))

;;;;;;
;;; Hyperlink

(def class* hyperlink ()
  ((uri :type (or string uri))
   (content :type t)))

(def (macro e) hyperlink (uri &optional text)
  `(make-instance 'hyperlink :uri (parse-uri ,uri) :content ,text))

(def (macro e) hyperlink/wikipedia (relative-uri &optional (text relative-uri))
  `(hyperlink (string+ "http://wikipedia.org/wiki/" ,relative-uri) ,text))

;;;;;;
;;; Title mixin

(def class* title-mixin ()
  ((title nil :type string)))

;;;;;;
;;; Book

(def class* book (text title-mixin)
  ((name :type symbol)
   (authors nil :type list))
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
