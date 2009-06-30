;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Books

(def special-variable *books* (make-hash-table))

(def (function e) find-book (name)
  (gethash name *books*))

(def (function e) (setf find-book) (new-value name)
  (setf (gethash name *books*) new-value))

;;;;;;
;;; Book element

(def class* book-element ()
  ((parent-book-element nil :type book-element)
   (contents nil :type list)))

(def function setf-parent-book-elements (book)
  (labels ((recurse (parent)
             (dolist (slot (class-slots (class-of parent)))
               (bind ((value (slot-value-using-class (class-of parent) parent slot)))
                 (typecase value
                   (list (dolist (child value)
                           (when (typep child 'book-element)
                             (setf (parent-book-element-of child) parent)
                             (recurse child)))))))))
    (recurse book))
  book)

;;;;;;
;;; Title mixin

(def class* title-mixin ()
  ((title nil :type string)))

;;;;;;
;;; Book

(def class* book (book-element title-mixin)
  ((authors nil :type list))
  (:documentation "A BOOK is a mostly textual description of something."))

(def (macro e) book ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'book ,@args :contents (list ,@contents)))

;;;;;;
;;; Chapter

(def class* chapter (book-element title-mixin)
  ())

(def (macro e) chapter ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'chapter ,@args :contents (list ,@contents)))

;;;;;;
;;; Paragraph

(def class* paragraph (book-element)
  ())

(def (macro e) paragraph ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'paragraph ,@args :contents (list ,@contents)))

;;;;;;
;;; Definer

(def (definer e :available-flags "e") book (name (&rest args &key &allow-other-keys) &body contents)
  `(setf (find-book ',name) (setf-parent-book-elements (book ,args ,@contents))))
