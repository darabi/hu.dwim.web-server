;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Projects

(def special-variable *projects* (make-hash-table))

(def (function e) find-project (name)
  (gethash name *projects*))

(def (function e) (setf find-project) (new-value name)
  (setf (gethash name *projects*) new-value))


;;;;;;
;;; Project

(def (class* e) project ()
  ((name nil :type string)
   (path :type pathname)))

(def constructor project
  (bind (((:slots name path) -self-))
    (unless name
      (setf name (or (pathname-name path)
                     (last-elt (pathname-directory path)))))))
