;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Project

(def (namespace e) project (&rest args &key &allow-other-keys)
  `(make-instance 'project ,@args))

(def (class* e) project ()
  ((name nil :type string)
   (path :type pathname)
   (description)))

(def constructor project
  (bind (((:slots name path) -self-))
    (unless name
      (setf name (or (pathname-name path)
                     (last-elt (pathname-directory path)))))))

;;;;;;
;;; Util

(def (function e) find-project-by-path (pathname)
  (iterate-project-namespace (lambda (name project)
                               (declare (ignore name))
                               (when (equal (path-of project) pathname)
                                 (return-from find-project-by-path project)))))

(def (function e) project-system-name (project)
  (name-of project))

(def (function e) project-licence-pathname (project)
  (flet ((try (filename)
           (bind ((licence-pathname (merge-pathnames filename (path-of project))))
             (when (probe-file licence-pathname)
               (return-from project-licence-pathname licence-pathname)))))
    (try "LICENCE")
    (try "LICENSE")
    (try "COPYING")
    (try "COPYRIGHT")))
