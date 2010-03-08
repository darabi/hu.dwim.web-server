;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Project

(def (namespace e) project (&rest args &key &allow-other-keys)
  `(make-instance 'project :name ',-name- ,@args))

(def (class* e) project ()
  ((name :type string)
   (path :type pathname)
   (description nil)))

;;;;;;
;;; Util

(def (function e) project-system-name (project)
  (bind ((path (path-of project)))
    (or (pathname-name path)
        (last-elt (pathname-directory path)))))

(def (function e) project-licence-pathname (project)
  (flet ((try (filename)
           (bind ((licence-pathname (merge-pathnames filename (path-of project))))
             (when (probe-file licence-pathname)
               (return-from project-licence-pathname licence-pathname)))))
    (try "LICENCE")
    (try "LICENSE")
    (try "COPYING")
    (try "COPYRIGHT")))

(def (function e) project-name (pathname)
  (bind ((project-name (iolib.pathnames:file-path-file pathname)))
    (or (find-symbol (string-upcase project-name) :keyword)
        project-name)))
