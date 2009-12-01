;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui
  :class hu.dwim.system
  :description "WUI with all its extensions."
  :depends-on (:hu.dwim.wui.component))

(defmethod perform :after ((op develop-op) (system (eql (find-system :hu.dwim.wui))))
  (let ((*package* (find-package :hu.dwim.wui)))
    (eval
     (read-from-string
      "(progn
         ;; set dojo to the latest available
         (setf *dojo-directory-name* (find-latest-dojo-directory-name (asdf:system-relative-pathname :hu.dwim.wui \"www/\")))
         (setf (log-level 'wui) +debug+)
         (setf *debug-on-error* t))")))
  (warn "Set WUI log level to +debug+; enabled server-side debugging, set *dojo-directory-name*"))
