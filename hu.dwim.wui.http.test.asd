;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui.http.test
  :class hu.dwim.test-system
  :package-name :hu.dwim.wui.test
  :depends-on (:drakma
               :hu.dwim.stefil+hu.dwim.def+swank
               :hu.dwim.wui.http
               :hu.dwim.wui+swank)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "environment" :depends-on ("package"))
                             (:file "http" :depends-on ("environment"))))))
