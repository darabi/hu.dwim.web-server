;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui.application.test
  :class hu.dwim.test-system
  :package-name :hu.dwim.wui.test
  :depends-on (:hu.dwim.wui.application
               :hu.dwim.wui.http.test)
  :components ((:module "test"
                :components ((:file "application" :depends-on ("authentication" "echo" "parameter" "performance" "session"))
                             (:file "authentication")
                             (:file "echo")
                             (:file "parameter")
                             (:file "performance")
                             (:file "session")))))
