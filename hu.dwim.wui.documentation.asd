;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui.documentation
  :class hu.dwim.documentation-system
  :depends-on (:hu.dwim.wui.test)
  :components ((:module "documentation"
                :components ((:file "package")
                             (:file "wui" :depends-on ("package"))))))
