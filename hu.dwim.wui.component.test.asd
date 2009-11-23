;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui.component.test
  :class hu.dwim.test-system
  :package-name :hu.dwim.wui.test
  :depends-on (:hu.dwim.graphviz
               :hu.dwim.wui.application.test
               :hu.dwim.wui.component
               :hu.dwim.wui+cl-graph+cl-typesetting
               :hu.dwim.wui+hu.dwim.reader+hu.dwim.syntax-sugar)
  :components ((:module "test"
                :components ((:file "component")))))
