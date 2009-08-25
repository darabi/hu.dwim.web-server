;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.wui.component.test
  :class hu.dwim.test-system
  :setup-readtable-function-name "hu.dwim.wui.test::setup-readtable"
  :package-name :hu.dwim.wui.test
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain (sans advertising clause)"
  :depends-on (:hu.dwim.reader
               :hu.dwim.wui.application.test
               :hu.dwim.wui.component
               :hu.dwim.wui+cl-graph+cl-typesetting)
  :components ((:module "test"
                :components ((:file "component")))))
