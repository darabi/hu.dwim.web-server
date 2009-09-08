;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui.application.test
  :class hu.dwim.test-system
  :setup-readtable-function-name "hu.dwim.wui.test::setup-readtable"
  :package-name :hu.dwim.wui.test
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain (sans advertising clause)"
  :depends-on (:hu.dwim.wui.application
               :hu.dwim.wui.http.test)
  :components ((:module "test"
                :components ((:file "application")))))
