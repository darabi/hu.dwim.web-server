;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui.application
  :class hu.dwim.system
  :setup-readtable-function-name "hu.dwim.wui::setup-readtable"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain (sans advertising clause)"
  :description "Extension to the basic HTTP server to become an HTTP application server for the world wide web."
  :long-description "Provides application, session, frame, action and entry point abstractions."
  :depends-on (:hu.dwim.wui.http)
  :components ((:module "source"
                :components ((:module "application"
                              :components ((:file "action" :depends-on ("variables" "application" "frame"))
                                           (:file "application" :depends-on ("variables" "dojo" "session" "frame"))
                                           (:file "dojo")
                                           (:file "entry-point" :depends-on ("variables"))
                                           (:file "frame" :depends-on ("variables" "session"))
                                           (:file "session" :depends-on ("variables"))
                                           (:file "variables")))))))
