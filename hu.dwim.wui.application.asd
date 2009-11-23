;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui.application
  :class hu.dwim.system
  :description "Application logic (sessions, etc) based on :hu.dwim.wui.http"
  :long-description "Provides application, session, frame, action and entry point abstractions over the simple :hu.dwim.wui.http infrastrucutre."
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
