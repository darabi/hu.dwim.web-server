;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.wui.application
  :class hu.dwim.system
  :description "Application logic (sessions, etc) based on hu.dwim.wui.http."
  :long-description "Provides application, session, frame, action and entry point abstractions over the simple hu.dwim.wui.http infrastrucutre."
  :depends-on (:hu.dwim.wui.http)
  :components ((:module "source"
                :components ((:module "application"
                              :components ((:file "action" :depends-on ("api" "application" "frame"))
                                           (:file "api" :depends-on ("conditions" "variables"))
                                           (:file "application" :depends-on ("api" "dojo" "session" "frame"))
                                           (:file "conditions")
                                           (:file "dojo" :depends-on ("api"))
                                           (:file "entry-point" :depends-on ("variables" "session-logic"))
                                           (:file "frame" :depends-on ("api" "session"))
                                           (:file "login-entry-point" :depends-on ("session-logic" "entry-point"))
                                           (:file "session" :depends-on ("api"))
                                           (:file "session-logic" :depends-on ("session" "application"))
                                           (:file "variables")))))))
