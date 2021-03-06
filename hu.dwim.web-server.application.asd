;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.web-server.application
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Application logic (sessions, etc) based on hu.dwim.web-server."
  :long-description "Provides application, session, frame, action and entry point abstractions over the basic hu.dwim.web-server infrastrucutre."
  :depends-on (:hu.dwim.web-server)
  :components ((:module "source"
                :components ((:module "application"
                              :components ((:file "action" :depends-on ("api" "application-response" "frame"))
                                           (:file "api" :depends-on ("conditions" "variables"))
                                           (:file "application" :depends-on ("api" "dojo" "session" "frame"))
                                           (:file "application-response" :depends-on ("application" "session-logic"))
                                           (:file "conditions")
                                           (:file "dojo" :depends-on ("api"))
                                           (:file "entry-point" :depends-on ("variables" "session-logic"))
                                           (:file "extjs")
                                           (:file "frame" :depends-on ("api" "session"))
                                           (:file "login" :depends-on ("session-logic" "entry-point"))
                                           (:file "login-entry-point" :depends-on ("login" "session-logic"))
                                           (:file "sencha-touch")
                                           (:file "session" :depends-on ("api"))
                                           (:file "session-logic" :depends-on ("session" "application"))
                                           (:file "variables")))))))

(defmethod perform :before ((op hu.dwim.asdf:develop-op) (system (eql (find-system :hu.dwim.web-server.application))))
  (hu.dwim.asdf:develop-system :hu.dwim.web-server))
