;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def localization en
  (render-frame-out-of-sync-error (refresh-href new-frame-href &key &allow-other-keys)
    <div
      <p "Browser window went out of sync with the server...">
      <p "Please avoid using the " <i "Back">
         " button of your browser and/or opening links in new windows by copy-pasting URL's or using the " <i "Open in new window">
         " feature of your browser. To achieve the same effect, you can use the navigation actions provided by the application.">
      <p <a (:href ,refresh-href) "Bring me back to the application">>
      <p <a (:href ,new-frame-href) "Reset my view of the application">>>)
  (render-application-internal-error-page (&key administrator-email-address &allow-other-keys)
    <div
      <p "The developers will be notified about this error and they will hopefully fix it soon.">
      ,(when administrator-email-address
         <p "You may contact the administrators at this email address: "
             <a (:href ,(mailto-href administrator-email-address)) ,administrator-email-address>>)>)
  (error.internal-server-error.title "Internal server error")
  (error.internal-server-error.message "An internal server error has occured, we are sorry for the inconvenience.")
  )

(def localization en
  (class-name.application "application")
  (class-name.session "session")
  (class-name.frame "frame")
  (class-name.action "action")
  (class-name.entry-point "entry point")

  (slot-name.debug-on-error "debug on error")
  (slot-name.processed-request-count "processed request count")
  (slot-name.priority "priority")
  (slot-name.path-prefix "path prefix")
  (slot-name.number-of-requests-to-sessions "number of requests to sessions")
  (slot-name.default-uri-scheme "default uri scheme")
  (slot-name.default-locale "default locale")
  (slot-name.supported-locales "supported locales")
  (slot-name.default-timezone "default timezone")
  (slot-name.session-class "session class")
  (slot-name.session-timeout "session timeout")
  (slot-name.frame-timeout "frame timeout")
  (slot-name.sessions-last-purged-at "sessions last purged at")
  (slot-name.maximum-number-of-sessions "maximum number of sessions")
  (slot-name.session-id->session "session map")
  (slot-name.administrator-email-address "administrator email address")
  (slot-name.running-in-test-mode "running in test mode")
  (slot-name.compile-time-debug-client-side "compile time debug client side")
  (slot-name.ajax-enabled "ajax enabled")
  (slot-name.dojo-skin-name "dojo skin name")
  (slot-name.dojo-file-name "dojo file name")
  (slot-name.dojo-directory-name "dojo directory name"))
