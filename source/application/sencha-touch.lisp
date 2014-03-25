;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;;;;;
;;; Variables

(def (special-variable e) *sencha-touch-skin-name*)

(def (special-variable e) *sencha-touch-file-name*)

(def (special-variable e) *sencha-touch-directory-name*)

;;;;;;
;;; application-with-sencha-touch-support

(def (class* ea) application-with-sencha-touch-support (application)
  ((skin-name "tundra") ;; FIXME: currently unused
   (production-file-name "sencha-touch.js")
   (debug-file-name "sencha-touch-debug-w-comments.js")
   (directory-name (or (find-latest-js-library "sencha-touch" "For building sencha-touch, cf. hu.dwim.web-server/etc/build-sencha-touch.sh")
                                    "sencha-touch/"))))

(def method startup-broker :after ((self application-with-sencha-touch-support))
  (unless (directory-name-of self)
    (warn "The ~S slot of application ~A is not initialized by the time the server was started! Please refer to the install guide (e.g. on http://dwim.hu) or the sources for details on how to install Sencha Touch."
          'directory-name self)))

(def method call-in-application-environment :around ((application application-with-sencha-touch-support) session thunk)
  (bind ((*sencha-touch-skin-name* (or (skin-name-of application)
                                       *sencha-touch-skin-name*))
         (*sencha-touch-file-name* (if (running-in-test-mode? application)
                                       (debug-file-name-of application)
                                       (production-file-name-of application)))
         (*sencha-touch-directory-name* (or (directory-name-of application)
                                            *sencha-touch-directory-name*)))
    (call-next-method)))

