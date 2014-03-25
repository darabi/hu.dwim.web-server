;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;;;;;
;;; Variables

(def (special-variable e) *extjs-skin-name*)

(def (special-variable e) *extjs-file-name*)

(def (special-variable e) *extjs-directory-name*)

;;;;;;
;;; application-with-ext-js-support

(def (class* ea) application-with-extjs-support (application)
  ((extjs-skin-name "dummy")
   (extjs-file-name "ext-all.js")
   (extjs-directory-name (or (find-latest-js-library "ext" "Download and put extjs in www/libraries/ext-4.x")
                                    "ext/"))))

(def method startup-broker :after ((self application-with-extjs-support))
  (unless (extjs-directory-name-of self)
    (warn "The ~S slot of application ~A is not initialized by the time the server was started! Please refer to the install guide (e.g. on http://dwim.hu) or the sources for details on how to install Sencha Touch."
          'extjs-directory-name self)))

(def method call-in-application-environment :around ((application application-with-extjs-support) session thunk)
  (bind ((*extjs-skin-name* (or (extjs-skin-name-of application)
                                       *extjs-skin-name*))
         (*extjs-file-name* (or (extjs-file-name-of application)
                                       *extjs-file-name*))
         (*extjs-directory-name* (or (extjs-directory-name-of application)
                                            *extjs-directory-name*)))
    (call-next-method)))

