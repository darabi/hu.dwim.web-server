;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def (generic e) startup-server (server &key &allow-other-keys))

(def (generic e) shutdown-server (server &key &allow-other-keys))

(def (generic e) handle-request (thing request))

(def (generic e) startup-broker (broker)
  (:method (broker)
    ))

(def (generic e) shutdown-broker (broker)
  (:method (broker)
    ))

(def generic matches-request? (broker request)
  (:method (broker request)
    #f))

(def generic produce-response (broker request))
