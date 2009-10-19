;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def logger wui () :appender (make-instance 'brief-stream-appender :stream *debug-io*))

(def logger rerl (wui))

(def logger timer (wui))

(def logger http   (rerl))

(def logger app    (rerl))

(def logger l10n   (app))

(def logger server (rerl))

(def logger files  (server))

(def logger threads (wui))
