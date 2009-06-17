;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def logger wui ()
  :level (if *load-as-production-p* +info+ +debug+)
  :compile-time-level (if *load-as-production-p* +debug+ +dribble+)
  :appender (make-instance 'brief-stream-log-appender :stream *debug-io*))

(def logger rerl (wui))

(def logger timer (wui))

(def logger http   (rerl))
(def logger app    (rerl))
(def logger l10n   (app))

(def logger server (rerl))
(def logger files  (server))

(def logger threads (wui))
