;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(deflogger wui ()
  :level (if *load-as-production-p* +info+ +debug+)
  :compile-time-level (if *load-as-production-p* +debug+ +dribble+)
  :appender (make-instance 'brief-stream-log-appender :stream *debug-io*))

(deflogger threads (wui))
(deflogger http (wui))
(deflogger rerl (wui))
(deflogger server (wui))
