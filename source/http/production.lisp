;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; HTTP port

(def (constant e) +default-http-server-port+ 8080)

(def (constant e) +http-server-port-command-line-option+
  '("http-server-port"
    :type integer
    :initial-value #.+default-http-server-port+
    :documentation "The HTTP port where the server will be listening for incoming requests"))

(def (function e) process-http-server-port-command-line-argument (arguments server)
  (when-bind http-server-port (getf arguments :http-server-port)
    (setf (port-of (find +default-http-server-port+ (listen-entries-of server) :key #'port-of)) http-server-port)))
