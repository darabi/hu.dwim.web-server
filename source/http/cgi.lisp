;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; cgi-broker

(def class* cgi-broker (path-prefix-entry-point)
  ((filename :type pathname)
   (cgi-path :type string))
  (:metaclass funcallable-standard-class))

(def (function e) make-cgi-broker (path-prefix filename &key priority cgi-path)
  (make-instance 'cgi-broker
                 :path-prefix path-prefix
                 :filename filename
                 :priority priority
                 :handler 'handle-cgi-request
                 :cgi-path cgi-path))

(def method produce-response ((entry-point cgi-broker) request)
  (funcall (handler-of entry-point) (filename-of entry-point) (cgi-path-of entry-point)))

;; TODO: error handling, etc.
(def (function e) handle-cgi-request (filename cgi-path)
  (bind ((stream (client-stream-of *request*))
         (filename (namestring filename))
         (tmp (filename-for-temporary-file)))
    (sb-ext:run-program filename nil
                        :wait #t
                        ;; TODO: revise according to http://hoohoo.ncsa.illinois.edu/cgi/env.html
                        :environment (list "GATEWAY_INTERFACE=CGI/1.1"
                                           "SERVER_SOFTWARE=hu.dwim.wui"
                                           (string+ "SERVER_NAME=" (machine-instance))
                                           (string+ "SERVER_PROTOCOL=" (http-version-string-of *request*))
                                           (string+ "SERVER_PORT=" "8080")
                                           (string+ "REQUEST_METHOD=" (http-method-of *request*))
                                           (string+ "PATH_INFO=" (path-of (uri-of *request*)))
                                           (string+ "PATH_TRANSLATED=")
                                           (string+ "SCRIPT_NAME=" cgi-path)
                                           (string+ "QUERY_STRING=" (query-of (uri-of *request*)))
                                           (string+ "REMOTE_HOST=")
                                           (string+ "REMOTE_ADDR=")
                                           (string+ "REMOTE_USER=")
                                           (string+ "REMOTE_IDENT=")
                                           (string+ "AUTH_TYPE=")
                                           (string+ "CONTENT_TYPE=")
                                           (string+ "CONTENT_LENGTH="))
                        :output tmp)
    (write-sequence #.(string-to-us-ascii-octets "HTTP/1.1 ") stream)
    (write-sequence (string-to-us-ascii-octets "200 OK") stream)
    (write-byte +space+ stream)
    (write-sequence (read-file-into-byte-vector tmp) stream)))
