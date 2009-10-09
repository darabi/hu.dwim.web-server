;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; cgi-serving-broker

(def class* cgi-serving-broker (path-prefix-entry-point)
  ((filename :type pathname)
   (environment nil))
  (:metaclass funcallable-standard-class))

(def (function e) make-cgi-serving-broker (path-prefix filename &key priority environment)
  (make-instance 'cgi-serving-broker
                 :path-prefix path-prefix
                 :filename filename
                 :priority priority
                 :environment environment
                 :handler 'handle-cgi-request))

(def method produce-response ((self cgi-serving-broker) request)
  (funcall (handler-of self) (filename-of self) (path-prefix-of self) :environment (environment-of self)))

(def (function e) handle-cgi-request (filename path-prefix &key environment)
  (bind ((filename (namestring filename))
         (tmp (filename-for-temporary-file)))
    (sb-ext:run-program filename nil
                        :wait #t
                        ;; TODO: revise according to http://hoohoo.ncsa.illinois.edu/cgi/env.html
                        :environment (append
                                      environment
                                      (list "GATEWAY_INTERFACE=CGI/1.1"
                                            "SERVER_SOFTWARE=hu.dwim.wui"
                                            (string+ "SERVER_NAME=" (machine-instance))
                                            (string+ "SERVER_PROTOCOL=" (http-version-string-of *request*))
                                            (string+ "SERVER_PORT=" "8080")
                                            (string+ "REQUEST_METHOD=" (http-method-of *request*))
                                            (string+ "PATH_INFO=")
                                            (string+ "PATH_TRANSLATED=")
                                            (string+ "SCRIPT_NAME=" (string+ (hu.dwim.wui::path-prefix-of *application*) path-prefix))
                                            (string+ "QUERY_STRING=" (query-of (uri-of *request*)))
                                            (string+ "REMOTE_HOST=")
                                            (string+ "REMOTE_ADDR=")
                                            (string+ "REMOTE_USER=")
                                            (string+ "REMOTE_IDENT=")
                                            (string+ "AUTH_TYPE=")
                                            (string+ "CONTENT_TYPE=")
                                            (string+ "CONTENT_LENGTH=")))
                        :output tmp)
    (make-raw-functional-response ()
      (bind ((stream (client-stream-of *request*)))
        (write-sequence #.(string-to-us-ascii-octets "HTTP/1.1 ") stream)
        (write-sequence (string-to-us-ascii-octets "200 OK") stream)
        (write-byte +space+ stream)
        (write-sequence (read-file-into-byte-vector tmp) stream)))))
