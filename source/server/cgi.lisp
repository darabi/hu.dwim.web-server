;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def class* cgi-broker (broker-with-path-prefix)
  ((cgi-file :type (or string pathname))
   (environment nil)))

(def (function e) make-cgi-broker (path-prefix cgi-file &key priority environment)
  (make-instance 'cgi-broker
                 :path-prefix path-prefix
                 :cgi-file cgi-file
                 :priority priority
                 :environment environment))

(def method produce-response ((self cgi-broker) request)
  (handle-cgi-request (cgi-file-of self) (path-prefix-of self) :environment (environment-of self)))

(def constant +static-cgi-environment+ '("GATEWAY_INTERFACE=CGI/1.1"
                                         "SERVER_SOFTWARE=hu.dwim.wui"))

(def function compute-cgi-environment (start-environment)
  (bind ((request-uri (uri-of *request*))
         (request-uri-path (path-of request-uri))
         (remaining-path (remaining-path-of-request-uri))
         (script-name (subseq request-uri-path (length remaining-path))))
    (debug-only (assert (equal script-name (apply #'concatenate 'string (reverse *matching-uri-path-element-stack*)))))
    (debug-only (assert (string= (string+ script-name remaining-path) request-uri-path)))
    (nconc (loop
             :for (name . value) :in start-environment
             :collect (string+ name "=" value))
           (copy-list +static-cgi-environment+)
           (optional-list
            (string+ "SERVER_NAME="     (host-of request-uri))
            (string+ "SERVER_PROTOCOL=" (http-version-string-of *request*))
            (string+ "SERVER_PORT=" (or (port-of request-uri) "80"))
            (string+ "REQUEST_METHOD="  (http-method-of *request*))
            (string+ "PATH_INFO="       remaining-path)
            ;; (string+ "PATH_TRANSLATED=")
            (string+ "SCRIPT_NAME="     script-name)
            (string+ "QUERY_STRING="    (query-of request-uri))
            (awhen (nth-value 2 (iolib.sockets:lookup-hostname *request-remote-host*))
              (string+ "REMOTE_HOST=" it))
            (string+ "REMOTE_ADDR="     (iolib:address-to-string *request-remote-host*))
            ;; (string+ "REMOTE_USER=")
            ;; (string+ "REMOTE_IDENT=")
            ;; (string+ "AUTH_TYPE=")
            ;; TODO hrm, currently we parse the entire request unconditionally... what about the next two?
            ;; (string+ "CONTENT_TYPE=")
            ;; (string+ "CONTENT_LENGTH=")
            ))))

(def (function ed) handle-cgi-request (cgi-file path-prefix &key environment)
  (bind ((cgi-file (truename cgi-file))
         (temporary-file (filename-for-temporary-file "cgi")))
    (cgi.debug "Executing CGI file ~S, matched on path-prefix ~S" cgi-file path-prefix)
    (bind ((final-environment (compute-cgi-environment environment)))
      (cgi.dribble "Executing CGI file ~S, matched on path-prefix ~S, temporary file will be ~S.~% * Input environment:~%     ~S~% * Final environment:~%     ~S. " cgi-file path-prefix temporary-file environment final-environment)
      (sb-ext:run-program cgi-file nil
                          :wait #t
                          ;; TODO: revise according to http://hoohoo.ncsa.illinois.edu/cgi/env.html
                          :environment final-environment
                          :output temporary-file))
    (aprog1
        (make-raw-functional-response ()
          (bind ((stream (client-stream-of *request*)))
            (write-sequence #.(string-to-us-ascii-octets "HTTP/1.1 ") stream)
            (write-sequence (string-to-us-ascii-octets "200 OK") stream)
            (write-byte +space+ stream)
            (write-sequence (read-file-into-byte-vector temporary-file) stream)))
      (setf (cleanup-thunk-of it) (lambda ()
                                    (delete-file temporary-file))))))
