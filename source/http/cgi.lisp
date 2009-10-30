;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; cgi-serving-broker

;; TODO no, it's not an entry point, only a broker-with-path-prefix
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

;; TODO maybe this environment arg should be an alist instead of a list of "FOO=value" strings? or a hashtable?
;; TODO get rid of *application* usage, should work without applications
(def (function ed) handle-cgi-request (filename path-prefix &key environment)
  ;; TODO (assert (starts-with-subseq path-prefix (path-of (uri-of *request*))))
  ;; almost: "darcsweb/darcsweb.cgi" "/darcsweb/darcsweb.cgi"
  (bind ((filename (namestring filename))
         (temporary-file (filename-for-temporary-file "cgi")))
    (sb-ext:run-program filename nil
                        :wait #t
                        ;; TODO: revise according to http://hoohoo.ncsa.illinois.edu/cgi/env.html
                        :environment (append
                                      environment
                                      (list "GATEWAY_INTERFACE=CGI/1.1"
                                            "SERVER_SOFTWARE=hu.dwim.wui"
                                            (string+ "SERVER_NAME=" (host-of (uri-of *request*)))
                                            (string+ "SERVER_PROTOCOL=" (http-version-string-of *request*))
                                            (string+ "SERVER_PORT=" (or (port-of (uri-of *request*)) "80"))
                                            (string+ "REQUEST_METHOD=" (http-method-of *request*))
                                            (string+ "PATH_INFO=" (subseq (path-of (uri-of *request*))
                                                                          ;; TODO this 1+ is a serious KLUDGE until it's turned into a broker and cleaned up
                                                                          (1+ (length path-prefix))))
                                            (string+ "PATH_TRANSLATED=")
                                            (string+ "SCRIPT_NAME=" (aif (and (boundp '*application*)
                                                                              (symbol-value '*application*))
                                                                         (string+ (hu.dwim.wui::path-prefix-of it) path-prefix)
                                                                         path-prefix))
                                            (string+ "QUERY_STRING=" (query-of (uri-of *request*)))
                                            (string+ "REMOTE_HOST=")
                                            (string+ "REMOTE_ADDR=" (iolib:address-to-string *request-remote-host*))
                                            (string+ "REMOTE_USER=")
                                            (string+ "REMOTE_IDENT=")
                                            (string+ "AUTH_TYPE=")
                                            ;; TODO hrm, we parse the entire request unconditionally... what about the next two?
                                            (string+ "CONTENT_TYPE=")
                                            (string+ "CONTENT_LENGTH="))
                                      (iter (for (name . value) :in (headers-of *request*))
                                            (unless (member name '#.(list +header/content-length+
                                                                          +header/content-type+
                                                                          +header/authorization+
                                                                          +header/proxy-authorization+)
                                                            :test #'string=)
                                              (collect (string+ "HTTP_" (nstring-upcase (substitute #\_ #\- name)) "="
                                                                value)))))
                        :output temporary-file)
    (aprog1
        (make-raw-functional-response ()
          (bind ((stream (client-stream-of *request*)))
            (write-sequence #.(string-to-us-ascii-octets "HTTP/1.1 ") stream)
            (write-sequence (string-to-us-ascii-octets "200 OK") stream)
            (write-byte +space+ stream)
            (write-sequence (read-file-into-byte-vector temporary-file) stream)))
      (setf (cleanup-thunk-of it) (lambda ()
                                    (delete-file temporary-file))))))
