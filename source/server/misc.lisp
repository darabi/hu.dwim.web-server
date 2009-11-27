;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def function trace-request-handling ()
  (trace broker/default-handler
         handle-request
         call-with-new-matching-uri-path-element
         remaining-path-of-request-uri
         call-if-matches-request
         produce-response
         query-brokers-for-response
         iterate-brokers-for-response
         make-file-serving-response-for-query-path
         make-file-serving-response-for-directory-entry
         handle-cgi-request
         ))

;;;;;;
;;; Server status

(def (function e) make-server-status-response ()
  (disallow-response-caching
   (make-byte-vector-response ((+header/content-type+ +utf-8-plain-text-content-type+))
     (write-server-status t))))

(def (function e) write-server-status (stream)
  (bind ((*print-pretty* #f)
         (start (local-time:now)))
    (format stream "Server:                                     ~A~%" *server*)
    (format stream "Current time:                               ~A~%" (local-time:format-timestring nil start))
    (format stream "Startup time:                               ~A~%" (local-time:format-timestring nil (started-at-of *server*)))
    (format stream "Uptime:                                     ~A days~%" (coerce (/ (local-time::timestamp-difference start (started-at-of *server*)) (* 24 60 60)) 'float))
    (format stream "Number of workers:                          ~A~%" (length (workers-of *server*)))
    (format stream "Number of workers occupied:                 ~A~%" (occupied-worker-count-of *server*))
    (format stream "Number of requests processed:               ~A~%" (processed-request-counter-of *server*))
    (format stream "Number of gracefully aborted requests:      ~A~%" (gracefully-aborted-request-count-of *server*))
    (format stream "Number of failed requests:                  ~A~%" (failed-request-count-of *server*))
    (format stream "Number of client connection resets:         ~A~%" (client-connection-reset-count-of *server*))
    (format stream "Number of live web sessions:                ~A~%" (iter (for broker :in (brokers-of *server*))
                                                                            ;; FIXME application is not yet defined here. sometimes it errors while loading...
                                                                            (when (typep broker 'application)
                                                                              (summing (hash-table-count (session-id->session-of broker))))))
    (terpri)
    (awhen (and (boundp '*application*)
                (symbol-value '*application*))
      (format stream "Application:                                ~A~%" it)
      (format stream "Number of requests to valid sessions:       ~A~%" (requests-to-sessions-count-of it))
      (format stream "Sessions last purged at:                    ~,2F seconds since boot~%" (coerce (sessions-last-purged-at-of it) 'float))
      (terpri))
    (format stream "Heap usage:                                 ~,2F MBytes~%" (/ (sb-kernel::dynamic-usage) 1024d0 1024d0))
    (format stream "Number of threads running:                  ~A~%" (length (sb-thread::list-all-threads)))
    ;;(format stream "Maximum heap size: ~,2F MB~%" (/ (sb-kernel::dynamic-) 1024d0 1024d0))
    (format stream "Elapsed time rendering this response:       ~,4F seconds~%" (local-time:timestamp-difference (local-time:now) start))))

;;;;;;
;;; user-agent

(def (load-time-constant e) +chrome-version-scanner+    (cl-ppcre:create-scanner "Chrome/([0-9]{1,}\.[0-9]{0,})"))
(def (load-time-constant e) +opera-version-scanner+     (cl-ppcre:create-scanner "Opera/([0-9]{1,}\.[0-9]{0,})"))
(def (load-time-constant e) +konqueror-version-scanner+ (cl-ppcre:create-scanner "Konqueror/([0-9]{1,}\.[0-9]{0,})"))
(def (load-time-constant e) +safari-version-scanner+    (cl-ppcre:create-scanner "Safari/([0-9]{1,}\.[0-9]{0,})"))
(def (load-time-constant e) +msie-version-scanner+      (cl-ppcre:create-scanner "MSIE ([0-9]{1,}\.[0-9]{0,})"))
(def (load-time-constant e) +mozilla-version-scanner+   (cl-ppcre:create-scanner "Mozilla/([0-9]{1,}\.[0-9]{0,})"))
(def (load-time-constant e) +drakma-version-scanner+    (cl-ppcre:create-scanner "Drakma/([0-9]{1,}\.[0-9]{0,})"))

(def (namespace :test 'equal) user-agent)

(def class* user-agent ()
  ((http-header :type (or null string))
   (kind :type keyword)
   (version :type number)
   (supported :type boolean :accessor supported?)))

(def print-object user-agent
  (print-user-agent -self- t))

(def function print-user-agent (user-agent stream)
  (format stream "~A : ~A (~A)"
          (kind-of user-agent) (version-of user-agent)
          (if (supported? user-agent)
              "supported"
              "unsupported")))

(def function make-generic-user-agent (http-header)
  (make-instance 'user-agent
                 :http-header http-header
                 :kind :generic
                 :version 0
                 :supported #f))

(def function determine-user-agent (request)
  (bind ((http-header (header-value request +header/user-agent+)))
    (or (find-user-agent http-header :otherwise nil)
        (setf (find-user-agent http-header)
              (parse-user-agent http-header)))))

(def function parse-user-agent (http-header)
  (flet ((check-user-agent (version-scanner user-agent minimum-version)
           (bind (((:values success? version-string) (cl-ppcre:scan-to-strings version-scanner http-header)))
             (when success?
               (bind ((version (or (ignore-errors
                                     (parse-number:parse-number (first-elt version-string)))
                                   0)))
                 (make-instance 'user-agent
                                :kind user-agent
                                :version version
                                :supported (>= version minimum-version)
                                :http-header http-header))))))
    (aprog1 (or (check-user-agent +chrome-version-scanner+ :chrome 3)
                (check-user-agent +opera-version-scanner+ :opera 9.6)
                (check-user-agent +konqueror-version-scanner+ :konqueror 4.2)
                (check-user-agent +safari-version-scanner+ :safari 4)
                (check-user-agent +msie-version-scanner+ :msie 7)
                (check-user-agent +mozilla-version-scanner+ :mozilla 5)
                (check-user-agent +drakma-version-scanner+ :drakma 0)
                (make-generic-user-agent http-header))
      (http.info "Determined user agent is ~A" it))))

(def class* user-agent-breakdown (user-agent)
  ((count :type integer)))

(def function make-user-agent-breakdown (&optional (server *server*))
  (bind ((user-agents (make-hash-table :test #'equal)))
    (iter (for broker :in (brokers-of server))
          (when (typep broker 'application)
            (iter (for (id session) :in-hashtable (session-id->session-of broker))
                  (for user-agent = (user-agent-of session))
                  (when user-agent
                    (incf (gethash (http-header-of user-agent) user-agents 0))))))
    (iter (for (http-header count) :in-hashtable user-agents)
          (collect (change-class (parse-user-agent http-header) 'user-agent-breakdown :count count)))))
