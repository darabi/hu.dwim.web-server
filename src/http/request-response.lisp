;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (generic e) header-value (message header-name))
(def (generic e) (setf header-value) (value message header-name))
(def (generic e) remote-host-of (message))
(defgeneric send-response (response))
(defgeneric send-headers (response))
(defgeneric close-request (request))

(def class* http-message ()
  ((headers nil)
   (cookies)))

(defmethod header-value ((message http-message) header-name)
  (cdr (assoc header-name (headers-of message) :test #'string=)))

(defmethod (setf header-value) (value (message http-message) header-name)
  (aif (assoc header-name (headers-of message) :test #'string=)
       (setf (cdr it) value)
       (push (cons header-name value) (headers-of message)))
  value)

;;;;;;;;;;;
;;; Cookies

(def (macro e) do-cookies ((cookie message) &body forms)
  `(dolist (,cookie (cookies-of ,message))
     ,@forms))

(defun find-cookies (cookie &key accepted-domains otherwise)
  (find-request-cookies *request* cookie :accepted-domains accepted-domains :otherwise otherwise))

(defun find-request-cookies (request cookie &key accepted-domains otherwise)
  (setf accepted-domains (ensure-list accepted-domains))
  (or (bind ((cookie-name (cond ((stringp cookie) cookie)
                                ((rfc2109:cookie-p cookie) (rfc2109:cookie-name cookie))
                                (t (error "FIND-COOKIE only supports string and rfc2109:cookie struct as cookie name specifier"))))
             (result (list)))
        (do-cookies (candidate request)
          (when (and (string= cookie-name (rfc2109:cookie-name candidate))
                     (or (null accepted-domains)
                         (some (lambda (domain)
                                 (ends-with-subseq domain (rfc2109:cookie-domain candidate)))
                               accepted-domains)))
            (push candidate result)))
        (nreverse result))
      (handle-otherwise otherwise)))

(def (function e) cookie-value (cookie &key domain otherwise)
  "Return the uri-unescaped cookie value from *REQUEST* or OTHERWISE if not found."
  (request-cookie-value *request* cookie :domain domain :otherwise otherwise))

(def (function e) request-cookie-value (request cookie &key domain otherwise)
  "Return the uri-unescaped cookie value from REQUEST or OTHERWISE if not found."
  (aif (find-request-cookies request cookie :accepted-domains (ensure-list domain))
       (unescape-as-uri (rfc2109:cookie-value (first it)))
       (handle-otherwise otherwise)))

(def (function e) add-cookie (cookie &optional (response *response*))
  "Add cookie to the current response."
  (assert (rfc2109:cookie-p cookie))
  (push cookie (cookies-of response)))

#+nil ;; TODO
(defmethod remote-address :around ((message http-message))
  (declare (optimize speed))
  (let ((physical-remote-address (call-next-method)))
    (if (and physical-remote-address
             (or (inet-address-private-p physical-remote-address)
                 (local-address-p physical-remote-address)))
        ;; check if we are in a proxy setup and extract the real remote address if provided.
        ;; but do so only if the physical remote address is coming from a machine from the local net.
        ;; please note that this is not a realiable source for ip addresses!
        (let ((ip-as-string (header-value message "X-Forwarded-For")))
          (when ip-as-string
            (let* ((real-remote-address (first (cl-ppcre:split "," ip-as-string :sharedp t)))
                   (pieces (cl-ppcre:split "\\." real-remote-address :sharedp t)))
              (declare (type list pieces))
              (if (= (length pieces) 4)
                  (iter (with result = (make-array 4 :element-type '(unsigned-byte 8)))
                        (for idx :from 0 :below 4)
                        (for ip-address-part = (parse-integer (pop pieces)))
                        (assert (<= 0 ip-address-part 255))
                        (setf (aref result idx) ip-address-part)
                        (finally (return result)))
                  (progn
                    (http.info "Returning NIL instead of an invalid ip address: ~S" ip-as-string)
                    nil)))))
        physical-remote-address)))


;;;;;;;;;;;
;;; Request

(def (class* e) request (http-message)
  ((network-stream)
   (http-method)
   (http-version)
   (raw-uri)
   (uri)
   (query-parameters :documentation "Holds all the query parameters from the uri and/or the request body")))

(defmethod remote-host-of ((request request))
  (iolib:remote-host (network-stream-of request)))

(defmethod cookies-of :around ((request request))
  (if (slot-boundp request 'cookies)
      (call-next-method)
      (setf (cookies-of request)
            ;; if we called SAFE-PARSE-COOKIES here, then cookies that don't match the domain restrinctions were dropped.
            ;; instead we parse all cookies and let the users control what they will accept.
            (rfc2109:parse-cookies (header-value request "Cookie")))))

(def (function e) parameter-value (name &optional default)
  (bind (((:values value found?) (request-parameter-value *request* name)))
    (values (if found? value default) found?)))

(def (function e) map-parameters (visitor)
  (map-request-parameters visitor *request*))

(def (macro e) do-parameters ((name value) &body body)
  `(map-request-parameters
    (lambda (,name ,value)
      ,@body)
    *request*))

(def (function e) request-parameter-value (request name)
  (bind ((entry (assoc name (query-parameters-of request) :test #'string=)))
    (values (cdr entry) (not (null entry)))))

(def (function e) map-request-parameters (visitor request)
  (bind ((result (list)))
    (dolist* ((name . value) (query-parameters-of request))
      (push (funcall visitor name value) result))
    (nreverse result)))

(defmethod close-request ((request request))
  (map-request-parameters (lambda (name value)
                            (declare (ignore name))
                            (when (typep value 'rfc2388-binary:mime-part)
                              (delete-file (rfc2388-binary:content value))))
                          request)
  request)


;;;;;;;;;;;
;;; Response

(def (class* e) response (http-message)
  ((headers-are-sent nil :type boolean)
   (external-format +external-format+ :documentation "May or may not be used by some higher level functionalities")))

(def constructor response
  (setf (header-value -self- +header/status+) +http-ok+)
  (setf (cookies-of -self-) (list)))

(defmethod encoding-name-of ((self response))
  (encoding-name-of (external-format-of self)))

(def (function io) write-crlf (stream)
  (write-byte +carriage-return+ stream)
  (write-byte +linefeed+ stream))

(def (function e) disallow-response-caching (response)
  "Sets the appropiate response headers that will instruct the clients not to cache this response."
  (setf (header-value response "Expires") #.(net.telent.date:universal-time-to-http-date +epoch-start+)
        (header-value response "Cache-Control") "no-store"))

(def (function o) send-http-headers (headers cookies &key (stream (network-stream-of *request*)))
  (flet ((write-header-line (name value stream)
           (write-sequence (string-to-us-ascii-octets name) stream)
           (write-sequence #.(string-to-us-ascii-octets ": ") stream)
           (write-sequence (string-to-iso-8859-1-octets value) stream)
           (write-crlf stream)
           (values)))
    (bind ((status (or (cdr (assoc +header/status+ headers :test #'string=))
                       +http-ok+)))
      (http.dribble "Sending headers (Status: ~S)" status)
      (write-sequence #.(string-to-us-ascii-octets "HTTP/1.1 ") stream)
      (write-sequence (string-to-us-ascii-octets status) stream)
      (write-byte +space+ stream)
      (write-crlf stream)
      (dolist* ((name . value) headers)
        (when value
          (http.dribble "Sending header ~S: ~S" name value)
          (write-header-line name value stream)))
      (dolist (cookie cookies)
        (http.dribble "Writing cookie header line for ~S" cookie)
        (write-header-line "Set-Cookie"
                           (if (rfc2109:cookie-p cookie)
                               (rfc2109:cookie-string-from-cookie-struct cookie)
                               cookie)
                           stream))
      (write-crlf stream)
      status)))

(defmethod send-headers ((response response))
  (http.debug "Sending headers of ~A" response)
  (send-http-headers (headers-of response) (cookies-of response)))

(defmethod send-response :around ((response response))
  (bind ((*response* response)
         (network-stream (network-stream-of *request*))
         (original-external-format (io.streams:external-format-of network-stream)))
    (http.debug "Sending response ~A" response)
    (unwind-protect
         (progn
           (setf (io.streams:external-format-of network-stream) (external-format-of response))
           (call-next-method))
      (setf (io.streams:external-format-of network-stream) original-external-format))))

(defmethod send-response ((response response))
  (assert (not (headers-are-sent-p response)) () "The headers of ~A have already been sent, this is a program error." response)
  (setf (headers-are-sent-p response) #t)
  (send-headers response))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; response-with-html-stream

(def (class* e) response-with-html-stream (response)
  ((html-stream nil)))

(def method html-stream-of :around ((self response-with-html-stream))
  (bind ((result (call-next-method)))
    (or result
        (setf (html-stream-of self)
              (make-in-memory-output-stream :external-format (external-format-of self))))))

(defmethod send-response ((response response-with-html-stream))
  (bind ((content nil))
    (awhen (slot-value response 'html-stream)
      (http.dribble "Converting html stream of ~A" response)
      (setf content (babel-streams:get-output-stream-sequence it))
      (assert (null (header-value response "Content-Length")))
      (http.dribble "Setting Content-Length header of ~A" response)
      (setf (header-value response "Content-Length") (princ-to-string (length content))))
    (call-next-method)
    (when (and content
               (not (string= "HEAD" (http-method-of *request*))))
      (http.dribble "Sending ~S (~D bytes) as body" content (length content))
      (write-sequence content (network-stream-of response)))))


;;;;;;;;;;;;;;;;;;;;;;;
;;; functional-response

(def (class* e) functional-response (response)
  ((thunk :type (or symbol function))))

(defmethod send-response ((response functional-response))
  (call-next-method)
  (funcall (thunk-of response)))

(def (function e) make-functional-response* (thunk &key headers cookies)
  (make-instance 'functional-response
                 :thunk thunk
                 :headers headers
                 :cookies cookies))

(def (macro e) make-functional-response ((&optional headers-as-plist cookie-list) &body body)
  (with-unique-names (response)
    `(bind ((,response (make-functional-response* (lambda () ,@body)))
            (*response* ,response))
       ;; this way *response* is bound while evaluating the following
       (setf (headers-of ,response) (list ,@(iter (for (name value) :on headers-as-plist :by #'cddr)
                                                  (collect `(cons ,name ,value)))))
       (setf (cookies-of ,response) (list ,@cookie-list))
       ,response)))

;; TODO delme?
(def (macro e) make-functional-html-response ((&optional headers-as-plist cookie-list) &body body)
  `(make-functional-response ((+header/content-type+ +html-content-type+
                               ,@headers-as-plist)
                              (,@cookie-list))
     (emit-into-html-stream (network-stream-of *request*)
       ,@body)))

(def (macro e) make-buffered-functional-html-response ((&optional headers-as-plist cookie-list) &body body)
  (with-unique-names (response)
    `(bind ((,response (make-byte-vector-response* nil))
            (*response* ,response))
       ;; set a default content type header. do it early, so that it's already set when the body is rendered
       (setf (header-value ,response +header/content-type+) (content-type-for +html-mime-type+))
       (bind ((buffer (emit-into-html-stream-buffer
                        ,@body)))
         (appendf (headers-of ,response) (list ,@(iter (for (name value) :on headers-as-plist :by #'cddr)
                                                       (collect `(cons ,name ,value)))))
         (setf (cookies-of ,response) (list ,@cookie-list))
         (setf (body-of ,response) buffer)
         ,response))))


;;;;;;;;;;;;;;;;;;;;;;;
;;; byte-vector-response

(def (class* e) byte-vector-response (response)
  ((body :type (or list vector))))

(def (function e) make-byte-vector-response* (bytes &key headers cookies)
  (make-instance 'byte-vector-response
                 :body bytes
                 :headers headers
                 :cookies cookies))

#+nil ; TODO is it needed? broken this way. delme?
(def (macro e) make-byte-vector-response (body &optional headers-as-plist cookie-list)
  (with-unique-names (response)
    `(bind ((,response (make-byte-vector-response* nil))
            (*response* ,response))
       ;; this way *response* is bound while evaluating the following
       (setf (headers-of ,response) (list ,@(iter (for (name value) :on headers-as-plist :by #'cddr)
                                                  (collect `(cons ,name ,value)))))
       (setf (cookies-of ,response) (list ,@cookie-list))
       ,response)))

(defmethod send-response ((response byte-vector-response))
  (bind ((body (ensure-list (body-of response)))
         (network-stream (network-stream-of *request*))
         (length 0))
    (dolist (piece body)
      (incf length (length piece)))
    (setf (header-value response +header/content-length+) (princ-to-string length))
    (call-next-method)
    (dolist (piece body)
      (write-sequence piece network-stream))))


;;;;;;;;;;;;;;;;;;;;;;;
;;; No handler response

(def class* no-handler-response (response)
  ())

(def (function e) make-no-handler-response ()
  (aprog1
      (make-instance 'no-handler-response)
    (setf (header-value it +header/status+) +http-not-found+)))

(defmethod send-response ((self no-handler-response))
  (emit-simple-html-document-response (:title "Page not found"
                                       :headers (headers-of self)
                                       :cookies (cookies-of self))
    <h1 "Page not found">
    <p ,(print-uri-to-string (uri-of *request*)) " was not found on this server">))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; request echo response

(def class* request-echo-response (response)
  ()
  (:documentation "Render back the request details as a simple html page, mostly for debugging purposes."))

(def (function e) make-request-echo-response ()
  (make-instance 'request-echo-response))

(defmethod send-response ((self request-echo-response))
  (emit-http-response ((+header/content-type+ +html-content-type+))
    (render-request *request*)))

(def (function e) render-request (request)
  (with-html-document (:title "URL echo server")
    <p ,(print-uri-to-string (uri-of request))>
    <hr>
    <table
      ,@(iter (for (name . value) :in (headers-of request))
              <tr
                <td ,name>
                <td ,value>>)>
    <hr>
    <table
      <thead
        <tr ,@(iter (for title :in '("Domain" "Path" "Name" "Value" "Max-age" "Secure" "Comment"))
                    (collect <td ,title>))>>
      ,@(iter (for cookie :in (cookies-of request))
              <tr
                ,@(iter (for reader :in '(rfc2109:cookie-domain
                                          rfc2109:cookie-path
                                          rfc2109:cookie-name
                                          rfc2109:cookie-value
                                          rfc2109:cookie-max-age
                                          rfc2109:cookie-secure
                                          rfc2109:cookie-comment))
                        (collect <td ,(or (funcall reader cookie) "")>))>)>))

;;;;;;;;;;;;;;;;;;;;;
;;; redirect response

(def class* redirect-response (response)
  ((target-uri :type string)))

(def constructor redirect-response
  (setf (header-value -self- +header/status+) +http-moved-temporarily+)
  (setf (header-value -self- +header/content-type+) +html-content-type+)
  (setf (external-format-of -self-) (ensure-external-format :utf-8)))

(def (function e) make-redirect-response (target-uri)
  (setf target-uri (etypecase target-uri
                     (uri (print-uri-to-string target-uri))
                     (string target-uri)))
  (bind ((response (make-instance 'redirect-response :target-uri target-uri)))
    (setf (header-value response +header/location+) target-uri)
    response))

(def method send-response ((self redirect-response))
  ;; can't use emit-http-response, because +header/content-location+ is not constant
  (call-next-method)
  (emit-into-html-stream (network-stream-of *request*)
    (with-html-document (:title "Redirect")
      <p "Page has moved " <a (:href ,(target-uri-of self)) "here">>)))


;;;;;;;;;;;;;;;;;;;;;;;
;;; do nothing response

(def class* do-nothing-response (response)
  ())

(def (function e) make-do-nothing-response ()
  (make-instance 'do-nothing-response))

(def method send-response ((self do-nothing-response))
  ;; nop
  )
