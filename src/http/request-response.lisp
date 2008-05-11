;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(defgeneric header-value (message header-name))
(defgeneric (setf header-value) (value message header-name))
(defgeneric send-response (response))
(defgeneric send-headers (response))
(defgeneric close-request (request))
(defgeneric network-stream-of (message))
(defgeneric remote-address-of (message))

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
       (if (functionp otherwise)
           (funcall otherwise)
           otherwise)))

(defun add-cookie (cookie &optional (response *response*))
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
  ((socket)
   (http-method)
   (http-version)
   (raw-uri)
   (uri)
   (query-parameters :documentation "Holds the accumulated query parameters from the uri and/or the request body")))

(defmethod network-stream-of ((request request))
  (socket-of request))

(defmethod remote-address-of ((request request))
  (net.sockets:remote-host (socket-of request)))

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

(def (function e) request-parameter-value (request name)
  (bind ((entry (assoc name (query-parameters-of request) :test #'string=)))
    (values (cdr entry) (not (null entry)))))

(def (function e) map-request-parameters (visitor request)
  (dolist* ((name . value) (query-parameters-of request))
    (funcall visitor name value)))

(defmethod close-request ((request request))
  request)


;;;;;;;;;;;
;;; Response

(def (class* e) response (http-message)
  ((headers-are-sent nil :type boolean)
   (external-format +external-format+ :documentation "May or may not be used by some higher level functionalities")))

(def constructor response
  (setf (header-value self +header/status+) +http-ok+)
  (setf (cookies-of self) (list)))

(defmethod encoding-name-of ((self response))
  (encoding-name-of (external-format-of self)))

(def (function io) write-crlf (stream)
  (write-byte +carriage-return+ stream)
  (write-byte +linefeed+ stream))

(def (function o) send-http-headers (headers &optional cookies)
  (flet ((write-header-line (name value stream)
           (write-sequence (string-to-us-ascii-octets name) stream)
           (write-sequence #.(string-to-us-ascii-octets ": ") stream)
           (write-sequence (string-to-iso-8859-1-octets value) stream)
           (write-crlf stream)
           (values)))
    (bind ((stream (network-stream-of *request*))
           (status (or (cdr (assoc +header/status+ headers :test #'string=))
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
  (send-http-headers (headers-of response) (cookies-of response)))

(defmethod send-response :around ((response response))
  (bind ((*response* response)
         (network-stream (network-stream-of *request*))
         (original-external-format (io.streams:external-format-of network-stream)))
    (setf (io.streams:external-format-of network-stream) (external-format-of response))
    (call-next-method)
    (setf (io.streams:external-format-of network-stream) original-external-format)))

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

(def (macro e) make-functional-response ((&optional headers-as-plist cookie-list) &body body)
  (with-unique-names (response)
    `(bind ((,response (make-functional-response* (lambda () ,@body)))
            (*response* ,response))
       ;; this way *response* is bound while evaluating the following
       (setf (headers-of ,response) (list ,@(iter (for (name value) :on headers-as-plist :by #'cddr)
                                                  (collect `(cons ,name ,value)))))
       (setf (cookies-of ,response) (list ,@cookie-list))
       ,response)))

(def (macro e) make-functional-html-response ((&optional headers-as-plist cookie-list) &body body)
  `(make-functional-response ((+header/content-type+ +html-content-type+
                               ,@headers-as-plist)
                              (,@cookie-list))
     (emit-into-html-stream (network-stream-of *request*)
       ,@body)))

(def (function e) make-functional-response* (thunk &key headers cookies)
  (make-instance 'functional-response
                 :thunk thunk
                 :headers headers
                 :cookies cookies))

(defmethod send-response ((response functional-response))
  (call-next-method)
  (funcall (thunk-of response)))


;;;;;;;;;;;;;;;;;;;;;;;
;;; No handler response

(def class* no-handler-response (response)
  ())

(defmethod send-response ((self no-handler-response))
  (emit-http-response ((+header/status+       +http-not-found+
                        +header/content-type+ +html-content-type+))
    (with-html-document-body (:title "Page not found")
      <h1 "Page not found">
      <p ,(print-uri-to-string (uri-of *request*)) " was not found on this server">)))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; request echo response

(def class* request-echo-response (response)
  ()
  (:documentation "Render back the request details as a simple html page, mostly for debugging purposes."))

(defmethod send-response ((self request-echo-response))
  (emit-http-response ((+header/content-type+ +html-content-type+))
    (render-request *request*)))

(def (function e) render-request (request)
  (with-html-document-body (:title "URL echo server" :content-type +html-content-type+)
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
  (setf (header-value self +header/status+) +http-moved-temporarily+)
  (setf (header-value self +header/content-type+) +utf-8-html-content-type+)
  (setf (external-format-of self) (ensure-external-format :utf-8)))

(def (function e) make-redirect-response (target-uri)
  (setf target-uri (etypecase target-uri
                     (uri (print-uri-to-string target-uri))
                     (string target-uri)))
  (bind ((response (make-instance 'redirect-response :target-uri target-uri)))
    (setf (header-value response +header/location+) target-uri)
    response))

(defmethod send-response ((self redirect-response))
  ;; can't use emit-http-response, because +header/content-location+ is not constant
  (emit-http-response* ((headers-of self)
                        (cookies-of self))
    (with-html-document-body (:title "Redirect")
      <p "Page has moved " <a (:href ,(target-uri-of self)) "here">>)))
