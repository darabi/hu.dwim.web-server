;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.web-server)

;;;;;;
;;; request

(def method close-request ((request http-request))
  (map-request-parameters (lambda (name value)
                            (declare (ignore name))
                            (when (typep value 'rfc2388-binary:mime-part)
                              (delete-file (rfc2388-binary:content value))))
                          request)
  (values))

(def function accepts-encoding? (encoding-name)
  (not (null (assoc encoding-name (accept-encodings-of *request*) :test #'string=))))

(def method accept-encodings-of :around ((request http-request))
  (if (slot-boundp request 'accept-encodings)
      (call-next-method)
      (setf (accept-encodings-of request)
            (aprog1
                (parse-accept-header-value (or (header-value request +header/accept-encoding+) ""))
              (http.dribble "Parsed the accept-encoding field for the request: ~A" it)))))

(def method cookies-of :around ((request http-request))
  (if (slot-boundp request 'cookies)
      (call-next-method)
      (setf (cookies-of request)
            ;; if we called SAFE-PARSE-COOKIES here, then cookies that don't match the domain restrinctions were dropped.
            ;; instead we parse all cookies and let the users control what they will accept.
            ;; FIXME because of this delayed parsing of cookies, we cannot turn cookie parsing errors into illegal-request errors.
            (aprog1
                (rfc2109:parse-cookies (header-value request "Cookie"))
              (http.dribble "Parsed the cookies for the request: ~A" it)))))

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

(def (function e) request-parameter-value (request name &optional default)
  (bind ((entry (assoc name (query-parameters-of request) :test #'string=))
         (value (cdr entry)))
    (if entry
        (when (equal value "")
          (setf value nil))
        (setf value default))
    (values value (not (null entry)))))

(def (function e) map-request-parameters (visitor request)
  (bind ((result (list)))
    (dolist ((name . value) (query-parameters-of request))
      (push (funcall visitor name value) result))
    (nreverse result)))


;;;;;;
;;; response

(def (class* e) http-response (http-message response)
  ((headers-are-sent #f :type boolean)
   (external-format +default-external-format+ :type external-format :documentation "May or may not be used by some higher level functionalities")))

(def method close-response ((self http-response))
  (values))

(def constructor http-response
  (setf (header-value -self- +header/status+) +http-ok+)
  (setf (cookies-of -self-) (list)))

(def method encoding-name-of ((self response))
  (aif (external-format-of self)
       (encoding-name-of it)
       (error "No external format for response ~A" self)))

(def (function e) disallow-response-caching (response)
  "Sets the appropiate response headers that will instruct the clients not to cache this response."
  (disallow-response-caching-in-header-alist (headers-of response))
  response)


;;;;;;
;;; primitive-response

(def method send-headers ((response primitive-http-response))
  (http.debug "Sending headers of ~A" response)
  (send-http-headers (headers-of response) (cookies-of response)))

(def method send-response :around ((response primitive-http-response))
  (store-response response)

  (call-next-method)
  ;; KLUDGE reconsider this. do we really need all this fooling around with dynamic external formats?
  #+nil
  (flet ((set-external-format (stream external-format)
           (etypecase stream
             (iolib.sockets:stream-socket (setf (iolib:external-format-of stream) external-format))
             (flexi-streams:flexi-stream (setf (flexi-streams:flexi-stream-external-format stream)
                                               (if (typep external-format 'flexi-streams::external-format)
                                                   external-format
                                                   (flexi-streams:make-external-format external-format))))))
         (get-external-format (stream)
           (etypecase stream
             (iolib.sockets:stream-socket (iolib:external-format-of stream))
             (flexi-streams:flexi-stream (flexi-streams:flexi-stream-external-format stream)))))
    (bind ((client-stream (client-stream-of *request*))
           (new-external-format (external-format-of response))
           (original-external-format (get-external-format client-stream)))
      (http.debug "Sending primitive response ~A, new external-format ~A, original external-format ~A" response new-external-format original-external-format)
      (if new-external-format
          (unwind-protect
               (progn
                 (set-external-format client-stream new-external-format)
                 (call-next-method))
            (set-external-format client-stream original-external-format))
          (call-next-method)))))

(def method send-response ((response primitive-http-response))
  (assert (not (headers-are-sent-p response)) () "The headers of ~A have already been sent, this is a program error." response)
  (setf (headers-are-sent-p response) #t)
  (send-headers response))


;;;;;;
;;; Cookies

(def (macro e) do-cookies ((cookie message) &body forms)
  `(dolist (,cookie (cookies-of ,message))
     ,@forms))

(def function find-cookies (cookie &key accepted-domains otherwise)
  (find-request-cookies *request* cookie :accepted-domains accepted-domains :otherwise otherwise))

(def function find-request-cookies (request cookie &key accepted-domains (otherwise '()))
  (setf accepted-domains (ensure-list accepted-domains))
  (or (bind ((cookie-name (cond ((stringp cookie) cookie)
                                ((rfc2109:cookie-p cookie) (rfc2109:cookie-name cookie))
                                (t (error "FIND-COOKIE only supports string and rfc2109:cookie struct as cookie name specifier"))))
             (result (list)))
        (do-cookies (candidate request)
          (when (and (string= cookie-name (rfc2109:cookie-name candidate))
                     (bind ((candidate-domain (rfc2109:cookie-domain candidate)))
                       ;; normally request cookies should not have a domain parameter, but...
                       (or (null accepted-domains)
                           (null candidate-domain)
                           (some (lambda (accepted-domain)
                                   (ends-with-subseq accepted-domain candidate-domain))
                                 accepted-domains))))
            (push candidate result)))
        (nreverse result))
      (handle-otherwise/value otherwise)))

(def (function e) cookie-value (cookie &key domain otherwise)
  "Return the uri-unescaped cookie value from *REQUEST* or OTHERWISE if not found."
  (request-cookie-value *request* cookie :domain domain :otherwise otherwise))

(def (function e) request-cookie-value (request cookie &key domain (otherwise nil))
  "Return the uri-unescaped cookie value from REQUEST or OTHERWISE if not found."
  (aif (find-request-cookies request cookie :accepted-domains (ensure-list domain))
       ;; we take the first one because cookies are in such an order that the one with the most specific path is at the head of the list
       ;; (this ordering does NOT apply to domains, and ff sends them in random order if the only difference is the domain! baaaaah... see comments at: http://www.nczonline.net/blog/2009/05/05/http-cookies-explained/ because of this the session id cookie of foo.com and dev.foo.com cannot be separated using a shared web server process)
       ;; normally we want the most specific one to be returned, otherwise use the more generic FIND-REQUEST-COOKIES call
       (hu.dwim.uri:percent-encoding/decode (rfc2109:cookie-value (first it)))
       (handle-otherwise/value otherwise)))

(def (function e) add-cookie (cookie &optional (response *response*))
  "Add cookie to the current response."
  (assert (rfc2109:cookie-p cookie))
  (push cookie (cookies-of response)))

#+nil ;; TODO
(def method remote-address :around ((message http-message))
  (declare (optimize speed))
  (let ((physical-remote-address (call-next-method)))
    (if (and physical-remote-address
             (or (inet-address-private-p physical-remote-address)
                 (local-address-p physical-remote-address)))
        ;; check if we are in a proxy setup and extract the real remote address if provided.
        ;; but do so only if the physical remote address is coming from a machine from the local net.
        ;; please note that X-Forwarded-For is not a realiable source for ip addresses!
        ;; that's why we only process them when the physical remote address is on the local net
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


;;;;;;
;;; Response with html stream

(def (class* e) response-with-html-stream (response)
  ((html-stream nil)))

(def method html-stream-of :around ((self response-with-html-stream))
  (bind ((result (call-next-method)))
    (or result
        (setf (html-stream-of self)
              (make-in-memory-output-stream :external-format (external-format-of self))))))

(def method send-response ((response response-with-html-stream))
  (bind ((content nil))
    (awhen (slot-value response 'html-stream)
      (http.dribble "Converting html stream of ~A" response)
      (setf content (babel-streams:get-output-stream-sequence it))
      (assert (null (header-value response +header/content-length+)))
      (http.dribble "Setting Content-Length header of ~A" response)
      (setf (header-value response +header/content-length+) (integer-to-string (length content))))
    (call-next-method)
    (when (and content
               (not (string= "HEAD" (http-method-of *request*))))
      (http.dribble "Sending ~S (~D bytes) as body" content (length content))
      (write-sequence content (client-stream-of response)))))


;;;;;;
;;; Functional response

;; TODO review the naming in this section...
(def (class* e) functional-response (primitive-http-response)
  ((thunk :type (or symbol function))
   (cleanup-thunk nil :type (or symbol function)))
  (:documentation "A primitive-response that sends the headers and then calls its thunk. Keep in mind that it will not do any buffering, so it uses up a worker thread while sending the response through the network."))

(def method send-response ((response functional-response))
  (call-next-method)
  (funcall (thunk-of response)))

(def (class* e) raw-functional-response (functional-response)
  ()
  (:documentation "A response that does nothing else than calling its thunk."))

(def method send-response ((response raw-functional-response))
  ;; override everything, including the sending of the headers...
  (funcall (thunk-of response)))

(def method close-response :after ((self functional-response))
  (awhen (cleanup-thunk-of self)
    (funcall it)))

(def (function e) make-functional-response* (thunk &key headers cookies (raw #f) cleanup-thunk)
  (if raw
      (make-instance 'raw-functional-response
                     :thunk thunk
                     :cleanup-thunk cleanup-thunk
                     :headers headers
                     :cookies cookies)
      (make-instance 'functional-response
                     :thunk thunk
                     :cleanup-thunk cleanup-thunk
                     :headers headers
                     :cookies cookies)))

(def function expand-make-functional-response (raw headers-as-plist cookie-list body)
  (with-unique-names (response)
    `(bind ((,response (make-functional-response* (named-lambda functional-response/body () ,@body) :raw ,raw)))
       (store-response ,response)
       ;; this way *response* is bound while evaluating the following
       (setf (headers-of ,response) (list ,@(iter (for (name value) :on headers-as-plist :by #'cddr)
                                                  (collect `(cons ,name ,value)))))
       (setf (cookies-of ,response) (list ,@cookie-list))
       ,response)))

(def (macro e) make-functional-response ((&optional headers-as-plist cookie-list) &body body)
  (expand-make-functional-response #f headers-as-plist cookie-list body))

(def (macro e) make-raw-functional-response (() &body body)
  (expand-make-functional-response #t nil nil body))

(def (macro e) make-functional-html-response ((&optional headers-as-plist cookie-list) &body body)
  `(make-functional-response ((+header/content-type+ (content-type-for +html-mime-type+ +default-encoding+)
                               ,@headers-as-plist)
                              (,@cookie-list))
     (emit-into-xml-stream (client-stream-of *request*)
       ,@body)))

(def (macro e) make-buffered-functional-html-response ((&optional headers-as-plist cookie-list) &body body)
  (with-unique-names (response)
    `(bind ((,response (make-byte-vector-response* nil)))
       (store-response ,response)
       ;; set a default content type header. do it early, so that it's already set when the body is rendered
       (setf (header-value ,response +header/content-type+) ,(content-type-for +html-mime-type+ +default-encoding+))
       (bind ((buffer (emit-into-xml-stream-buffer (:external-format +default-external-format+)
                        ,@body)))
         (appendf (headers-of ,response) (list ,@(iter (for (name value) :on headers-as-plist :by #'cddr)
                                                       (collect `(cons ,name ,value)))))
         (setf (cookies-of ,response) (list ,@cookie-list))
         (setf (body-of ,response) buffer)
         ,response))))


;;;;;;
;;; Byte vector response

(def (class* e) byte-vector-response (primitive-http-response)
  ((last-modified-at nil)
   (body :type (or list vector))))

(def (function e) make-byte-vector-response* (bytes &key headers cookies last-modified-at seconds-until-expires content-type)
  (bind ((result (make-instance 'byte-vector-response
                                :body bytes
                                :headers headers
                                :cookies cookies
                                :last-modified-at last-modified-at
                                :external-format nil)))
    (when seconds-until-expires
      (setf (header-value result +header/expires+) (local-time:to-rfc1123-timestring
                                                    (local-time:adjust-timestamp (local-time:now) (offset :sec seconds-until-expires)))))
    (when content-type
      (setf (header-value result +header/content-type+) content-type))
    result))

(def (macro e) make-byte-vector-response ((&optional headers-as-plist cookie-list) &body body)
  (with-unique-names (response)
    `(bind ((,response (make-byte-vector-response* nil)))
       (store-response ,response)
       ;; this way *response* is bound while evaluating the following
       (setf (headers-of ,response) (list ,@(iter (for (name value) :on headers-as-plist :by #'cddr)
                                                  (collect `(cons ,name ,value)))))
       (setf (cookies-of ,response) (list ,@cookie-list))
       (setf (body-of ,response)
             (babel-streams:with-output-to-sequence (*standard-output* :external-format :utf-8)
               ,@body))
       ,response)))

(def method send-response ((response byte-vector-response))
  (serve-sequence (body-of response)
                  :compress-content-with (unless (header-value response +header/content-encoding+)
                                           (default-response-compression))
                  :headers (headers-of response)
                  :cookies (cookies-of response)
                  :last-modified-at (last-modified-at-of response)))


;;;;;;
;;; Not found response

(def class* not-found-response (primitive-http-response)
  ())

(def (function e) make-not-found-response ()
  (aprog1
      (make-instance 'not-found-response)
    (setf (header-value it +header/status+) +http-not-found+)))

(def method send-response ((self not-found-response))
  (emit-http-response/simple-html-document (:status +http-not-found+
                                            :title "Page not found"
                                            :headers (headers-of self)
                                            :cookies (cookies-of self)
                                            :cacheable #f)
    <h1 "Page not found">
    <p <span (:style "background-color: #fdd;") ,(hu.dwim.uri:print-uri-to-string (uri-of *request*) :escape #f)>
       " was not found on this server">))


;;;;;;
;;; Request echo response

(def class* request-echo-response (primitive-http-response)
  ()
  (:documentation "Render back the request details as a simple html page, mostly for debugging purposes."))

(def (function e) make-request-echo-response ()
  (make-instance 'request-echo-response))

(def method send-response ((self request-echo-response))
  (emit-http-response ((+header/content-type+ +html-mime-type+))
    (render-request *request*)))

(def (function e) render-request (request)
  (emit-html-document (:title "URL echo server")
    <p "Raw request uri: \"" ,(raw-uri-of request) "\"">
    <p "Parsed request uri: \"" ,(hu.dwim.uri:print-uri-to-string (uri-of request)) "\"">
    <h3 "Headers">
    <hr>
    <table
      ,@(iter (for (name . value) :in (headers-of request))
              <tr
                <td ,name>
                <td ,value>>)>
    <h3 "Cookies">
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

;;;;;;
;;; Redirect response

(def class* redirect-response (primitive-http-response)
  ((target-uri :type string)))

(def print-object (redirect-response :identity #f :type #t)
  (princ (target-uri-of -self-)))

(def constructor redirect-response
  (setf (header-value -self- +header/status+) +http-moved-temporarily+)
  (setf (header-value -self- +header/content-type+) +utf-8-html-content-type+)
  (setf (external-format-of -self-) (ensure-external-format :utf-8)))

(def (function e) make-redirect-response (target-uri)
  (setf target-uri (etypecase target-uri
                     (hu.dwim.uri:uri (hu.dwim.uri:print-uri-to-string target-uri))
                     (string target-uri)))
  (bind ((response (make-instance 'redirect-response :target-uri target-uri)))
    (setf (header-value response +header/location+) target-uri)
    response))

(def method send-response ((self redirect-response))
  ;; can't use emit-http-response, because +header/content-location+ is not constant
  ;; don't (call-next-method) because it would sideffect response with (setf (headers-are-sent-p response) #t)
  (send-headers self)
  (emit-into-xml-stream (client-stream-of *request*)
    (emit-html-document (:title "Redirect")
      <p "Page has moved " <a (:href ,(target-uri-of self)) "here">>)))


;;;;;;
;;; Do nothing response

(def class* do-nothing-response (primitive-http-response)
  ())

(def (function e) make-do-nothing-response ()
  (make-instance 'do-nothing-response))

(def method send-response ((self do-nothing-response))
  ;; nop
  )
