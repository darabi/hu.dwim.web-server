;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(defgeneric read-request (server stream))

(defmethod read-request :around (server stream)
  (with-thread-name " / READ-REQUEST"
    (call-next-method)))

(def (function io) parse-http-version (version-string)
  (declare (type string version-string))
  (flet ((fail-unless (condition)
           (unless condition
             (abort-server-request (format nil "Illegal http version string ~S" version-string)))))
    (declare (inline fail-unless))
    (fail-unless (starts-with-subseq "HTTP/" version-string))
    (bind ((dot-position (position #\. version-string :start 5)))
      (fail-unless dot-position)
      (bind ((major-version (parse-integer version-string :start 5 :end dot-position))
             ((:values minor-version end-position) (parse-integer version-string :start (1+ dot-position))))
        (fail-unless (= (length version-string) end-position))
        (values major-version minor-version)))))

(def (function o) read-http-request (stream)
  (bind ((line (read-http-request-line stream :length-limit +maximum-http-request-header-line-length+))
         (pieces (split-ub8-vector +space+ line))
         ((http-method uri-octets &optional raw-version-string) pieces))
    (http.dribble "In READ-HTTP-REQUEST, first line in ISO-8859-1 is ~S" (iso-8859-1-octets-to-string line))
    ;; uri decoding: octets -> us-ascii -> foo%12%34bar unescape resulting in octets -> utf-8.
    ;; processing anything else here would be ad-hoc...
    (bind ((headers (aprog1
                        (read-http-request-headers stream)
                      (http.dribble "Request headers are ~S" it))))
      (flet ((header-value (name &key mandatory)
               (bind ((result (awhen (assoc name headers :test #'string=)
                                (cdr it))))
                 (when (and mandatory
                            (not result))
                   (abort-server-request (format nil "No ~S header in the request" name)))
                 result)))
        (bind ((version-string (us-ascii-octets-to-string raw-version-string))
               ((:values major-version minor-version) (parse-http-version version-string))
               (raw-uri (us-ascii-octets-to-string uri-octets))
               (raw-uri-length (length raw-uri))
               (raw-content-length (header-value +header/content-length+))
               (keep-alive? (and raw-content-length
                                 (parse-integer raw-content-length :junk-allowed #t)
                                 (>= major-version 1)
                                 (>= minor-version 1)
                                 (not (string= (header-value +header/connection+) "close"))))
               (host (header-value "Host" :mandatory #t))
               (host-length (length host))
               (scheme "http") ; TODO
               (scheme-length (length scheme))
               (uri-string (bind ((position 0)
                                  (result (make-string (+ scheme-length #.(length "://") host-length raw-uri-length)
                                                       :element-type 'base-char)))
                             (replace result scheme)
                             (replace result #.(coerce "://" 'simple-base-string) :start1 (incf position scheme-length))
                             (replace result host :start1 (incf position #.(length "://")))
                             (replace result raw-uri :start1 (incf position host-length))
                             result))
               (uri (%parse-uri uri-string))
               (uri-parameters (query-parameters-of uri)))
          (http.dribble "Request query parameters from the uri: ~S" uri-parameters)
          ;; extend the parameters with the possible stuff in the request body
          ;; making sure duplicate entries are recorded in a list keeping the original order.
          (bind ((parameters (read-http-request-body stream
                                                     raw-content-length
                                                     (header-value "Content-Type")
                                                     uri-parameters)))
            (http.dribble "All the request query parameters: ~S" parameters)
            (make-instance 'request
                           :raw-uri raw-uri
                           :uri uri
                           :keep-alive keep-alive?
                           :network-stream stream
                           :query-parameters parameters
                           :http-method (us-ascii-octets-to-string http-method)
                           :http-version-string version-string
                           :http-major-version major-version
                           :http-minor-version minor-version
                           :headers headers)))))))

(declaim (ftype (function * simple-ub8-vector) read-http-request-line))

(def (function o) read-http-request-line (stream &key (length-limit #.(* 4 1024)))
  "A simple state machine which reads chars from STREAM until it gets a CR-LF sequence. Signals an error upon EOF."
  (declare (type array-index length-limit))
  (bind ((buffer (make-adjustable-vector 64 :element-type '(unsigned-byte 8)))
         (count 0))
    (declare (type array-index count))
    (labels ((read-next-char ()
               (bind ((byte (read-byte stream t 'eof)))
                 (assert (not (eq byte 'eof)))
                 (when (> (incf count) length-limit)
                   (report-dos "LENGTH-LIMIT (~A) reached in READ-HTTP-REQUEST-LINE" length-limit))
                 byte))
             (cr ()
               (let ((next-byte (read-next-char)))
                 (case next-byte
                   (#.+linefeed+
                    (return-from read-http-request-line (coerce buffer 'simple-ub8-vector)))
                   (t ;; add both the cr and this char to the buffer
                    (vector-push-extend #.+carriage-return+ buffer)
                    (vector-push-extend next-byte buffer)
                    (next)))))
             (next ()
               (let ((next-byte (read-next-char)))
                 (case next-byte
                   (#.+carriage-return+
                    (cr))
                   (#.+linefeed+
                    (error "Linefeed received without a Carriage Return"))
                   (t
                    (vector-push-extend next-byte buffer)
                    (next))))))
      (next))))

(def (function o) read-http-request-headers (stream)
  (flet ((split-http-header-line (line)
           (declare (type simple-ub8-vector line))
           (let* ((colon-position (position +colon+ line :test #'=))
                  (name-length colon-position)
                  (value-start (1+ colon-position))
                  (value-end (length line)))
             (declare (type array-index name-length value-start value-end))
             ;; skip any leading space char in the header value
             (iter
               (for start :upfrom value-start)
               (while (< start value-end))
               (for byte = (aref line start))
               (while (or (= +space+ byte)
                          (= +tab+ byte)))
               (declare (type array-index start))
               (incf start)
               (finally (setf value-start (1- start))))
             (cons (subseq line 0 name-length)
                   (subseq line value-start value-end)))))
    (iter
      (for header-line = (read-http-request-line stream))
      (for count :upfrom 0)
      (until (zerop (length header-line)))
      (when (> count +maximum-http-request-header-line-count+)
        (report-dos "More then ~A http header lines" count))
      (for (name . value) = (split-http-header-line header-line))
      (collect (cons (us-ascii-octets-to-string name)
                     (iso-8859-1-octets-to-string value))))))

(def (function o) make-rfc2388-callback (form-data-accumulator file-accumulator)
  (lambda (mime-part)
    (http.dribble "Processing mime part ~S." mime-part)
    (bind ((content-disposition-header (rfc2388-binary:get-header mime-part "Content-Disposition"))
           (content-disposition (rfc2388-binary:header-value content-disposition-header))
           (name (rfc2388-binary:get-header-attribute content-disposition-header "name"))
           (filename (rfc2388-binary:get-header-attribute content-disposition-header "filename")))
      (http.dribble "Got a mime part. Disposition: ~S; Name: ~S; Filename: ~S" content-disposition name filename)
      (http.dribble "Mime Part:---~%~A---" (with-output-to-string (dump) (rfc2388-binary:print-mime-part mime-part dump)))
      (cond
        ((or (string-equal "file" content-disposition)
             (not (null filename)))
         (bind (((:values file tmp-filename) (open-temporary-file :name-prefix "wui-upload-")))
           (setf (rfc2388-binary:content mime-part) file)
           (http.dribble "Sending mime part data to file ~S (~S)" tmp-filename (rfc2388-binary:content mime-part))
           (bind ((counter 0)
                  (buffer (make-array 8196 :element-type '(unsigned-byte 8)))
                  (buffer-length (length buffer))
                  (buffer-index 0))
             (declare (type array-index buffer-length buffer-index counter))
             (values (lambda (byte)
                       (declare (type (unsigned-byte 8) byte))
                       ;;(http.dribble "File byte ~4,'0D: ~D~:[~; (~C)~]" counter byte (<= 32 byte 127) (code-char byte))
                       (setf (aref buffer buffer-index) byte)
                       (incf counter)
                       (incf buffer-index)
                       (when (>= buffer-index buffer-length)
                         (write-sequence buffer file)
                         (setf buffer-index 0)))
                     (lambda ()
                       (http.dribble "Done with file ~S" (rfc2388-binary:content mime-part))
                       (unless (zerop buffer-index)
                         (write-sequence buffer file :end buffer-index))
                       (http.dribble "Closing ~S" (rfc2388-binary:content mime-part))
                       (close file)
                       (http.dribble "Closed, repoening")
                       (setf (rfc2388-binary:content mime-part) tmp-filename)
                       (http.dribble "Opened ~S" (rfc2388-binary:content mime-part))
                       (funcall file-accumulator name mime-part)
                       (values))
                     (lambda ()
                       (close file)
                       (delete-file tmp-filename))))))
        ((string-equal "form-data" content-disposition)
         (http.dribble "Grabbing mime-part data as string.")
         (setf (rfc2388-binary:content mime-part) (make-adjustable-vector 10 :element-type '(unsigned-byte 8)))
         (bind ((counter 0))
           (declare (type array-index counter))
           (values (lambda (byte)
                     (declare (type (unsigned-byte 8) byte))
                     (http.dribble "Form-data byte ~4,'0D: ~D~:[~; (~C)~]." counter byte (<= 32 byte 127) (code-char byte))
                     (incf counter)
                     (vector-push-extend byte (rfc2388-binary:content mime-part)))
                   (lambda ()
                     ;; TODO fixed utf-8?
                     (bind ((content (utf-8-octets-to-string (rfc2388-binary:content mime-part))))
                       (http.dribble "Done with form-data ~S: ~S" name content)
                       (funcall form-data-accumulator name content)
                       (values))))))
        (t
         (error "Don't know how to handle the mime-part ~S (content-disposition: ~S)" mime-part content-disposition-header))))))

(def (function o) read-http-request-body (stream raw-content-length raw-content-type initial-parameters)
  (when (and raw-content-length
             raw-content-type)
    (with-thread-name " / READ-REQUEST-BODY"
      (bind ((content-length (parse-integer raw-content-length :junk-allowed t)))
        (when (and content-length
                   (> content-length 0))
          (when (> content-length *request-content-length-limit*)
            (request-content-length-limit-reached content-length))
          (bind (((:values content-type attributes) (rfc2388-binary:parse-header-value raw-content-type)))
            (switch (content-type :test #'string=)
              ("application/x-www-form-urlencoded"
               ;; TODO dos prevention, lower limit here than *request-content-length-limit*, or separate for files
               (bind ((buffer (make-array content-length :element-type '(unsigned-byte 8))))
                 (read-sequence buffer stream)
                 (bind ((buffer-as-string
                         (aif (cdr (assoc "charset" attributes :test #'string=))
                              ;; TODO check the standard, disable unescape in parse-query-parameters if needed...
                              (eswitch (it :test #'string-equal)
                                ("utf-8"      (utf-8-octets-to-string buffer))
                                ("ascii"      (us-ascii-octets-to-string buffer))
                                ("iso-8859-1" (iso-8859-1-octets-to-string buffer)))
                              (us-ascii-octets-to-string buffer))))
                   (http.dribble "Parsing application/x-www-form-urlencoded body. Attributes: ~S, value: ~S" attributes buffer-as-string)
                   ;; TODO buffer-as-string should never be non-ascii... read up on the standard, do something about that coerce...
                   (setf buffer-as-string (coerce buffer-as-string 'simple-base-string))
                   (return-from read-http-request-body (parse-query-parameters buffer-as-string initial-parameters)))))
              ("multipart/form-data"
               (http.dribble "Parsing multipart/form-data body. Attributes: ~S." attributes)
               (bind ((boundary (cdr (assoc "boundary" attributes :test #'string=))))
                 ;; TODO DOS prevention: add support for rfc2388-binary to limit parsing length if the ContentLength header is fake, pass in *request-content-length-limit*
                 (rfc2388-binary:read-mime stream boundary
                                           (make-rfc2388-callback
                                            (lambda (name value)
                                              (record-query-parameter (cons name value) initial-parameters))
                                            (lambda (name file-mime-part)
                                              (record-query-parameter (cons name file-mime-part) initial-parameters))))
                 (return-from read-http-request-body initial-parameters)))
              (t (abort-server-request "Invalid request content type"))))))))
  (http.debug "Skipped parsing request body, raw Content-Type is [~S], raw Content-Length is [~S]" raw-content-type raw-content-length)
  initial-parameters)
