(in-package :wui-test)

(in-root-suite)

(defmacro with-wui-logger-level (log-level &body body)
  `(with-logger-level (wui) ,log-level
    ,@body))

(defvar *muffle-compiler-warnings* t
  "Muffle or not the compiler warnings while the tests are running.")

(defmacro with-test-compiler-environment (&body body)
  `(handler-bind ((style-warning
                   (lambda (c)
                     (when *muffle-compiler-warnings*
                       (muffle-warning c)))))
    ,@body))

(defsuite* test (&key (log-level +warn+))
  (with-wui-logger-level log-level
    (with-test-compiler-environment
      (run-child-tests))))

(defmacro deftest (name args &body body)
  `(stefil:deftest ,name ,args
    ;; bind these, so that we can setf it freely in the tests...
    (bind ((*test-application* *test-application*)
           (*test-server* *test-server*))
      ,@body)))

(defvar *test-server* nil
  "The currently running test server.")

(defvar *test-application* nil
  "The currently running test application.")

(defparameter *test-host*      +any-host+)
(defparameter *test-port*      8080)

(define-symbol-macro +test-server-base-url+
  (concatenate 'string "http://" (address-to-string *test-host*) ":" (princ-to-string *test-port*) "/"))

(defun out (string &rest args)
  (let ((*package* #.(find-package :common-lisp)))
    (apply 'format (html-stream-of *response*) string args)))

(defgeneric web (path &key request-parameters expected-status parse-result-as)
  (:method ((path string) &key request-parameters (expected-status 200) parse-result-as)
    (multiple-value-bind (body status headers url)
        (drakma:http-request (strcat (when (or (< (length path) 4)
                                               (not (string= (subseq path 0 4) "http")))
                                       +test-server-base-url+)
                                     (if (and (> (length path) 0)
                                              (char= (elt path 0) #\/))
                                         (subseq path 1)
                                         path))
                             :parameters request-parameters)
      (declare (ignore url))
      (when expected-status
        (is (= status expected-status)))
      #+nil
      (when parse-result-as
        (setf body (ecase parse-result-as
                     (:sexp (read-from-string body))
                     ((:xmls :dom)
                      (flexi-streams:with-input-from-sequence
                          (stream (flexi-streams:string-to-octets body :external-format :utf-8))
                        (cxml:parse-stream stream (ecase parse-result-as
                                                    (:dom (cxml-dom:make-dom-builder))
                                                    (:xmls (cxml-xmls:make-xmls-builder)))))))))
      (values body status headers)))

  (:method ((path wui::uri) &rest args)
    (apply 'web (print-uri-to-string path) args)))

(defun uri (&optional (path "") &rest query-params)
  (let ((concatenated-path (apply #'strcat (ensure-list path))))
    (make-uri
     :scheme "http"
     :host (address-to-string *test-host*)
     :port *test-port*
     :path (concatenate 'string
                        (path-prefix-of *test-application*)
                        (if (and (not (zerop (length concatenated-path)))
                                 (char= (elt concatenated-path 0) #\/))
                            (subseq concatenated-path 1)
                            concatenated-path))
     :query-parameters
     (iter (for (name value) :on query-params :by #'cddr)
           (collect (cons name value))))))




