(in-package :wui.test)

(def macro with-wui-logger-level (log-level &body body)
  `(with-logger-level (wui) ,log-level
    ,@body))

(def special-variable *muffle-compiler-warnings* t
  "Muffle or not the compiler warnings while the tests are running.")

(def macro with-test-compiler-environment (&body body)
  `(handler-bind ((style-warning
                   (lambda (c)
                     (when *muffle-compiler-warnings*
                       (muffle-warning c)))))
    ,@body))

(defsuite* (test :in root-suite) (&key (log-level +warn+))
  (with-wui-logger-level log-level
    (with-test-compiler-environment
      (run-child-tests))))

(def definer test (name args &body body)
  `(def stefil::test ,name ,args
    ;; rebind these, so that we can setf it freely in the tests...
    (bind ((*test-application* *test-application*)
           (*test-server* *test-server*))
      ,@body)))

(def special-variable *test-server* nil
  "The currently running test server.")

(def special-variable *test-application* nil
  "The currently running test application.")

(def special-variable *test-host*      +any-host+
  "The test server host.")

(def special-variable *test-port*      8080
  "The test server port.")

(def symbol-macro +test-server-base-url+
  (print-uri-to-string (make-uri :scheme "http" :host *test-host* :port *test-port* :path "/")))

(def function out (string &rest args)
  (let ((*package* #.(find-package :common-lisp)))
    (apply 'format (html-stream-of *response*) string args)))

(def generic web (path &key request-parameters expected-status parse-result-as)
  (:method ((path string) &key request-parameters (expected-status 200) parse-result-as)
    (multiple-value-bind (body status headers url)
        (drakma:http-request (concatenate-string (when (or (< (length path) 4)
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

(def function uri (&optional (path ""))
  (let ((concatenated-path (apply #'concatenate-string (ensure-list path))))
    (make-uri
     :scheme "http"
     :host (address-to-string *test-host*)
     :port *test-port*
     :path (concatenate 'string
                        (path-prefix-of *test-application*)
                        (if (and (not (zerop (length concatenated-path)))
                                 (char= (elt concatenated-path 0) #\/))
                            (subseq concatenated-path 1)
                            concatenated-path)))))
