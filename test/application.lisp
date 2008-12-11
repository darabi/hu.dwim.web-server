(in-package :wui-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test application for basic app features

(def special-variable *test-application* (make-application :path-prefix "/test/"))

(def entry-point (*test-application* :path "params") ((number "0" number?) ((the-answer "theanswer") "not supplied" the-answer?))
  (make-functional-response ()
    (emit-simple-html-document-http-response (:title "foo")
      <p "Parameters:"
        <a (:href ,(concatenate-string (path-prefix-of *test-application*)
                                       (if (or number? the-answer?)
                                           "params"
                                           "params?theanswer=yes&number=42")))
          "try this">>
      <table
        <thead <tr <td "Parameter name"> <td "Parameter value">>>
        ,@(do-parameters (name value)
            <tr
              <td ,name>
              <td ,(etypecase value
                     (string value)
                     (list (princ-to-string value))
                     (rfc2388-binary:mime-part
                      (bind ((mime-part value))
                        <p "Mime part headers:">
                        <table
                        <thead <tr <td "Header"> <td "Value">>>
                        ,@(iter (for header :in (rfc2388-binary:headers mime-part))
                                (collect <tr
                                           <td ,(or (rfc2388-binary:header-name header) "")
                                           <td ,(or (rfc2388-binary:header-value header) "")>>>))>
                        <p "Mime part content: " ,(princ-to-string (rfc2388-binary:content mime-part))>)))>>)>
      <hr>
      <form (:method "post")
        <input (:name "input1"             :value ,(or (parameter-value "input1") "1"))>
        <input (:name "input2-with-áccent" :value ,(or (parameter-value "input2-with-áccent") "Ááő\"$&+ ?űúéö"))>
        <input (:type "submit")>>
      <form (:method "post" :enctype "multipart/form-data")
        <input (:name "file-input1" :type "file")>
        <input (:name "file-input2" :value ,(or (parameter-value "file-input2") "file2"))>
        <input (:type "submit")>>
      <hr>
      <p "The parsed request: ">
      (render-request *request*))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; echo application to echo back the request

(def special-variable *echo-application* (make-application :path-prefix "/echo/"))

(def entry-point (*echo-application* :path-prefix "") ()
  (make-request-echo-response))

;;;;;;;;;;;;;;;;;;;;;;;
;;; session application

(def special-variable *session-application* (make-application :path-prefix "/session/"))

(def entry-point (*session-application* :path "") ()
  (if *session*
      (progn
        (assert (and (boundp '*frame*)
                     *frame*))
        (make-root-component-rendering-response *frame*))
      (bind ((application *application*)) ; need to capture it in the closure
        (make-raw-functional-response ()
          (emit-simple-html-document-http-response ()
            <p "There's no session... "
               <a (:href ,(concatenate-string (path-prefix-of application) "new/"))
                  "create new session">>)))))

(def entry-point (*session-application* :path "new/" :with-session-logic #f) ()
  (bind ((new-session (make-new-session *application*))
         (old-session nil))
    (with-session-logic ()
      ;; this voodoo is not necessary here for building a simple redirect response,
      ;; but it's here for demonstrational purposes.
      (setf old-session *session*)
      (setf *session* new-session)
      (values))
    ;; we may only lock the app again after our session's lock
    ;; has been released to avoid deadlocks by strictly following the
    ;; app -> session locking order...
    (with-lock-held-on-application (*application*)
      (when old-session
        (delete-session *application* old-session))
      (register-session *application* new-session))
    (with-lock-held-on-session (new-session)
      (bind ((new-frame (make-new-frame *application* new-session)))
        (setf *frame* new-frame)
        (register-frame *application* new-session new-frame)
        (make-redirect-response-for-current-application)))))

(def entry-point (*session-application* :path "delete/" :with-session-logic #f) ()
  (bind ((old-session nil))
    (with-session-logic ()
      (setf old-session *session*)
      (values))
    (when old-session
      (with-lock-held-on-application (*application*)
        (delete-session *application* old-session)))
    (make-redirect-response (path-prefix-of *application*))))

(def file-serving-entry-point *session-application* "/session/static/" (project-relative-pathname "wwwroot/"))

(def function start-test-server-with-test-applications (&key (maximum-worker-count 16) (log-level +debug+))
  (setf (log-level 'wui) log-level)
  (start-test-server-with-brokers (list (make-redirect-broker "/session" "/session/")
                                        *session-application*
                                        (make-redirect-broker "/echo" "/echo/")
                                        *echo-application*
                                        (make-redirect-broker "/test" "/test/")
                                        *test-application*)
                                  :maximum-worker-count maximum-worker-count))
