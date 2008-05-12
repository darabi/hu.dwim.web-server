(in-package :wui-test)

(def special-variable *test-application* (make-application :path-prefix "/test/"))

(def entry-point (*test-application* :path "params") ((number "0" number?) ((the-answer "theanswer") "not supplied" the-answer?))
  (make-functional-html-response ()
    (with-html-document-body (:title "foo")
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

(def special-variable *echo-application* (make-application :path-prefix "/echo/"))

(def entry-point (*echo-application* :path-prefix "") ()
  +request-echo-response+)

(def special-variable *session-application* (make-application :path-prefix "/session/"))

(def entry-point (*session-application* :path "") ()
  (bind ((session *session*)
         (application *application*))
    (make-functional-response ((+header/content-type+ +html-content-type+))
      (emit-into-html-stream (network-stream-of *request*)
        (with-html-document-body ()
          <p "session: " ,(princ-to-string session)
          <a (:href ,(concatenate-string (path-prefix-of application)
                                         (if session "delete/" "new/")))
            ,(if session "drop session" "new session")>>
          <hr>
          (render-request *request*))))))

(def entry-point (*session-application* :path "new/" :lookup-and-lock-session #f) ()
  (bind ((new-session (make-new-session *application*))
         (old-session nil))
    (prog1
        (with-looked-up-and-locked-session
          ;; all this is not necessary here for building a simple redirect response,
          ;; but it's here for demonstrational purposes.
          (setf old-session *session*)
          (setf *session* new-session)
          (make-redirect-response (path-prefix-of *application*)))
      ;; we may only lock the app again after our session's lock
      ;; has been released. this is to avoid deadlocks by strictly following the
      ;; app -> session locking order...
      (with-lock-held-on-application *application*
        (when old-session
          (delete-session *application* old-session))
        (register-session *application* new-session)))))

(def entry-point (*session-application* :path "delete/" :lookup-and-lock-session #f) ()
  (bind ((old-session nil))
    (with-looked-up-and-locked-session
      (setf old-session *session*))
    (when old-session
      (with-lock-held-on-application *application*
        (delete-session *application* old-session)))
    (make-redirect-response (path-prefix-of *application*))))

(ensure-entry-point *session-application*
                    (make-file-serving-broker "/session/wui/" (project-relative-pathname "")))

(def function start-server-with-test-applications (&key (maximum-worker-count 16) (log-level +dribble+))
  (with-logger-level wui log-level
    (start-server-with-brokers (list (make-redirect-broker "/session" "/session/")
                                     *session-application*
                                     (make-redirect-broker "/echo" "/echo/")
                                     *echo-application*
                                     (make-redirect-broker "/test" "/test/")
                                     *test-application*)
                               :maximum-worker-count maximum-worker-count)))

