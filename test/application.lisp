(in-package :wui-test)

(def special-variable *test-application* (make-application :path-prefix "/test/"))

(def entry-point (*test-application* :path "params") ((number "0" number?) ((the-answer "theanswer") "not supplied" the-answer?))
  (make-functional-html-response ()
    (with-html-document-body (:title "foo")
      <p "number: " ,(princ-to-string number)>
      <p "the-answer: " ,(princ-to-string the-answer)>
      <a (:href ,(concatenate-string (path-prefix-of *test-application*)
                                     (if (or number? the-answer?)
                                         "params"
                                         "params?theanswer=yes&number=42")))
        "try this">)))

(def special-variable *echo-application* (make-application :path-prefix "/echo/"))

(def entry-point (*echo-application* :path-prefix "") ()
  +request-echo-response+)

(def special-variable *session-application* (make-application :path-prefix "/session/"))

(def entry-point (*session-application* :path "") ()
  (bind ((session *session*))
    (make-functional-response ((+header/content-type+ +html-content-type+))
      (emit-into-html-stream (network-stream-of *request*)
        (with-html-document-body ()
          <p "session: " ,(princ-to-string session)>
          <hr>
          (render-request *request*))))))

(def entry-point (*session-application* :path "new/") ()
  (values (make-redirect-response (path-prefix-of *application*))
          (lambda ()
            ;; we need to supply this in a callback that is called after the session
            ;; has been released to avoid deadlocks by strictly following the
            ;; app -> session locking order...
            (with-lock-held-on-application *application*
              (make-new-session *application*)))))

(def function start-server-with-test-applications (&key (maximum-worker-count 16) (log-level +dribble+))
  (with-logger-level wui log-level
    (start-server-with-brokers (list (make-redirect-broker "/session" "/session/")
                                     *session-application*
                                     (make-redirect-broker "/echo" "/echo/")
                                     *echo-application*
                                     (make-redirect-broker "/test" "/test/")
                                     *test-application*)
                               :maximum-worker-count maximum-worker-count)))

