(in-package :wui-test)

(def special-variable *test-application* (make-application :path-prefix "/test/"))

(def entry-point (*test-application* :path "params") ((number "0" number?) ((the-answer "theanswer") "not supplied" the-answer?))
  (make-functional-response ((+header/content-type+ (content-type-for +html-mime-type+ *encoding*)))
    (emit-into-html-stream (network-stream-of *request*)
      (with-simple-html-body (:title "foo")
        <p "number: " ,number>
        <p "the-answer: " ,the-answer>
        <a (:href ,(concatenate-string (path-prefix-of *test-application*)
                                       (if (or number? the-answer?)
                                           "params"
                                           "params?theanswer=yes&number=42")))
          "try this">))))

(def special-variable *echo-application* (make-application :path-prefix "/echo/"))

(def entry-point (*echo-application* :path-prefix "") ()
  (make-request-echo-response))

(def function start-server-with-test-applications (&key (maximum-worker-count 16) (log-level +dribble+))
  (with-logger-level wui log-level
    (start-server-with-brokers (list (make-redirect-broker "/echo" "/echo/")
                                     *echo-application*
                                     (make-redirect-broker "/test" "/test/")
                                     *test-application*)
                               :maximum-worker-count maximum-worker-count)))
