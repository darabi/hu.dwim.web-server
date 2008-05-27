(in-package :wui-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test application for basic app features

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; echo application to echo back the request

(def special-variable *echo-application* (make-application :path-prefix "/echo/"))

(def entry-point (*echo-application* :path-prefix "") ()
  (make-request-echo-response))


;;;;;;;;;;;;;;;;;;;;;;;
;;; session application

(def component test-window ()
  ((id (random-simple-base-string))
   (counter 0)))

(def method render ((self test-window))
  (with-html-document-body ()
    <p "session: " ,(princ-to-string *session*)>
    <p "frame: " ,(princ-to-string *frame*)>
    <p "root component: " ,(id-of self)>
    ;;<span (:onclick `js-inline(alert "fooo")) "click-me!">
    <p
      "Counter: " ,(counter-of self)
      <br>
      <a (:href ,(action-to-href (make-action (incf (counter-of self)))))
        "increment">
      <br>
      <a (:href ,(action-to-href (make-action (decf (counter-of self)))))
        "decrement">>
    <a (:href ,(concatenate-string (path-prefix-of *application*)
                                   (if *session* "delete/" "new/")))
      ,(if *session* "drop session" "new session")>
    <hr>
    (render-request *request*)))

(def special-variable *session-application* (make-application :path-prefix "/session/"))

(def entry-point (*session-application* :path "") ()
  (if *session*
      (progn
        (assert (and (boundp '*frame*)
                     *frame*))
        (unless (typep (root-component-of *frame*) 'test-window)
          (setf (root-component-of *frame*) (make-instance 'test-window)))
        (make-root-component-rendering-response *frame*))
      (bind ((application *application*)) ; need to capture it in the closure
        (make-functional-response ((+header/content-type+ +html-content-type+))
          (emit-into-html-stream (network-stream-of *request*)
            (with-html-document-body ()
              <p "There's no session... "
                <a (:href ,(concatenate-string (path-prefix-of application) "new/"))
                  "create new session">>))))))

(def entry-point (*session-application* :path "new/" :lookup-and-lock-session #f) ()
  (bind ((new-session (make-new-session *application*))
         (old-session nil))
    (with-session/frame/action-logic ()
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

(def entry-point (*session-application* :path "delete/" :lookup-and-lock-session #f) ()
  (bind ((old-session nil))
    (with-session/frame/action-logic ()
      (setf old-session *session*)
      (values))
    (when old-session
      (with-lock-held-on-application (*application*)
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

