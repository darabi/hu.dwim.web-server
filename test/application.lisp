;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui.test)

;;;;;;
;;; Test application for basic app features

(def class* test-application (standard-application)
  ()
  (:default-initargs :dojo-directory-name (find-latest-dojo-directory-name (asdf:system-relative-pathname :hu.dwim.wui "www/"))))

(def special-variable *test-application* (make-instance 'test-application :path-prefix "/test/"))

(def entry-point (*test-application* :path "performance")
  (with-request-parameters (name)
    (make-functional-html-response ()
      (emit-html-document ()
        <h3 ,(or name "The name query parameter is not specified!")>))))

(def method authenticate ((application test-application) (session session-with-login-support) (login-data login-data/identifier-and-password))
  (bind ((entry (assoc (identifier-of login-data) '(("test" "test123")
                                                    ("joe" nil))
                       :test 'equal)))
    (if (and entry
             (equal (second entry) (password-of login-data)))
        (first entry)
        ;; authentication failed
        nil)))

(def entry-point (*test-application* :path +login-entry-point-path+)
  (with-entry-point-logic/login-with-identifier-and-password ()
    (make-raw-functional-response ()
      (emit-simple-html-document-http-response (:title "Login" :status +http-ok+)
        (if *session*
            <table ()
              <tr <td "*session*"> <td ,(princ-to-string *session*) >>
              <tr <td "(authenticate-return-value-of *session*)"> <td ,(princ-to-string (authenticate-return-value-of *session*)) >>>
            <p "There's no *session*">)
        (flet ((render-login-link (identifier password)
                 <a (:href ,(bind ((uri (make-uri-for-current-application +login-entry-point-path+)))
                              (setf (uri-query-parameter-value uri "identifier") identifier)
                              (setf (uri-query-parameter-value uri "password") password)
                              (setf (uri-query-parameter-value uri "user-action") "t")
                              (print-uri-to-string uri)))
                    "Login with " ,identifier "/" ,password >
                 <br>))
          (render-login-link "test" "test123")
          (render-login-link "test" "wrong-password")
          (render-login-link "joe" nil))
        <a (:href ,(print-uri-to-string (make-uri-for-current-application "logout/")))
           "Logout">))))

(def entry-point (*test-application* :path "logout/")
  (with-session-logic (:requires-valid-session #f)
    (when *session*
      (logout-current-session))
    (make-redirect-response (make-uri-for-current-application +login-entry-point-path+))))

(def function render-mime-part-details (mime-part)
  <p "Mime part headers:">
  <table
    <thead <tr <th "Header">
               <th "Value">>>
    ,@(iter (for header :in (rfc2388-binary:headers mime-part))
            (collect <tr
                      <td ,(or (rfc2388-binary:header-name header) "")>
                      <td ,(or (rfc2388-binary:header-value header) "")>>))>
  <table
    <thead <tr <th "Property">
               <th "Value">>>
    <tr <td "Content">
        <td ,(princ-to-string (rfc2388-binary:content mime-part))>>
    <tr <td "Content charset">
        <td ,(rfc2388-binary:content-charset mime-part)>>
    <tr <td "Content length">
        <td ,(rfc2388-binary:content-length mime-part)>>
    <tr <td "Content type">
        <td ,(rfc2388-binary:content-type mime-part)>>>)

(def entry-point (*test-application* :path "params")
  (with-request-parameters ((number "0" number?) ((the-answer "theanswer") "not supplied" the-answer?))
    (make-raw-functional-response ()
      (emit-simple-html-document-http-response (:title "foo")
        <p "Parameters:"
          <a (:href ,(string+ (path-prefix-of *test-application*)
                                         (if (or number? the-answer?)
                                             "params"
                                             "params?theanswer=yes&number=42")))
            "try this">>
        <table
          <thead <tr <td "Parameter name">
                     <td "Parameter value">>>
          ,@(do-parameters (name value)
              <tr
                <td ,name>
                <td ,(etypecase value
                       (string value)
                       (list (princ-to-string value))
                       (rfc2388-binary:mime-part
                        (render-mime-part-details value)))>>)>
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
        (render-request *request*)))))

;;;;;;
;;; Echo application to echo back the request

(def special-variable *echo-application* (make-instance 'standard-application :path-prefix "/echo/"))

(def entry-point (*echo-application* :path-prefix "")
  (make-request-echo-response))

;;;;;;
;;; Session application

;; TODO this example might be obsolete and/or out of date

(def special-variable *session-application* (make-instance 'standard-application :path-prefix "/session/"))

(def entry-point (*session-application* :path "" )
  (with-entry-point-logic (:requires-valid-session #f :ensure-session #f :requires-valid-frame #f :ensure-frame #t)
    (if *session*
        (progn
          (assert (and (boundp '*frame*)
                       *frame*))
          (make-raw-functional-response ()
            (emit-simple-html-document-http-response ()
              <p "We have a session now... "
                 <span ,(or (root-component-of *frame*)
                            (setf (root-component-of *frame*) "Hello world from a session!"))>
                 <a (:href ,(string+ (path-prefix-of *application*) "delete/"))
                    "delete session">>)))
        (bind ((application *application*)) ; need to capture it in the closure
          (make-raw-functional-response ()
            (emit-simple-html-document-http-response ()
              <p "There's no session... "
                 <a (:href ,(string+ (path-prefix-of application) "new/"))
                    "create new session">>))))))

(def entry-point (*session-application* :path "new/")
  (with-entry-point-logic (:requires-valid-session #f :ensure-session #t :requires-valid-frame #f)
    (make-redirect-response-for-current-application)))

(def entry-point (*session-application* :path "delete/")
  (bind ((old-session nil))
    (with-session-logic ()
      (setf old-session *session*)
      (values))
    (when old-session
      (with-lock-held-on-application (*application*)
        (delete-session *application* old-session)))
    (make-redirect-response (path-prefix-of *application*))))

(def file-serving-entry-point *session-application* "/session/static/" (system-relative-pathname :hu.dwim.wui.test "www/"))

(def function start-test-server-with-test-applications (&key (maximum-worker-count 16) (log-level +debug+))
  (setf (log-level 'wui) log-level)
  (start-test-server-with-brokers (list (make-redirect-broker "/session" "/session/")
                                        *session-application*
                                        (make-redirect-broker "/echo" "/echo/")
                                        *echo-application*
                                        (make-redirect-broker "/test" "/test/")
                                        *test-application*)
                                  :maximum-worker-count maximum-worker-count))
