;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui.test)

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
