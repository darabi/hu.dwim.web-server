;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def (constant e) +login-identifier-cookie-name+           "login-identifier")
(def (constant e) +login-entry-point-path+                 "login/")
(def (constant e) +logout-entry-point-path+                "logout/")
(def (constant e) +session-timed-out-query-parameter-name+ "timed-out")
(def (constant e) +user-action-query-parameter-name+       "user-action")
(def (constant e) +continue-url-query-parameter-name+      "continue-url")

(def (layered-function e) valid-login-password? (password)
  (:method (password)
    #t))

(def (layered-function e) valid-login-identifier? (identifier)
  (:method (password)
    #t))

(def function extract-login-data/identifier-and-password ()
  (with-request-parameters (identifier password)
    (string/trim-whitespace-and-maybe-nil-it identifier)
    (string/trim-whitespace-and-maybe-nil-it password)
    (setf identifier (or identifier
                         (cookie-value +login-identifier-cookie-name+)))
    (when (and (valid-login-identifier? identifier)
               (valid-login-password? password))
      (make-instance 'login-data/identifier-and-password
                     :identifier identifier
                     :password password))))

(def function decorate-login-response/identifier-and-password (response login-data)
  (assert response)
  (bind ((identifier (identifier-of login-data)))
    (when identifier
      (app.dribble "DECORATE-LOGIN-RESPONSE/IDENTIFIER-AND-PASSWORD is decorating the response with the login identifier cookie value: ~S" identifier)
      (add-cookie (make-cookie +login-identifier-cookie-name+ identifier
                               :max-age #.(* 60 60 24 365 100)
                               :domain (string+ "." (host-of (uri-of *request*)))
                               :path (path-of (uri-of *request*)))
                  response)))
  response)

(def (macro e) with-entry-point-logic/login-with-identifier-and-password ((&rest extra-arguments) &body body)
  `(with-entry-point-logic/login ('extract-login-data/identifier-and-password
                                  'decorate-login-response/identifier-and-password
                                  ,@extra-arguments)
     ,@body))

(def (with-macro* e) with-entry-point-logic/login (login-data-extractor response-decorator &rest extra-arguments)
  (remove-from-plistf extra-arguments :login-data-extractor :response-decorator)
  (with-request-parameters (continue-url ((user-action? +user-action-query-parameter-name+) #f)
                                         ((timed-out? +session-timed-out-query-parameter-name+) #f))
    (string/trim-whitespace-and-maybe-nil-it continue-url)
    (with-entry-point-logic (:with-optional-session/frame-logic #t)
      (bind ((login-data (funcall login-data-extractor))
             (response nil))
        (app.debug "WITH-ENTRY-POINT-LOGIC/LOGIN, login-data is ~S, user-action? is ~S, continue-url is ~S" login-data user-action? continue-url)
        (when login-data
          (setf (extra-arguments-of login-data) extra-arguments)
          (if user-action?
              (block call-login
                (handler-bind ((error/login-failed (lambda (error)
                                                     (declare (ignore error))
                                                     (return-from call-login nil))))
                  (app.dribble "WITH-ENTRY-POINT-LOGIC/LOGIN will now call LOGIN")
                  (setf *session* (login *application* *session* login-data))))
              (app.debug "WITH-ENTRY-POINT-LOGIC/LOGIN skipped calling LOGIN because it's not a user-action"))
          (setf response (if continue-url
                             (make-redirect-response continue-url)
                             (-with-macro/body- (login-data '-login-data- :ignorable #t))))
          (app.dribble "WITH-ENTRY-POINT-LOGIC/LOGIN received the response ~A" response)
          ;; it's a wierd situation if there's no response at this point, but let's keep the flexibility...
          (when response
            (app.dribble "WITH-ENTRY-POINT-LOGIC/LOGIN is calling response-decorator ~S" response-decorator)
            ;; and we return with the decorated response
            (setf response (decorate-application-response *application* (funcall response-decorator response login-data)))))
        response))))
