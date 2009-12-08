;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def (class* e) application-with-login-support (application)
  ())

(def (class* e) session-with-login-support (session)
  ((authenticated-session nil :documentation "An object representing the current authenticated session (which does not neccessarily have the same lifecycle as the web session).")))

(def method session-class list ((application application-with-login-support))
  'session-with-login-support)

(def (constant e) +login-identifier-cookie-name+           "login-identifier")
(def (constant e) +login-entry-point-path+                 "login/")
(def (constant e) +session-timed-out-query-parameter-name+ "timed-out")
(def (constant e) +user-action-query-parameter-name+       "user-action")
(def (constant e) +continue-url-query-parameter-name+      "continue-url")

(def (layered-function e) valid-login-password? (password)
  (:method (password)
    #t))

(def (layered-function e) valid-login-identifier? (identifier)
  (:method (password)
    #t))

(def (layered-function e) call-in-login-entry-point-environment (application login-encapsulation thunk)
  (:method ((application application-with-login-support) login-encapsulation thunk)
    (funcall thunk)))

(def (layered-function e) authenticate (application unregistered-new-web-session login-encapsulation)
  (:documentation "Should return an authenticated-session authentication-happened?), and (values nil #f) is a valid return value."))

(def (definer e) identifier-and-password-login-entry-point (application &rest args &key
                                                                        (path +login-entry-point-path+)
                                                                        frame-component-factory
                                                                        login-component-factory
                                                                        unauthenticated-response-factory
                                                                        &allow-other-keys)
  (unless (and (or frame-component-factory unauthenticated-response-factory)
               (not (and frame-component-factory unauthenticated-response-factory)))
    (error "You must specify a :FRAME-COMPONENT-FACTORY if using the default :UNAUTHENTICATED-RESPONSE-FACTORY for a IDENTIFIER-AND-PASSWORD-LOGIN-ENTRY-POINT ~S" -whole-))
  ;; we only transfer the provided keyword args to fall back to the defaulting rules of the entry-point definer
  (bind ((entry-point-arguments (iter (for (key value) :on args :by #'cddr)
                                      (when (member key '(:with-optional-session/frame-logic
                                                          :with-session-logic :requires-valid-session :ensure-session :with-frame-logic :requires-valid-frame :ensure-frame))
                                        (collect key)
                                        (collect value))))
         (extra-arguments (remove-from-plist args
                                             :path
                                             :with-optional-session/frame-logic
                                             :with-session-logic :requires-valid-session :ensure-session :with-frame-logic :requires-valid-frame :ensure-frame
                                             :frame-component-factory :unauthenticated-response-factory)))
    ;; make :with-session-logic default to #f
    (setf (getf entry-point-arguments :with-session-logic)
          (getf entry-point-arguments :with-session-logic #f))
    `(def (entry-point ,@-options-) (,application :path ,path ,@entry-point-arguments)
         (identifier password user-action continue-url timed-out)
       (%identifier-and-password-login-entry-point/phase1 user-action identifier password continue-url timed-out
                                                          ,path
                                                          ,frame-component-factory
                                                          ,login-component-factory
                                                          ,unauthenticated-response-factory
                                                          (list ,@extra-arguments)))))

(def function %identifier-and-password-login-entry-point/phase1 (user-action? identifier password continue-url timed-out?
                                                                              uri-path
                                                                              frame-component-factory
                                                                              login-component-factory
                                                                              unauthenticated-response-factory
                                                                              extra-arguments)
  (unless unauthenticated-response-factory
    (setf unauthenticated-response-factory
          (named-lambda unauthenticated-response-factory/default (&key identifier password user-action? (valid-input? #t) timed-out? &allow-other-keys)
            (bind ((login-component (if login-component-factory
                                        (funcall login-component-factory)
                                        (make-identifier-and-password-login/widget)))
                   (frame-component (funcall frame-component-factory login-component)))
              (setf (identifier-of login-component) identifier)
              (setf (password-of login-component) password)
              (cond
                ((and user-action?
                      (not valid-input?))
                 (add-component-error-message login-component #"login.message.invalid-user-input"))
                ((or user-action?
                     password)
                 (add-component-error-message login-component #"login.message.authentication-failed")))
              (when timed-out?
                (add-component-warning-message login-component #"login.message.session-timed-out"))
              (make-component-rendering-response frame-component)))))
  (macrolet ((cleanup-input (var)
               `(when ,var
                  (setf ,var (string-trim " " ,var))
                  (when (zerop (length ,var))
                    (setf ,var nil)))))
    (cleanup-input continue-url)
    (cleanup-input identifier)
    (cleanup-input password)
    (setf identifier (or identifier
                         (cookie-value +login-identifier-cookie-name+))))
  ;; ok, params are all ready to be processed
  (call-in-application-environment
   *application* nil
   (named-lambda %identifier-and-password-login-entry-point/phase2 ()
     (bind ((valid-input? (and (valid-login-identifier? identifier)
                               (valid-login-password? password)))
            (response nil)
            (authentication-happened? nil))
       (app.debug "IDENTIFIER-AND-PASSWORD-ENTRY-POINT checks valid-input? ~S and user-action? ~S" valid-input? user-action?)
       (or (when (and valid-input?
                      user-action?)
             (bind ((login-encapsulation (make-instance 'identifier-and-password-login-encapsulation
                                                        :identifier identifier
                                                        :password password
                                                        :extra-arguments extra-arguments)))
               (setf (values response authentication-happened?)
                     (call-in-login-entry-point-environment *application* login-encapsulation
                                                            (named-lambda %identifier-and-password-login-entry-point/phase3 ()
                                                              (%identifier-and-password-login-entry-point/phase4 login-encapsulation continue-url)))))
             (if response
                 (progn
                   (when authentication-happened?
                     (app.dribble "IDENTIFIER-AND-PASSWORD-ENTRY-POINT is decorating the response with the login identifier cookie value: ~S" identifier)
                     (add-cookie (make-cookie +login-identifier-cookie-name+ identifier
                                              :max-age #.(* 60 60 24 365 100)
                                              :domain (string+ "." (host-of (uri-of *request*)))
                                              :path (string+ (path-prefix-of *application*) uri-path))
                                 response))
                   response)))
           (funcall unauthenticated-response-factory
                    :identifier identifier :password password
                    :user-action? user-action? :valid-input? valid-input? :timed-out? timed-out?))))))

(def function %identifier-and-password-login-entry-point/phase4 (login-encapsulation continue-url)
  (block authenticating
    (bind ((application *application*)
           (session *session*)
           (new-session? #f))
      (flet ((return-result (response authentication-happened?)
               (return-from authenticating
                 (values response authentication-happened?))))
        (app.debug "Login entry point called with login-encapsulation ~A, continue-url ~S" login-encapsulation continue-url)
        (unless session
          (with-session-logic (:requires-valid-session #f :lock-session #f)
            (app.debug "Login entry point reached while we already have a web session: ~A" *session*)
            (setf session *session*)))
        (if session
            (when (authenticated-session-of session)
              (app.debug "Login entry point reached while we already have an authenticated session, bailing out...")
              (return-result nil #f))
            (progn
              (setf session (make-new-session application))
              (setf new-session? #t)))
        (bind ((authenticated-session (authenticate application session login-encapsulation)))
          (if authenticated-session
              (progn
                (app.debug "~S returned ~S" 'authenticate authenticated-session)
                (setf *session* session)
                (when new-session?
                  (app.debug "Registering new web session ~A" session)
                  (with-lock-held-on-application (application)
                    (register-session application session)))
                (setf (authenticated-session-of session) authenticated-session)
                (app.debug "Login entry point is preparing the response, continue-url is ~S" continue-url)
                (return-result (decorate-application-response application (if continue-url
                                                                              (make-redirect-response continue-url)
                                                                              (make-redirect-response-for-current-application)))
                               #t))))))))

(def (class* ea) login-encapsulation ()
  ((extra-arguments '()))
  (:documentation "A login-encapsulation is an object that packages up login information and can be used for dispatching in a later phase of authentication. The EXTRA-ARGUMENTS slot can hold some &rest keyword arguments that is useful later on."))

(def (class* ea) identifier-and-password-login-encapsulation (login-encapsulation)
  ((identifier)
   (password)))

(def print-object identifier-and-password-login-encapsulation
  (write-string "identifier: ")
  (write (identifier-of -self-)))
