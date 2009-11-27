;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def class* entry-point (broker)
  ())

(def generic entry-point-equals-for-redefinition (a b)
  (:method (a b)
    #f)

  (:method :around (a b)
           (if (eql (class-of a) (class-of b))
               (call-next-method)
               #f))

  (:method ((a broker-with-path) (b broker-with-path))
    (string= (path-of a) (path-of b)))

  (:method ((a broker-with-path-prefix) (b broker-with-path-prefix))
    (string= (path-prefix-of a) (path-prefix-of b))))

(def class* path-entry-point (entry-point broker-with-path)
  ())

(def method call-if-matches-request ((entry-point path-entry-point) request thunk)
  (when (string= (path-of entry-point) *application-relative-path*)
    (bind ((*entry-point-relative-path* ""))
      (funcall thunk))))

(def class* path-prefix-entry-point (entry-point broker-with-path-prefix)
  ())

(def method call-if-matches-request ((entry-point path-prefix-entry-point) request thunk)
  (bind (((:values matches? *entry-point-relative-path*) (starts-with-subseq (path-prefix-of entry-point) *application-relative-path* :return-suffix #t)))
    (when matches?
      (funcall thunk))))

(def (function e) ensure-entry-point (application entry-point)
  (setf (entry-points-of application)
        (delete-if (lambda (old-entry-point)
                     (entry-point-equals-for-redefinition old-entry-point entry-point))
                   (entry-points-of application)))
  (appendf (entry-points-of application) (list entry-point))
  (setf (entry-points-of application)
        (stable-sort (entry-points-of application)
                     (lambda (a b)
                       (block comparing
                         (bind ((a-priority (priority-of a))
                                (b-priority (priority-of b)))
                           (when (= a-priority
                                    b-priority)
                             (when (and (typep a 'path-prefix-entry-point)
                                        (typep b 'path-entry-point))
                               (return-from comparing #f))
                             (when (and (typep a 'path-entry-point)
                                        (typep b 'path-prefix-entry-point))
                               (return-from comparing #t))
                             (flet ((broker-path-or-path-prefix-or-nil (broker)
                                      (typecase broker
                                        (broker-with-path (path-of broker))
                                        (broker-with-path-prefix (path-prefix-of broker))
                                        (t nil))))
                               (bind ((a-path (broker-path-or-path-prefix-or-nil a))
                                      (b-path (broker-path-or-path-prefix-or-nil b)))
                                 (when (and a-path
                                            b-path)
                                   ;; if we can extract path for brokers of the same priority then the one with a longer path goes first
                                   (return-from comparing (> (length a-path) (length b-path)))))))
                           (> a-priority b-priority))))))
  entry-point)

(def function %call-with-entry-point-logic (thunk with-session-logic requires-valid-session ensure-session
                                                  with-frame-logic requires-valid-frame ensure-frame
                                                  with-action-logic)
  (when (and with-frame-logic (not with-session-logic))
    (error "Can't use WITH-FRAME-LOGIC without WITH-SESSION-LOGIC"))
  (when (and with-action-logic (not with-frame-logic))
    (error "Can't use WITH-ACTION-LOGIC without WITH-FRAME-LOGIC"))
  (when (and ensure-session (not with-session-logic))
    (error "Can't use ENSURE-SESSION without WITH-SESSION-LOGIC"))
  (when (and ensure-frame (not with-frame-logic))
    (error "Can't use ENSURE-FRAME without WITH-FRAME-LOGIC"))
  (surround-body-when with-session-logic
      (with-session-logic (:requires-valid-session requires-valid-session :ensure-session ensure-session)
        (-body-))
    (surround-body-when with-frame-logic
        (progn
          (when requires-valid-session
            (assert *session*))
          (if *session*
              (with-frame-logic (:requires-valid-frame requires-valid-frame :ensure-frame ensure-frame)
                (-body-))
              (-body-)))
      (surround-body-when with-action-logic
          (if *frame*
              (with-action-logic ()
                (-body-))
              (-body-))
        (call-in-post-action-environment *application* *session* *frame*
                                         (named-lambda call-in-post-action-environment-body ()
                                           (convert-to-primitive-response (call-in-entry-point-environment *application* *session* thunk))))))))

(def (definer e) entry-point ((application &rest args &key
                                           (with-optional-session/frame-logic #f)
                                           (with-session-logic #t)
                                           (requires-valid-session with-session-logic)
                                           (ensure-session #f)
                                           (with-frame-logic with-session-logic)
                                           (requires-valid-frame requires-valid-session)
                                           (ensure-frame #f)
                                           (with-action-logic with-frame-logic)
                                           (path nil path-p)
                                           (path-prefix nil path-prefix-p)
                                           class &allow-other-keys)
                               request-lambda-list &body body)
  (declare (ignore path path-prefix))
  (when with-optional-session/frame-logic
    ;; TODO style warn when any of these is provided
    (setf with-session-logic #t)
    (setf requires-valid-session #f)
    (setf with-frame-logic #t)
    (setf ensure-frame #t)
    (setf requires-valid-frame #f))
  (remove-from-plistf args :class
                      :with-optional-session/frame-logic
                      :with-session-logic :requires-valid-session :ensure-session
                      :with-frame-logic :requires-valid-frame :ensure-frame
                      :with-action-logic)
  (assert (not (and path-p path-prefix-p)))
  (unless class
    (when path-p
      (setf class ''path-entry-point))
    (when path-prefix-p
      (setf class ''path-prefix-entry-point)))
  (with-unique-names (entry-point request)
    `(bind ((,entry-point (make-instance ,class ,@args)))
       (setf (handler-of ,entry-point)
             (named-lambda entry-point-definer/handler (&key ((:broker ,entry-point)) ((:request ,request)) &allow-other-keys)
               (check-type ,entry-point entry-point)
               (call-if-matches-request ,entry-point ,request
                                        (lambda ()
                                          (%call-with-entry-point-logic (named-lambda entry-point-definer/body ()
                                                                          (app.debug "Entry point body reached")
                                                                          (block entry-point
                                                                            ;; BODY is in an intentionally visible block called 'ENTRY-POINT
                                                                            (with-request-params* ,request ,request-lambda-list
                                                                              ,@body)))
                                                                        ,with-session-logic ,requires-valid-session ,ensure-session
                                                                        ,with-frame-logic   ,requires-valid-frame ,ensure-frame
                                                                        ,with-action-logic)))))
       (ensure-entry-point ,application ,entry-point))))

(def (generic e) call-in-entry-point-environment (application session thunk)
  (:method (application session thunk)
    (bind ((*inside-user-code* #t))
      (funcall thunk))))

(def (definer e) file-serving-entry-point (application path-prefix root-directory &key priority)
  `(ensure-entry-point ,application (make-directory-serving-broker ,path-prefix ,root-directory :priority ,priority)))

(def (definer e) js-file-serving-entry-point (application path-prefix root-directory &key priority)
  `(ensure-entry-point ,application (make-js-directory-serving-broker ,path-prefix ,root-directory :priority ,priority)))

(def (definer e) js-component-hierarchy-serving-entry-point (application path-prefix &key priority)
  `(ensure-entry-point ,application (make-js-component-hierarchy-serving-broker ,path-prefix :priority ,priority)))

(def (definer e) cgi-serving-entry-point (application path-prefix filename &key priority environment)
  `(ensure-entry-point ,application (make-cgi-broker ,path-prefix ,filename :priority ,priority :environment ,environment)))
