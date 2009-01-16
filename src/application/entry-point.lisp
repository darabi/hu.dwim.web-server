;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def class* entry-point (broker)
  ((handler :type function))
  (:metaclass funcallable-standard-class))

(def method priority-of ((broker broker))
  0)

(defmethod produce-response ((entry-point entry-point) request)
  (funcall (handler-of entry-point) request))

(defgeneric entry-point-equals-for-redefinition (a b)
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
  ()
  (:metaclass funcallable-standard-class))

(def constructor path-entry-point
  (set-funcallable-instance-function
    -self- (lambda (request application relative-path)
             (path-entry-point-handler -self- request application relative-path))))

(def function path-entry-point-handler (entry-point request application relative-path)
  (declare (ignore application))
  (when (string= (path-of entry-point) relative-path)
    (produce-response entry-point request)))

(def class* path-prefix-entry-point (entry-point broker-with-path-prefix)
  ()
  (:metaclass funcallable-standard-class))

(def constructor path-prefix-entry-point
  (set-funcallable-instance-function
    -self- (lambda (request application relative-path)
             (path-prefix-entry-point-handler -self- request application relative-path))))

(def function path-prefix-entry-point-handler (entry-point request application relative-path)
  (declare (ignore application))
  (when (starts-with-subseq (path-prefix-of entry-point) relative-path)
    (produce-response entry-point request)))

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
                             (bind ((a-path (broker-path-or-path-prefix-or-nil a))
                                    (b-path (broker-path-or-path-prefix-or-nil b)))
                               (when (and a-path
                                          b-path)
                                 ;; if we can extract path for brokers of the same priority then the one with a longer path goes first
                                 (return-from comparing (> (length a-path) (length b-path))))))
                           (> a-priority b-priority))))))
  entry-point)

(def (definer e) entry-point ((application &rest args &key
                                           (with-optional-session/frame-logic #f)
                                           (with-session-logic #t)
                                           (requires-valid-session with-session-logic)
                                           (ensure-session #f)
                                           (with-frame-logic with-session-logic)
                                           (requires-valid-frame #t)
                                           (ensure-frame #f)
                                           (with-action-logic with-frame-logic)
                                           (path nil path-p)
                                           (path-prefix nil path-prefix-p)
                                           class &allow-other-keys)
                               request-lambda-list &body body)
  (declare (ignore path path-prefix))
  (bind ((boolean-values (list with-session-logic ensure-session requires-valid-session
                               with-frame-logic requires-valid-frame ensure-frame
                               with-action-logic)))
    (unless (every [typep !1 'boolean] boolean-values)
      (error "The entry-point definer does not evaluate many of its boolean keyword arguments, so they must be either T or NIL. Please check them: ~S" boolean-values)))
  (when with-optional-session/frame-logic
    (setf with-session-logic #t)
    (setf requires-valid-session #f)
    (setf with-frame-logic #t)
    (setf requires-valid-frame #f))
  (when (and with-frame-logic (not with-session-logic))
    (error "Can't use WITH-FRAME-LOGIC without WITH-SESSION-LOGIC"))
  (when (and with-action-logic (not with-frame-logic))
    (error "Can't use WITH-ACTION-LOGIC without WITH-FRAME-LOGIC"))
  (remove-from-plistf args :class
                      :with-optional-session/frame-logic
                      :with-session-logic :requires-valid-session :ensure-session
                      :with-frame-logic :requires-valid-frame :ensure-frame
                      :with-action-logic)
  (assert (not (and path-p path-prefix-p)))
  (assert (or (not ensure-session) with-session-logic) () "It's quite contradictory to ask for ENSURE-SESSION without WITH-SESSION-LOGIC")
  (assert (or (not ensure-frame) with-frame-logic) () "It's quite contradictory to ask for ENSURE-FRAME without WITH-FRAME-LOGIC")
  (unless class
    (when path-p
      (setf class ''path-entry-point))
    (when path-prefix-p
      (setf class ''path-prefix-entry-point)))
  (bind ((wrapper-expression '(entry-point)))
    (when with-action-logic
      (setf wrapper-expression
            `(if *frame*
                 (with-action-logic ()
                   ,wrapper-expression)
                 ,wrapper-expression)))
    (when with-frame-logic
      (setf wrapper-expression
            `(if *session*
                 (with-frame-logic (:requires-valid-frame ,requires-valid-frame :ensure-frame ,ensure-frame)
                   ,wrapper-expression)
                 ,wrapper-expression)))
    (when with-session-logic
      (setf wrapper-expression
            `(with-session-logic (:requires-valid-session ,requires-valid-session :ensure-session ,ensure-session)
               ,wrapper-expression)))
    (with-unique-names (request)
      `(ensure-entry-point ,application
                           (make-instance
                            ,class ,@args
                            :handler (lambda (,request)
                                       (flet ((entry-point ()
                                                ;; in an intentionally visible BLOCK called 'entry-point
                                                (with-request-params* ,request ,request-lambda-list
                                                  ,@body)))
                                         ,wrapper-expression)))))))

(def (definer e) file-serving-entry-point (application path-prefix root-directory &key priority)
  `(ensure-entry-point ,application (make-file-serving-broker ,path-prefix ,root-directory :priority ,priority)))

(def (definer e) js-file-serving-entry-point (application path-prefix root-directory &key priority)
  `(ensure-entry-point ,application (make-js-file-serving-broker ,path-prefix ,root-directory :priority ,priority)))
