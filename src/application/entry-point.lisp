;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def class* entry-point (broker)
  ((priority 0)
   (handler :type function))
  (:metaclass funcallable-standard-class))

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
    self (lambda (request application relative-path)
           (path-entry-point-handler self request application relative-path))))

(def function path-entry-point-handler (entry-point request application relative-path)
  (declare (ignore application))
  (when (string= (path-of entry-point) relative-path)
    (produce-response entry-point request)))

(def class* path-prefix-entry-point (entry-point broker-with-path-prefix)
  ()
  (:metaclass funcallable-standard-class))

(def constructor path-prefix-entry-point
  (set-funcallable-instance-function
    self (lambda (request application relative-path)
           (path-prefix-entry-point-handler self request application relative-path))))

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
  entry-point)

(def (definer e) entry-point ((application &rest args &key
                                           (lookup-and-lock-session #t)
                                           (path nil path-p)
                                           (path-prefix nil path-prefix-p)
                                           class &allow-other-keys)
                               request-lambda-list &body body)
  (declare (ignore path path-prefix))
  (remove-from-plistf args :class :lookup-and-lock-session)
  (assert (not (and path-p path-prefix-p)))
  (unless class
    (when path-p
      (setf class ''path-entry-point))
    (when path-prefix-p
      (setf class ''path-prefix-entry-point)))
  (with-unique-names (request)
    `(ensure-entry-point ,application
                         (make-instance
                          ,class ,@args
                          :handler (lambda (,request)
                                     (with-request-params ,request ,request-lambda-list
                                       ,(if lookup-and-lock-session
                                            `(with-looked-up-and-locked-session *application*
                                               ,@body)
                                            `(progn
                                               ,@body))))))))
