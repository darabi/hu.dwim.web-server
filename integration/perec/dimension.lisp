;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; dimensions/mixin

(def (component e) dimensions/mixin ()
  ((dimensions :type list)))

(def constructor dimensions/mixin ()
  (bind (((:slots dimensions) -self-))
    (setf dimensions (mapcar 'hu.dwim.perec:lookup-dimension dimensions))))

;;;;;;
;;; coordinates-dependent/mixin

(def (component e) coordinates-dependent/mixin (dimensions/mixin)
  ((coordinates
    nil
    :type list
    :computed-in computed-universe/session)))

(def refresh-component coordinates-dependent/mixin
  (bind (((:slots dimensions coordinates) -self-))
    (setf coordinates (mapcar 'hu.dwim.perec:coordinate dimensions))))

(def method to-be-refreshed-component? :around ((self coordinates-dependent/mixin))
  (or (call-next-method)
      (bind (((:read-only-slots dimensions coordinates) self))
        (iter (for dimension :in dimensions)
              (for coordinate :in coordinates)
              ;; KLUDGE: ensure-list works because a range coordinate is already a list
              (unless (hu.dwim.perec:coordinate-equal dimension (ensure-list coordinate) (ensure-list (hu.dwim.perec:coordinate dimension)))
                (return-from to-be-refreshed-component? #t))))))

;;;;;;
;;; coordinates-provider/mixin

(def (component e) coordinates-provider/mixin (dimensions/mixin)
  ((coordinates
    :type list
    :computed-in computed-universe/session)))

(def component-environment coordinates-provider/mixin
  (bind (((:read-only-slots dimensions coordinates) -self-))
    (hu.dwim.perec:with-coordinates dimensions (force coordinates)
      (with-error-log-decorator (make-error-log-decorator
                                  (format t "~%The environment of the coordinates-provider ~A follows:" -self-)
                                  (foreach [format t "~%  ~S: ~@<~A~:>" (hu.dwim.perec:name-of !1) !2] dimensions (force coordinates)))
        (call-next-method)))))

;;;;;;
;;; t/alternator/inspector

(def layered-method make-alternatives ((component t/alternator/inspector) (class standard-class) (prototype hu.dwim.perec::dimension) (value hu.dwim.perec::dimension))
  (list* (make-instance 'dimension/documentation/inspector
                        :component-value value
                        :component-value-type (component-value-type-of component))
         (call-next-layered-method)))

;;;;;;
;;; dimension/documentation/inspector

(def (component e) dimension/documentation/inspector (t/documentation/inspector title/mixin)
  ())

(def method make-documentation ((component dimension/documentation/inspector) (class standard-class) (prototype hu.dwim.perec::dimension) (value hu.dwim.perec::dimension))
  (hu.dwim.perec::documentation-of value))

(def render-component dimension/documentation/inspector
  (render-title-for -self-)
  (render-contents-for -self-))

(def render-xhtml dimension/documentation/inspector
  (with-render-style/component (-self-)
    (render-title-for -self-)
    (render-contents-for -self-)))

(def layered-method make-title ((self dimension/documentation/inspector) (class standard-class) (prototype hu.dwim.perec::dimension) (value hu.dwim.perec::dimension))
  (title/widget ()
    (localized-dimension-name value :capitalize-first-letter #t)))
