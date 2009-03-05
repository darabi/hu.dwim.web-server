;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Time provider

(def component time-provider (content-component)
  ((time :type prc::timestamp)))

(def component-environment time-provider
  (prc::call-with-time (time-of -self-) #'call-next-method))

;;;;;;
;;; Time selector

(def component time-selector (timestamp-inspector)
  ()
  (:default-initargs :edited #t))

(def (macro e) time-selector (time)
  `(make-instance 'time-selector :component-value ,time))

;;;;;;
;;; Validity selector

(def component validity-selector (member-inspector)
  ()
  (:default-initargs :edited #t :possible-values '(2007 2008 2009) :client-name-generator [integer-to-string !2]))

(def (macro e) validity-selector (&key validity)
  `(make-instance 'validity-selector
                  :component-value ,(if (stringp validity)
                                        (parse-integer validity)
                                        validity)))

(def function compute-timestamp-range (component)
  (bind (((:read-only-slots single) component))
    (if single
        (bind ((partial-timestamp-string (princ-to-string (component-value-of (range-of component)))))
          (list (prc::first-moment-for-partial-timestamp partial-timestamp-string)
                (prc::last-moment-for-partial-timestamp partial-timestamp-string)))
        (not-yet-implemented))))

;;;;;;
;;; Validity provider

(def component validity-provider (content-component)
  ((selector :type component)))

(def (macro e) validity-provider ((&key validity) &body forms)
  `(make-instance 'validity-provider
                  :content (progn ,@forms)
                  :selector (validity-selector :validity ,validity)))

(def render validity-provider ()
  <div ,(render (selector-of -self-))
       ,(call-next-method) >)

(def component-environment validity-provider
  (bind ((year (component-value-of (selector-of -self-))))
    (prc::call-with-validity-range (local-time:encode-timestamp 0 0 0 0 1 1 year :offset 0)
                                   (local-time:encode-timestamp 0 0 0 0 1 1 (1+ year) :offset 0)
                                   #'call-next-method)))

;;;;;;
;;; Coordinates provider

(def component coordinates-provider (content-component)
  ((dimensions)
   (coordinates)))

(def (macro e) coordinates-provider (dimensions coordinates &body content)
  `(make-instance 'coordinates-provider
                  :dimensions (mapcar #'prc:lookup-dimension ,dimensions)
                  :coordinates ,coordinates
                  :content (progn ,@content)))

(def component-environment coordinates-provider
  (bind (((:read-only-slots dimensions coordinates) -self-))
    (prc:with-coordinates dimensions (force coordinates)
      (call-next-method))))

;;;;;;
;;; Coordinates dependent component mixin

(def component coordinates-dependent-component-mixin ()
  ((dimensions)
   (coordinates)))

(def constructor coordinates-dependent-component-mixin ()
  (with-slots (dimensions coordinates) -self-
    (setf dimensions (mapcar 'prc:lookup-dimension dimensions)
          coordinates (prc:make-empty-coordinates dimensions))))

(def (function e) print-object/coordinates-dependent-component-mixin (self)
  (princ "dimensions: ")
  (princ (if (slot-boundp self 'dimensions)
             (hu.dwim.wui::dimensions-of self)
             "<unbound>"))
  (princ ", coordinates: ")
  (princ (if (slot-boundp self 'coordinates)
             (hu.dwim.wui::coordinates-of self)
             "<unbound>")))

(def render :before coordinates-dependent-component-mixin ()
  (setf (coordinates-of -self-)
        (iter (for dimension :in (dimensions-of -self-))
              (for old-coordinate :in (coordinates-of -self-))
              (for new-coordinate = (prc:coordinate dimension))
              (unless (prc:coordinate-equal dimension (ensure-list old-coordinate) (ensure-list new-coordinate))
                (mark-outdated -self-))
              (collect new-coordinate))))

(def (function e) coordinates-bound-according-to-dimension-type-p (component)
  (iter (for dimension :in (dimensions-of component))
        (always (etypecase dimension
                  (prc::inheriting-dimension #t)
                  (prc::ordering-dimension #t)
                  (prc::dimension
                   (typep (prc::coordinate dimension) (prc::the-type-of dimension)))))))
