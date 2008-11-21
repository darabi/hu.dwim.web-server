;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Time provider

(def component time-provider (content-component)
  ((time :type prc::timestamp)))

(def call-in-component-environment time-provider ()
  (prc::call-with-time (time-of -self-) #'call-next-method))

;;;;;;
;;; Time selector

(def component time-selector (timestamp-inspector)
  ((edited #t)))

(def (macro e) time-selector (time)
  `(make-instance 'time-selector :component-value ,time))

;;;;;;
;;; Validity provider

(def component validity-provider (content-component)
  ((validity-begin :type prc::timestamp)
   (validity-end :type prc::timestamp)))

(def (macro e) validity-provider ((&key validity validity-begin validity-end) &body forms)
  `(make-instance 'validity-provider
                  :content (progn ,@forms)
                  :validity-begin ,(if validity
                                       (prc::first-moment-for-partial-timestamp validity)
                                       validity-begin)
                  :validity-end ,(if validity
                                     (prc::last-moment-for-partial-timestamp validity)
                                     validity-end)))

(def call-in-component-environment validity-provider ()
  (prc::call-with-validity-range (validity-begin-of -self-) (validity-end-of -self-) #'call-next-method))

;;;;;;
;;; Validity selector

(def component validity-selector (member-inspector)
  ()
  (:default-initargs :edited #t :possible-values '(2007 2008 2009) :client-name-generator #'integer-to-string))

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
;;; Validity selector and provider

(def component validity-selector-and-provider (validity-selector content-component)
  ())

(def (macro e) validity-selector-and-provider ((&key validity) &body forms)
  `(make-instance 'validity-selector-and-provider
                  :content (progn ,@forms)
                  :component-value ,(if (stringp validity)
                                        (parse-integer validity)
                                        validity)))

(def render validity-selector-and-provider ()
  <div ,(call-next-method)
       ,(render (content-of -self-)) >)

(def call-in-component-environment validity-selector-and-provider ()
  (bind ((year (component-value-of -self-)))
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

(def call-in-component-environment coordinates-provider ()
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

(def render :before coordinates-dependent-component-mixin ()
  (setf (coordinates-of -self-)
        (iter (for dimension :in (dimensions-of -self-))
              (for old-coordinate :in (coordinates-of -self-))
              (for new-coordinate = (prc:coordinate dimension))
              (unless (prc:coordinate-equal dimension old-coordinate new-coordinate)
                (mark-outdated -self-))
              (collect new-coordinate)))
  (ensure-uptodate -self-))