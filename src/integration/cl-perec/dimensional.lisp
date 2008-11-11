;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Time provider

(def component time-provider-component (content-component)
  ((time :type prc::timestamp)))

(def call-in-component-environment time-provider-component ()
  (prc::call-with-time (time-of -self-) #'call-next-method
    (call-next-method)))

;;;;;;
;;; Time selector

(def component time-selector-component (content-component)
  ((time :type component)))

(def render time-selector-component ()
  (bind (((:read-only-slots time) -self-))
    <div ,(render time)
         ,(call-next-method)>))

;;;;;;
;;; Validity provider

(def component validity-provider-component (content-component)
  ((validity-begin :type prc::timestamp)
   (validity-end :type prc::timestamp)))

(def (macro e) validity-provider ((&key validity validity-begin validity-end) &body forms)
  `(make-instance 'validity-provider-component
                  :content (progn ,@forms)
                  :validity-begin ,(if validity
                                       (prc::first-moment-for-partial-timestamp validity)
                                       validity-begin)
                  :validity-end ,(if validity
                                     (prc::last-moment-for-partial-timestamp validity)
                                     validity-end)))

(def call-in-component-environment validity-provider-component ()
  (prc::call-with-validity-range (validity-begin-of -self-) (validity-end-of -self-) #'call-next-method))

;;;;;;
;;; Validity selector

(def component validity-selector-component (content-component)
  ((range :type component)))

(def constructor validity-selector-component ()
  (begin-editing (range-of -self-)))

(def (macro e) validity-selector ((&key validity validity-begin validity-end) &body forms)
  `(make-instance 'validity-selector-component
                  :content (progn ,@forms)
                  :range (timestamp-range :range-start ,(if validity
                                                            (prc::first-moment-for-partial-timestamp validity)
                                                            validity-begin)
                                          :range-end ,(if validity
                                                          (prc::last-moment-for-partial-timestamp validity)
                                                          validity-end))))

(def render validity-selector-component ()
  (bind (((:read-only-slots range) -self-))
    <div ,(render range)
         ,(call-next-method)>))

(def function compute-timestamp-range (component)
  (bind (((:read-only-slots single range range-start range-end) component))
    (if single
        (bind ((partial-timestamp-string (princ-to-string (component-value-of range))))
          (values (prc::first-moment-for-partial-timestamp partial-timestamp-string)
                  (prc::last-moment-for-partial-timestamp partial-timestamp-string)))
        (not-yet-implemented))))

;;;;;;
;;; Coordinate provider

(def component coordinates-provider (content-component)
  ((dimensions)
   (coordinates)))

(def (macro e) coordinates-provider (dimensions coordinates &body content)
  `(make-instance 'coordinate-provider
                  :dimensions (mapcar #'prc:lookup-dimension ,dimensions)
                  :coordinates ,coordinates
                  :content (progn ,@content)))

(def call-in-component-environment coordinates-provider ()
  (bind (((:read-only-slots dimensions coordinates) -self-))
    (prc:with-coordinates dimensions coordinates
      (call-next-method))))
