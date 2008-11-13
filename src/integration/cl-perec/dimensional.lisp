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

(def component validity-selector (timestamp-range-component)
  ((edited #t)))

(def (macro e) validity-selector (&key validity validity-begin validity-end)
  `(make-instance 'validity-selector
                  :range-start ,(if validity
                                    (prc::first-moment-for-partial-timestamp validity)
                                    validity-begin)
                  :range-end ,(if validity
                                  (prc::last-moment-for-partial-timestamp validity)
                                  validity-end)))

(def function compute-timestamp-range (component)
  (bind (((:read-only-slots single) component))
    (if single
        (bind ((partial-timestamp-string (princ-to-string (component-value-of (range-of component)))))
          (list (prc::first-moment-for-partial-timestamp partial-timestamp-string)
                (prc::last-moment-for-partial-timestamp partial-timestamp-string)))
        (not-yet-implemented))))

;;;;;;
;;; Validity selector and provider

(def component validity-selector-and-provider (content-component validity-selector)
  ())

(def (macro e) validity-selector-and-provider ((&key validity validity-begin validity-end) &body forms)
  `(make-instance 'validity-selector-and-provider
                  :content (progn ,@forms)
                  :range-start ,(if validity
                                    (prc::first-moment-for-partial-timestamp validity)
                                    validity-begin)
                  :range-end ,(if validity
                                  (prc::last-moment-for-partial-timestamp validity)
                                  validity-end)))

;;;;;;
;;; Coordinate provider

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
    (prc:with-coordinates dimensions coordinates
      (call-next-method))))
