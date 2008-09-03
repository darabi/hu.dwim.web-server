;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Timestamp range

(def component timestamp-range-component (editable-component)
  ((lower-bound
    nil
    :type (or null local-time:timestamp))
   (upper-bound
    nil
    :type (or null local-time:timestamp))
   (single
    #t
    :type boolean)
   (unit
    :year
    :type (member :year :month :weak :day :hour :minute :second))
   (range
    (make-instance 'member-component
                   ;; TODO: kill this hack
                   :possible-values '(2007 2008 2009)
                   :component-value 2008
                   :client-name-generator #'princ-to-string)
    :type component)
   (range-start
    (make-instance 'timestamp-component)
    :type component)
   (range-end
    (make-instance 'timestamp-component)
    :type component)))

(def (macro e) timestamp-range (&key range-start range-end)
  `(make-instance 'timestamp-range-component
                  :range-start (make-instance 'timestamp-component :component-value ,range-start)
                  :range-end (make-instance 'timestamp-component :component-value ,range-end)))

(def render timestamp-range-component ()
  (bind (((:read-only-slots single range range-start range-end) -self-))
    (if single
        (render range)
        (render-horizontal-list (list range-start range-end)))))

(def function compute-timestamp-range (component)
  (bind (((:read-only-slots single range range-start range-end) component))
    (if single
        (bind ((partial-timestamp-string (princ-to-string (component-value-of range))))
          (values (prc::first-moment-for-partial-timestamp partial-timestamp-string)
                  (prc::last-moment-for-partial-timestamp partial-timestamp-string)))
        (not-yet-implemented))))

;;;;;;
;;; Temporal provider

(def component temporal-provider-component (content-component)
  ((t-value :type prc::timestamp)))

(def call-in-component-environment temporal-provider-component ()
  (prc::call-with-t (t-value-of -self-) #'call-next-method
    (call-next-method)))

;;;;;;
;;; Temporal selector

(def component temporal-selector-component (content-component)
  ((t-value :type component)))

(def render temporal-selector-component ()
  (bind (((:read-only-slots t-value) -self-))
    <div ,(render t-value)
         ,(call-next-method)>))

;;;;;;
;;; Validity provider

(def component validity-provider-component (content-component)
  ((validity-start :type prc::timestamp)
   (validity-end :type prc::timestamp)))

(def call-in-component-environment validity-provider-component ()
  (prc::call-with-validity-range (validity-start-of -self-) (validity-end-of -self-) #'call-next-method))

;;;;;;
;;; Validity selector

(def component validity-selector-component (content-component)
  ((range :type component)))

(def constructor validity-selector-component ()
  (begin-editing (range-of -self-)))

(def (macro e) validity-selector ((&key validity validity-start validity-end) &body forms)
  `(make-instance 'validity-selector-component
                  :content (progn ,@forms)
                  :range (timestamp-range :range-start ,(if validity
                                                            (prc::first-moment-for-partial-timestamp validity)
                                                            validity-start)
                                          :range-end ,(if validity
                                                          (prc::last-moment-for-partial-timestamp validity)
                                                          validity-end))))

(def render validity-selector-component ()
  (bind (((:read-only-slots range) -self-))
    <div ,(render range)
         ,(call-next-method)>))

(def call-in-component-environment validity-selector-component ()
  (bind (((:values validity-start validity-end) (compute-timestamp-range (range-of -self-))))
    (prc::call-with-validity-range validity-start validity-end #'call-next-method)))
