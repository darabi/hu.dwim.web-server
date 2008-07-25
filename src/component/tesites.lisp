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
    :type (member :year :quarter-year :month :weak :day :hour :minute :second))
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
;;; Temporal selector

(def component temporal-selector-component ()
  ((t-value)))

(def call-in-rendering-environment temporal-selector-component ()
  (rdbms:with-transaction* (:default-terminal-action :rollback :t (t-value-of -self-))
    (call-next-method)))

;;;;;;
;;; Validity selector

(def component validity-selector-component (content-component)
  ((range :type component)
   (refresh-command :type component)))

(def constructor validity-selector-component ()
  (setf (refresh-command-of -self-) (make-validity-selector-refresh-command -self-))
  (begin-editing (range-of -self-)))

(def (macro e) validity-selector ((&key validity-start validity-end) &body forms)
  `(make-instance 'validity-selector-component
                  :content (progn ,@forms)
                  :range (timestamp-range :range-start ,validity-start :range-end ,validity-end)))

(def function call-with-selected-validity-range (component thunk)
  
  (bind (((:values validity-start validity-end) (compute-timestamp-range (range-of component))))
    (prc::call-with-validity-range validity-start validity-end thunk)))

(def call-in-rendering-environment validity-selector-component ()
  (call-with-selected-validity-range -self- #'call-next-method))

(def render validity-selector-component ()
  (bind (((:read-only-slots range refresh-command) -self-))
    <div ,(render range)
         ,(render refresh-command)
         ,(call-next-method)>))

(def method refresh-component ((self validity-selector-component))
  (call-with-selected-validity-range self #'call-next-method))

(def function make-validity-selector-refresh-command (component)
  (command (icon refresh)
           (make-action
             (refresh-component component))))
