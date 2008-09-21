;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Timestamp range

;; TODO: implement
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
                   :client-name-generator #'integer-to-string)
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
