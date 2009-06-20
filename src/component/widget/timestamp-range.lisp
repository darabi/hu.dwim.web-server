;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Timestamp range

;; TODO: implement
(def (component e) timestamp-range-component (editable/mixin)
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
    :type component)
   (range-begin
    (make-instance 'timestamp-component)
    :type component)
   (range-end
    (make-instance 'timestamp-component)
    :type component)))

(def constructor timestamp-range-component ()
  (not-yet-implemented)
  (setf (range-of -self-) (make-instance 'member-inspector
                                         :edited #t
                                         ;; KLUDGE: TODO: kill this hack
                                         :possible-values '(2007 2008 2009)
                                         ;; KLUDGE: what a kludge here
                                         :component-value (local-time:timestamp-year (range-begin-of -self-))
                                         :client-name-generator [integer-to-string !2])))

(def (macro e) timestamp-range (&key range-begin range-end)
  `(make-instance 'timestamp-range-component
                  :range-begin (make-instance 'timestamp-component :component-value ,range-begin)
                  :range-end (make-instance 'timestamp-component :component-value ,range-end)))

(def render-xhtml timestamp-range-component
  (bind (((:read-only-slots single range range-begin range-end) -self-))
    (if single
        (render-component range)
        (render-horizontal-list-layout (list range-begin range-end)))))
