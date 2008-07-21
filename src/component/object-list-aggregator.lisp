;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object list aggregator

(def component standard-object-list-aggregator-component (abstract-standard-object-list-component abstract-standard-class-component)
  ((slot-values nil :type components)))

(def method (setf component-value-of) :after (new-value (self standard-object-list-aggregator-component))
  (with-slots (slot-values instances the-class) self
    (setf slot-values
          (iter (for slot :in (standard-object-aggregated-slots the-class))
                (for slot-value = (find slot slot-values :key #'component-value-of))
                (if slot-value
                    (setf (component-value-of slot-value) instances)
                    (setf slot-value (make-instance 'standard-object-list-slot-value-aggregator-component :instances instances :slot slot)))
                (collect slot-value)))))

(def render standard-object-list-aggregator-component ()
  (bind (((:read-only-slots slot-values instances) -self-))
    <div "Darab: " ,(length instances)
         <table
             <thead
              <th "Név">
              <th "Minimum">
              <th "Maximum">
              <th "Átlag">
              <th "Összesen">>
           <tbody
            ,@(mapcar (lambda (slot-value)
                        <tr (:class ,(odd/even-class slot-value slot-values))
                            ,@(render slot-value)>)
                      slot-values)>>>))

(def generic standard-object-aggregated-slots (class)
  (:method ((class standard-class))
    (filter-if (lambda (slot)
                 (subtypep (slot-definition-type slot) 'number))
               (class-slots class))))

;;;;;;
;;; Standard object list slot value aggregator

(def component standard-object-list-slot-value-aggregator-component (abstract-standard-object-list-component abstract-standard-object-slot-value-component)
  ((minimum)
   (maximum)
   (average)
   (sum)))

(def method (setf component-value-of) :after (new-value (self standard-object-list-slot-value-aggregator-component))
  (bind (((:read-only-slots instances slot) self))
    (setf instances (mapcar #'reuse-standard-object-instance instances))
    (iter (with slot-name = (slot-definition-name slot))
          (for instance :in instances)
          (for value = (slot-value instance slot-name))
          (minimize value :into minimum)
          (maximize value :into maximum)
          (sum value :into sum)
          (finally
           (setf (minimum-of self) minimum)
           (setf (maximum-of self) maximum)
           (setf (sum-of self) sum)
           (setf (average-of self) (/ sum (length instances)))))))

(def render standard-object-list-slot-value-aggregator-component ()
  (bind (((:read-only-slots minimum maximum average sum slot) -self-))
    (list <td ,(localized-slot-name slot)>
          <td ,minimum>
          <td ,maximum>
          <td ,(coerce average 'float)>
          <td ,sum>)))
