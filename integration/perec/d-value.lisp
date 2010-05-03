;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; d-value/inspector

(def (component e) d-value/inspector (t/inspector)
  ()
  (:documentation "Inspector for a D-VALUE instance in various alternative views."))

(def subtype-mapper *inspector-type-mapping* (or null hu.dwim.perec::d-value) d-value/inspector)

(def method slot-type (class prototype (slot hu.dwim.perec::persistent-effective-slot-definition-d))
  'hu.dwim.perec::d-value)

(def layered-method make-alternatives ((component d-value/inspector) class prototype value)
  (list* (make-instance 'd-value/table/inspector
                        :component-value value
                        :component-value-type (component-value-type-of component))
         (call-next-layered-method)))

;;;;;;
;;; t/reference/inspector

(def layered-method make-reference-content ((reference t/reference/inspector) class prototype (instance hu.dwim.perec::d-value))
  (if (hu.dwim.perec::single-d-value-p instance)
      (bind ((single-value (hu.dwim.perec::single-d-value instance)))
        (make-reference-content reference (class-of single-value) single-value single-value))
      (string+ (write-to-string (length (hu.dwim.perec::c-values-of instance))) " dimensional values")))

;;;;;;
;;; d-value/table/inspector

(def component d-value/table/inspector (inspector/style
                                        t/detail/inspector
                                        table/widget
                                        component-messages/widget)
  ())

(def refresh-component d-value/table/inspector
  (bind (((:slots component-value rows columns) -self-)
         (dimensions (hu.dwim.perec::dimensions-of component-value)))
    (setf columns (cons (make-instance 'place/column/inspector
                                       :component-value "BLAH" ;; TODO:
                                       :header "Value"
                                       :cell-factory (lambda (row)
                                                       (make-value-inspector (hu.dwim.perec::value-of (component-value-of row)))))
                        (iter (for index :from 0)
                              (for dimension :in dimensions)
                              (rebind (index)
                                (collect (make-instance 'place/column/inspector
                                                        :component-value "BLAH" ;; TODO:
                                                        :header (localized-dimension-name dimension)
                                                        :cell-factory (lambda (row)
                                                                        (make-coordinate-inspector (elt dimensions index)
                                                                                                   (elt (hu.dwim.perec::coordinates-of (component-value-of row)) index)))))))))
    (setf rows (iter (for c-value :in (hu.dwim.perec::c-values-of component-value))
                     (collect (make-instance 't/row/inspector :component-value c-value))))))

;;;;;;
;;; Util

(def function localized-dimension-name (dimension &key capitalize-first-letter)
  (bind ((name (string-downcase (hu.dwim.perec::name-of dimension)))
         (localized-name (lookup-first-matching-resource
                           ("dimension-name" name))))
    (if capitalize-first-letter
        (capitalize-first-letter localized-name)
        localized-name)))

(def function make-coordinate-inspector (dimension coordinate)
  (if (typep dimension 'hu.dwim.perec::ordering-dimension)
      (make-coordinate-range-inspector coordinate)
      (make-value-viewer (if (length= 1 coordinate)
                             (first coordinate)
                             coordinate)
                         :default-alternative-type 'reference-component)))

(def function make-coordinate-range-inspector (coordinate)
  ;; TODO: KLUDGE: this is really much more complex than this
  (bind ((begin (hu.dwim.perec::coordinate-range-begin coordinate))
         (end (hu.dwim.perec::coordinate-range-end coordinate)))
    (if (local-time:timestamp= begin end)
        ;; single moment of time
        (localized-timestamp begin)
        (local-time:with-decoded-timestamp (:day day-begin :month month-begin :year year-begin :timezone local-time:+utc-zone+) begin
          (local-time:with-decoded-timestamp (:day day-end :month month-end :year year-end :timezone local-time:+utc-zone+) end
            (cond
              ;; whole year
              ((and (= 1 day-begin)
                    (= 1 month-begin)
                    (= 1 day-end)
                    (= 1 month-end)
                    (= 1 (- year-end year-begin)))
               (integer-to-string year-begin))
              ;; TODO: range of years
              ;; whole month
              ((and (= 1 day-begin)
                    (= 1 day-end)
                    (= year-end year-begin)
                    (= 1 (- month-end month-begin)))
               (localize-month-name (1- month-begin)))
              ;; range of months
              ((and (= 1 day-begin)
                    (= 1 day-end)
                    (or (= year-end year-begin)
                        (and (= 1 (- year-end year-begin))
                             (= 1 month-end))))
               (string+ (localize-month-name (1- month-begin)) " - " (localize-month-name (mod (- month-end 2) 12))))
              ;; TODO: whole day
              ;; TODO: range of days
              (t
               (string+ (localized-timestamp begin) " - " (localized-timestamp end)))))))))
