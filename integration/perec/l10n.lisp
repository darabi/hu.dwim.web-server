;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def localization-loading-locale-loaded-listener wui-resource-loader/hu.dwim.perec :wui "localization/integration/hu.dwim.perec/"
  :log-discriminator "WUI")
(register-locale-loaded-listener 'wui-resource-loader/hu.dwim.perec)

(def method localized-instance-name ((instance hu.dwim.perec::persistent-object))
  (bind ((class (class-of instance)))
    (if (typep class 'hu.dwim.meta-model::entity)
        (flet ((localize-value (value)
                 (bind ((d-value? (hu.dwim.perec::d-value-p value)))
                   (if (and (not d-value?)
                            (typep value 'standard-object))
                       (localized-instance-name value)
                       (princ-to-string (if (and d-value?
                                                 (hu.dwim.perec::single-d-value-p value))
                                            (hu.dwim.perec::single-d-value value)
                                            value))))))
          (bind ((properties (hu.dwim.meta-model::reference-properties-of class)))
            (if (length= 1 properties)
                (localize-value (slot-value-using-class class instance (first properties)))
                (with-output-to-string (result)
                  (macrolet ((emit (string)
                               `(write-string ,string result)))
                    (emit (localized-class-name class :with-article #t :capitalize-first-letter #t))
                    (when properties
                      (emit ": "))
                    (iter (for property :in properties)
                          (for value = (slot-value-using-class class instance property))
                          (unless (first-iteration-p)
                            (emit ", "))
                          (emit (localized-slot-name property))
                          (emit " = ")
                          (emit (localize-value value))))))))
        (call-next-method))))
