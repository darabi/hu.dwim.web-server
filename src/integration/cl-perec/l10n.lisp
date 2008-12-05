;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def method localized-instance-name ((instance prc::persistent-object))
  (bind ((class (class-of instance)))
    (if (typep class 'dmm::entity)
        (flet ((localize-value (value)
                 (bind ((d-value? (prc::d-value-p value)))
                   (if (and (not d-value?)
                            (typep value 'standard-object))
                       (localized-instance-name value)
                       (princ-to-string (if (and d-value?
                                                 (prc::single-d-value-p value))
                                            (prc::single-d-value value)
                                            value))))))
          (bind ((properties (dmm::reference-properties-of class)))
            (if (length= 1 properties)
                (localize-value (slot-value-using-class class instance (first properties)))
                (apply #'concatenate-string
                       (localized-class-name class :with-article #t :capitalize-first-letter #t) (when properties ": ")
                       (iter (for property :in properties)
                             (for value = (slot-value-using-class class instance property))
                             (collect (concatenate-string (unless (first-iteration-p) ", ")
                                                          (localized-slot-name property) " = " (localize-value value))))))))
        (call-next-method))))
