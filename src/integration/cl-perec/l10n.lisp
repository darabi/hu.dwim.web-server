;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def resource-loading-locale-loaded-listener wui-resource-loader/cl-perec (project-relative-pathname "resources/integration/cl-perec/")
  :log-discriminator "WUI")
(register-locale-loaded-listener 'wui-resource-loader/cl-perec)

(def method localized-instance-reference-string ((instance prc::persistent-object))
  (bind ((class (class-of instance)))
    (if (typep class 'dmm::entity)
        (flet ((localize-value (value)
                 (bind ((d-value? (prc::d-value-p value)))
                   (if (and (not d-value?)
                            (typep value 'standard-object))
                       (localized-instance-reference-string value)
                       (princ-to-string (if (and d-value?
                                                 (prc::single-d-value-p value))
                                            (prc::single-d-value value)
                                            value))))))
          (bind ((properties (dmm::reference-properties-of class)))
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
