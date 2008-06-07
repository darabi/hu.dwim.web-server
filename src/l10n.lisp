;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(defmethod localize ((class class))
  (lookup-resource (concatenate-string "class-name." (string-downcase (class-name class))) nil))

(def (generic e) localized-slot-name (slot &key capitalize-first-letter prefix-with)

  (:method ((slot effective-slot-definition) &key &allow-other-keys)
    (bind ((slot-name (slot-definition-name slot)))
      (lookup-first-matching-resource
        ((class-name (owner-class-of-effective-slot-definition slot)) slot-name)
        ("slot-name" slot-name))))

  (:method :around ((slot effective-slot-definition) &key (capitalize-first-letter #t) prefix-with)
    (bind (((:values str found?) (call-next-method)))
      (when capitalize-first-letter
        (setf str (capitalize-first-letter str)))
      (values (if prefix-with
                  (concatenate-string prefix-with str)
                  str)
              found?))))

(defun localized-slot-name<> (slot &rest args)
  (bind (((:values str found) (apply #'localized-slot-name slot args)))
    <span (:class ,(concatenate-string "label " (unless found
                                                  +missing-resource-css-class+)
                            ;; TODO this is fragile here, should use a public api in dmm
                            ;; something like associationp
                            ;;(when (typep slot 'dmm:effective-association-end)
                            ;;  "association")
                            ))
          ,str>))
