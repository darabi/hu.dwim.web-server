;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(defmethod localize ((class class))
  (lookup-first-matching-resource
    ("class-name" (string-downcase (qualified-symbol-name (class-name class))))
    ("class-name" (string-downcase (class-name class)))))

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

(def function localized-slot-name<> (slot &rest args)
  (bind (((:values str found) (apply #'localized-slot-name slot args)))
    <span (:class ,(concatenate-string "slot-name "
                                       (unless found
                                         +missing-resource-css-class+)
                            ;; TODO this is fragile here, should use a public api in dmm
                            ;; something like associationp
                            ;;(when (typep slot 'dmm:effective-association-end)
                            ;;  "association")
                            ))
          ,str>))

(def function localized-class-name (class &key capitalize-first-letter with-article plural)
  (assert (typep class 'class))
  (bind (((:values class-name found?) (localize class)))
    (when plural
      (setf class-name (plural-of class-name)))
    (when with-article
      (setf class-name (concatenate-string (if plural
                                               (definite-article-for class-name)
                                               (indefinite-article-for class-name))
                                           " "
                                           class-name)))
    (values (if capitalize-first-letter
                (capitalize-first-letter class-name)
                class-name)
            found?)))

(def function localized-class-name<> (class &key capitalize-first-letter with-indefinite-article)
  (assert (typep class 'class))
  (bind (((:values class-name found?) (localize class)))
    (when with-indefinite-article
      (bind ((article (indefinite-article-for class-name)))
        (write (if capitalize-first-letter
                   (capitalize-first-letter article)
                   article)
               :stream *html-stream*)
        (write-char #\Space *html-stream*)))
    <span (:class ,(concatenate-string "class-name "
                                       (unless found?
                                         +missing-resource-css-class+)))
          ,(if (and capitalize-first-letter
                    (not with-indefinite-article))
               (capitalize-first-letter class-name)
               class-name)>))

(def function localized-boolean-value (value)
  (bind (((:values str found?) (localize (if value "boolean-value.true" "boolean-value.false"))))
    (values str found?)))

(def function localized-boolean-value<> (value)
  (bind (((:values str found?) (localized-boolean-value value)))
    <span (:class ,(append
                    (unless found?
                      +missing-resource-css-class+)
                    (list "boolean")))
      ,str>))

(def function localized-enumeration-member (member-value &key class slot capitalize-first-letter)
  ;; TODO fails with 'person 'sex: '(OR NULL SEX-TYPE)
  (unless class
    (setf class (when slot
                  (owner-class-of-effective-slot-definition slot))))
  (bind ((member-name (cond ((symbolp member-value)
                             (symbol-name member-value))
                            ;; TODO ? ((typep member-value 'dmm:named-element)
                            ;; (element-name-of member-value))
                            (t (write-to-string member-value))))
         (slot-definition-type (when slot
                                 (slot-definition-type slot)))
         (class-name (when class
                       (class-name class)))
         (slot-name (when slot
                      (symbol-name (slot-definition-name slot))))
         ((:values str found?)
          (lookup-first-matching-resource
            (when (and class-name
                       slot-name)
              class-name slot-name member-name)
            (when slot-name
              slot-name member-name)
            (when (and slot-definition-type
                       ;; TODO strip off (or null ...) from the type
                       (symbolp slot-definition-type))
              slot-definition-type member-name)
            ("member-type-value" member-name))))
    (when capitalize-first-letter
      (setf str (capitalize-first-letter str)))
    (values str found?)))

(def function localized-enumeration-member<> (member-value &key class slot capitalize-first-letter)
  (bind (((:values str found?) (localized-enumeration-member member-value :class class :slot slot
                                                             :capitalize-first-letter capitalize-first-letter)))
    <span (:class ,(unless found? +missing-resource-css-class+))
      ,str>))
