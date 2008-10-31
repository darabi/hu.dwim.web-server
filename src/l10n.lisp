;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; localization primitives

(def method localize ((class class))
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
                                               (with-definite-article class-name)
                                               (with-indefinite-article class-name))
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
      (bind ((article (with-indefinite-article class-name)))
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
  (bind ((member-name (typecase member-value
                        (symbol (symbol-name member-value))
                        (class (class-name member-value))
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
    (when (and (not found?)
               (typep member-value 'class))
      (setf (values str found?) (localized-class-name member-value)))
    (when capitalize-first-letter
      (setf str (capitalize-first-letter str)))
    (values str found?)))

(def function localized-enumeration-member<> (member-value &key class slot capitalize-first-letter)
  (bind (((:values str found?) (localized-enumeration-member member-value :class class :slot slot
                                                             :capitalize-first-letter capitalize-first-letter)))
    <span (:class ,(unless found? +missing-resource-css-class+))
      ,str>))

(def (constant :test 'equal) +timestamp-format+ '((:year 4) #\- (:month 2) #\- (:day 2) #\ 
                                                  (:hour 2) #\: (:min 2) #\: (:sec 2) #\.
                                                  (:usec 6)))

(def (function e) localized-timestamp (timestamp)
  ;; TODO many
  (local-time:format-timestring nil timestamp :format +timestamp-format+))


;;;;;;
;;; utilities

#+nil
(def (function e) render-client-timezone-offset-probe ()
  "Renders an input field with a callback that will set the CLIENT-TIMEZONE slot of the session when the form is submitted."
  (with-unique-js-names (id)
    <input (:id ,id
            :type "hidden"
            :name (client-state-sink (value)
                    (bind ((offset (parse-integer value :junk-allowed #t)))
                      (if offset
                          (bind ((local-time (parse-rfc3339-timestring value)))
                            (ucw.l10n.debug "Setting client timezone from ~A" local-time)
                            (setf (client-timezone-of (context.session *context*)) (timezone-of local-time)))
                          (progn
                            (app.warn "Unable to parse the client timezone offset: ~S" value)
                            (setf (client-timezone-of (context.session *context*)) +utc-zone+)))))) >
    (ucw:script `(on-load
                   (setf (slot-value ($ ,id) 'value)
                         (dojo.date.to-rfc-3339 (new *date)))))))

;; TODO rename to short-textual-representation-for or anything else without "name" in it
(def generic localized-instance-name (instance)
  (:method ((instance standard-object))
    (princ-to-string instance)))
