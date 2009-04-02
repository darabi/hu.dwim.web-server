;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (definer e) resource-loading-locale-loaded-listener (name asdf-system base-directory &key log-discriminator (setup-readtable-function ''setup-readtable))
  (setf log-discriminator (or log-discriminator ""))
  (with-standard-definer-options name
    (once-only (setup-readtable-function asdf-system base-directory log-discriminator)
      `(def function ,name (locale-name)
         (l10n.debug "Loading ~A resources for locale ~S" ,log-discriminator locale-name)
         (bind ((file (merge-pathnames (concatenate-string locale-name ".lisp") (system-relative-pathname ,asdf-system ,base-directory))))
           (when (probe-file file)
             (bind ((*readtable* (copy-readtable *readtable*)))
               (awhen ,setup-readtable-function
                 (funcall it))
               (cl-l10n::load-resource-file :wui file)
               (l10n.info "Loaded ~A resources for locale ~S from ~A" ,log-discriminator locale-name file))))))))

(def resource-loading-locale-loaded-listener wui-resource-loader :wui "resources/"
  :log-discriminator "WUI")
(register-locale-loaded-listener 'wui-resource-loader)

(def resource-loading-locale-loaded-listener wui-resource-loader/application :wui "resources/application/"
  :log-discriminator "WUI")
(register-locale-loaded-listener 'wui-resource-loader/application)

(def resource-loading-locale-loaded-listener wui-resource-loader/component :wui "resources/component/"
  :log-discriminator "WUI")
(register-locale-loaded-listener 'wui-resource-loader/component)

;;;;;;
;;; localization primitives

(def (function e) localized-mime-type-description (mime-type)
  (lookup-first-matching-resource
    ("mime-type" mime-type)))

(def (function e) localized-mime-type-description<> (mime-type)
  (bind (((:values str found) (localized-mime-type-description mime-type)))
    <span (:class ,@(concatenate-string "slot-name "
                                        (unless found
                                          +missing-resource-css-class+)))
          ,str>))

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
      (assert str)
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
      (setf class-name (if plural
                           (with-definite-article class-name)
                           (with-indefinite-article class-name))))
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
  (bind ((member-name (member-component-value-name member-value))
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
                                                  (:hour 2) #\: (:min 2) #\: (:sec 2)))

(def (function e) localized-timestamp (timestamp &key verbosity (relative-mode #f) display-day-of-week)
  (declare (ignore verbosity relative-mode display-day-of-week))
  ;; TODO many
  (local-time:format-timestring nil timestamp :format +timestamp-format+)
  ;; TODO (cl-l10n:format-timestamp nil timestamp)
  )

#+nil ;; TODO port this old localized-timestamp that supports relative mode
(defun localized-timestamp (local-time &key
                           (relative-mode nil)
                           (display-day-of-week nil display-day-of-week-provided?))
  "DWIMish timestamp printer. RELATIVE-MODE requests printing human friendly dates based on (NOW)."
  (setf local-time (local-time:adjust-local-time local-time (set :timezone ucw:*client-timezone*)))
  (bind ((*print-pretty* nil)
         (*print-circle* nil)
         (cl-l10n::*time-zone* nil)
         (now (local-time:now)))
    (with-output-to-string (stream)
      (with-decoded-local-time (:year year1 :month month1 :day day1 :day-of-week day-of-week1) now
        (with-decoded-local-time (:year year2 :month month2 :day day2) local-time
          (bind ((year-distance (abs (- year1 year2)))
                 (month-distance (abs (- month1 month2)))
                 (day-distance (- day2 day1))
                 (day-of-week-already-encoded? nil)
                 (day-of-week-should-be-encoded? (or display-day-of-week
                                                     (and relative-mode
                                                          (zerop year-distance)
                                                          (zerop month-distance)
                                                          (< (abs day-distance) 7))))
                 (format-string/date (if (and relative-mode
                                              (zerop year-distance))
                                         (if (zerop month-distance)
                                             (cond
                                               ((<= -1 day-distance 1)
                                                (setf day-of-week-already-encoded? t)
                                                (setf day-of-week-should-be-encoded? nil)
                                                (case day-distance
                                                  (0  #"today")
                                                  (-1 #"yesterday")
                                                  (1  #"tomorrow")))
                                               (t
                                                ;; if we are within the current week
                                                ;; then print only the name of the day.
                                                (if (and (< (abs day-distance) 7)
                                                         (<= 1 ; but sunday can be a potential source of confusion
                                                             (+ day-of-week1
                                                                day-distance)
                                                             6))
                                                    (progn
                                                      (setf day-of-week-already-encoded? t)
                                                      "%A")
                                                    (progn
                                                      (setf day-of-week-should-be-encoded? nil)
                                                      "%b. %e."))))
                                             "%b. %e.")
                                         "%Y. %b. %e."))
                 (format-string (concatenate 'string
                                             format-string/date
                                             (if (or day-of-week-already-encoded?
                                                     (not day-of-week-should-be-encoded?)
                                                     (and display-day-of-week-provided?
                                                          (not display-day-of-week)))
                                                 ""
                                                 " %A")
                                             " %H:%M:%S")))
            (cl-l10n::print-time-string format-string stream
                                        (local-time:universal-time
                                         (local-time:adjust-local-time! local-time
                                           (set :timezone local-time:*default-timezone*)))
                                        (current-locale))))))))

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

(def (generic e) localized-instance-reference-string (instance)
  (:method ((instance standard-object))
    (princ-to-string instance)))

(def function apply-resource-function (name &optional args)
  (flet ((fallback-locale-for-functional-resources ()
           (acond
             ((and (boundp '*application*)
                   *application*)
              (default-locale-of it))
             (*fallback-locale-for-functional-resources*
              (locale it))
             (t (error "Could not identify a fallback locale for functional resources.")))))
    (lookup-resource name :arguments args
                     :otherwise (lambda ()
                                  (with-locale (fallback-locale-for-functional-resources)
                                    (lookup-resource name :arguments args
                                                     :otherwise [error "Functional resource ~S is missing even for the fallback locale ~A" name (current-locale)]))))))

(setf *fallback-locale-for-functional-resources* "en")
