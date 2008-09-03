;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Kludges

;; TODO: KLUDGE: this redefines, but we are practical for now
(def function find-main-type-in-or-type (type)
  (remove-if (lambda (element)
               (member element '(or cl-perec:unbound null)))
             type))

;; TODO: KLUDGE: this redefines, but we are practical for now
(def function find-type-by-name (name)
  (or (find-class name #f)
      (cl-perec:find-type name)))

;; KLUDGE: TODO: redefined for now
(def function update-component-value-from-place (place component)
  (when (place-bound-p place)
    (bind ((value (value-at-place place)))
      (setf (component-value-of component)
            (if (prc::values-having-validity-p value)
                (prc::single-values-having-validity-value value)
                value)))))

;; KLUDGE: TODO: redefined for now
(def layered-function render-icon-label (icon label)
  (:method (icon label)
    ;; TODO: use a flag in the component to mark the label as important (so cannot be hidden) and forget about typep here
    ;; TODO: factor this out into a preferences accessor function
    (when (and (dmm::has-authenticated-session)
               (typep (parent-component-of (parent-component-of icon)) 'command-bar-component))
      (bind ((effective-subject (dmm::current-effective-subject))
             (subject-preferences (when effective-subject
                                    (dmm::subject-preferences-of effective-subject))))
        (when (and subject-preferences
                   ;; TODO use some preferences framework that can fall back to defaults unless overridden
                   ;; KLUDGE dmm dependency
                   (slot-boundp subject-preferences 'dmm:display-command-labels)
                   (not (dmm:display-command-labels? subject-preferences)))
          (return-from render-icon-label nil))))
    label))

;; KLUDGE: TODO: redefined for now
(def (function e) save-editing (editable &key (leave-editing #t))
  (assert (typep editable 'editable-component))
  ;; TODO: make this with-transaction part of a generic save-editing protocol dispatch
  (rdbms::with-transaction
    (store-editing editable)
    (when leave-editing
      (leave-editing editable))
    (rdbms:register-transaction-hook :after :commit
      (add-user-information editable "A változtatások elmentése sikerült"))))

;; KLUDGE: TODO: redefined for now
(def method make-place-component-command-bar ((self standard-object-place-maker))
  (bind ((type (the-type-of self)))
    (make-instance 'command-bar-component :commands (optional-list (when (prc::null-subtype-p type)
                                                                     (make-set-place-to-nil-command self))
                                                                   (when (or (initform-of self)
                                                                             (prc::unbound-subtype-p type))
                                                                     (make-set-place-to-unbound-command self))
                                                                   (make-set-place-to-find-instance-command self)
                                                                   (make-set-place-to-new-instance-command self)))))
