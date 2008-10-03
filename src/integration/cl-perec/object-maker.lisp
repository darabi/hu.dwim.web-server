;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Customizations

(def layered-method make-standard-object-detail-maker-class ((component standard-object-detail-maker) (class prc::persistent-class) (prototype prc::persistent-object))
  (if (dmm::developer-p (dmm::current-effective-subject))
      (make-viewer class :default-component-type 'reference-component)
      (call-next-method)))

(def layered-method collect-standard-object-detail-maker-slots ((component standard-object-detail-maker) (class prc::persistent-class) (prototype prc::persistent-object))
  (bind ((excluded-slot-name
          (awhen (find-ancestor-component-with-type component 'standard-object-slot-value-component)
            (bind ((slot (slot-of it)))
              (when (typep slot 'prc::persistent-association-end-effective-slot-definition)
                (slot-definition-name (prc::other-association-end-of slot)))))))
    (remove-if (lambda (slot)
                 (or (prc:persistent-object-internal-slot-p slot)
                     (eq (slot-definition-name slot) excluded-slot-name)))
               (call-next-method))))

(def layered-method collect-standard-object-detail-maker-slots ((component standard-object-detail-maker) (class dmm::entity) (prototype prc::persistent-object))
  (filter-if (lambda (slot)
               (dmm::authorize-operation 'dmm::create-entity-property-operation :-entity- class :-property- slot))
             (call-next-method)))

(def layered-method execute-create-instance ((component standard-object-maker) (class prc::persistent-class))
  (rdbms::with-transaction
    (prog1-bind instance (call-next-method)
      (rdbms:register-transaction-hook :after :commit
        (add-user-message component "Az új ~A létrehozása sikerült" (list (localized-class-name (the-class-of component)))
                          :category :information
                          :permanent #t
                          :content (make-viewer instance :default-component-type 'reference-component))))))
