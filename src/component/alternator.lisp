;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;; TODO: review this

;;;;;;
;;; Alternator

(def component alternator-component (editable-component)
  ((alternatives nil)
   (content nil :type component)
   (command-bar nil :type component)))

(def render alternator-component ()
  (with-slots (content command-bar) self
    (if (typep content 'reference-component)
        (render content)
        (render-vertical-list (list content command-bar)))))

(def function make-alternator-command-bar-component (component alternatives)
  (make-instance 'command-bar-component :commands (append (list (make-top-command-component component)
                                                                (make-refresh-command-component component)
                                                                (make-begin-editing-command-component component)
                                                                (make-save-editing-command-component component)
                                                                (make-cancel-editing-command-component component))
                                                          (mapcar (lambda (alternative)
                                                                    (make-alternative-component-replace-command-component component alternative))
                                                                  alternatives))))

(def function make-alternative-component-replace-command-component (component alternative)
  (make-replace-command-component (delay (content-of component)) alternative
                                  :visible (delay (and (not (has-edited-descendant-component-p (content-of component)))
                                                       (not (eq (force alternative) (content-of component)))))
                                  :icon (make-alternative-component-replace-command-icon-component (class-prototype (the-class-of alternative)))))

(def generic make-alternative-component-replace-command-icon-component (prototype)
  (:method ((prototype component))
    (make-icon-component nil :label (string-capitalize (substitute #\Space #\- (trim-suffix "-component" (string-downcase (class-name (class-of prototype))))))))

  (:method ((prototype reference-component))
    (make-icon-component 'collapse :label "Collapse" :tooltip "Collapse to reference")))

(def method join-editing ((alternator alternator-component))
  (unless (typep (content-of alternator) 'reference-component)
    (call-next-method)))

(def class* component-factory ()
  ((the-class)
   (component nil))
  (:metaclass funcallable-standard-class))

(def function find-alternative-component (alternatives type)
  (some (lambda (class)
          (awhen (find-if (lambda (alternative)
                            (subtypep (the-class-of alternative) class))
                          alternatives)
            (force it)))
        (bind ((class (find-class type)))
          (ensure-finalized class)
          (class-precedence-list class))))

(def function find-default-alternative-component (alternatives)
  (find-alternative-component alternatives 'detail-component))

(def macro delay-alternative-component (type &body forms)
  `(aprog1 (make-instance 'component-factory :the-class (find-class ,type))
     (set-funcallable-instance-function it (delay (or (component-of it)
                                                      (setf (component-of it) (progn ,@forms)))))))

(def macro delay-alternative-component-type (type &rest args)
  `(delay-alternative-component ,type (make-instance ,type ,@args)))

(def function setf-expand-reference-to-default-alternative-command-component (reference)
  (setf (expand-command-of reference)
        (make-expand-reference-command-component reference (delay (find-default-alternative-component (alternatives-of (parent-component-of reference))))))
  reference)

#+nil
(def (generic e) make-alternative-components (thing)
  (:method (thing)
    (list (delay-alternative-component-type 't-component :value thing)))

  (:method ((thing null))
    (list (delay-alternative-component-type 'nil-component)))

  (:method ((place slot-value-place))
    (bind ((slot (slot-of place))
           (type (slot-definition-type slot))
           (prototype
            (case type
              (boolean #t)
              (string "42")
              (list nil)
              (t (class-prototype (find-class type))))))
      (make-alternative-components prototype)))

  (:method ((string string))
    (list (delay-alternative-component-type 'string-component :value string)))

  (:method ((integer integer))
    (list (delay-alternative-component-type 'integer-component :value integer)))

  (:method ((list list))
    (if (typep list 'proper-list)
        (list (if (every-type-p 'standard-object list)
                  (delay-alternative-component-type 'instance-list-table-component :instances list)
                  (delay-alternative-component-type 'list-component :elements list))
              (delay-alternative-component-type 'reference-list-component :targets list)
              (delay-alternative-component 'reference-component
                (setf-expand-reference-to-default-alternative-command-component (make-instance 'reference-component :target list))))
        (call-next-method)))

  (:method ((slot standard-slot-definition))
    (list (delay-alternative-component-type 'slot-detail-component :slot slot)
          (delay-alternative-component 'slot-reference-component
            (setf-expand-reference-to-default-alternative-command-component (make-instance 'slot-reference-component :target slot)))))

  (:method ((class standard-class))
    (list (delay-alternative-component-type 'class-detail-component :the-class class)
          (delay-alternative-component 'class-reference-component
            (setf-expand-reference-to-default-alternative-command-component (make-instance 'class-reference-component :target class)))))

  (:method ((instance standard-object))
    (list (delay-alternative-component-type 'instance-detail-component :instance instance)
          (delay-alternative-component 'instance-reference-component
            (setf-expand-reference-to-default-alternative-command-component (make-instance 'standard-object-reference-component :target instance)))))

  (:method ((instance structure-object))
    (list (delay-alternative-component-type 'instance-detail-component :instance instance)
          (delay-alternative-component 'instance-reference-component
            (setf-expand-reference-to-default-alternative-command-component (make-instance 'standard-object-reference-component :target instance))))))
