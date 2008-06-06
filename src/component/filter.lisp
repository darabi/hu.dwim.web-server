;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object filter

(def component standard-object-filter-component (abstract-standard-class-component alternator-component)
  ((result :type component)))

(def method (setf component-value-of) :after (new-value (self standard-object-filter-component))
  (with-slots (result the-class default-component-type alternatives content command-bar) self
    (setf result (make-instance 'empty-component))
    (if the-class
        (setf alternatives (list (delay-alternative-component-type 'standard-object-filter-detail-component :the-class the-class)
                                 (delay-alternative-component 'standard-object-filter-reference-component
                                   (setf-expand-reference-to-default-alternative-command (make-instance 'standard-object-filter-reference-component :target the-class))))
              content (if default-component-type
                          (find-alternative-component alternatives default-component-type)
                          (find-default-alternative-component alternatives)))
        (setf alternatives nil
              content nil))
    (setf command-bar (make-instance 'command-bar-component :commands (append (list (make-top-command self)
                                                                                    (make-filter-instances-command self (delay (result-of self))))
                                                                              (make-alternative-commands self alternatives))))))

(def render standard-object-filter-component ()
  (with-slots (result content command-bar) -self-
    (if (typep content '(or reference-component atomic-component))
        (render content)
        (render-vertical-list (list content command-bar result)))))

;;;;;;
;;; Standard object filter detail

(def component standard-object-filter-detail-component (abstract-standard-class-component detail-component)
  ((class :accessor nil :type component)
   (slot-value-group :type component)))

(def method (setf component-value-of) :after (new-value (self standard-object-filter-detail-component))
  (with-slots (the-class class slot-value-group command-bar) self
    (setf class (make-viewer-component the-class :default-component-type 'reference-component)
          slot-value-group (make-instance 'standard-object-slot-value-group-filter-component :the-class the-class :slots (standard-object-filter-detail-slots the-class)))))

(def generic standard-object-filter-detail-slots (class)
  (:method ((class standard-class))
    (class-slots class))

  (:method ((class prc::persistent-class))
    (iter (for slot :in (prc::persistent-effective-slots-of class))
          (if (dmm::authorize-operation 'dmm::filter-entity-property-operation :-entity- class :-property- slot)
              (collect slot)))))

(def render standard-object-filter-detail-component ()
  (with-slots (the-class class slots-values slot-value-group command-bar) -self-
    <div
      <span "Filter instances of " ,(render class)>
      <div
        <h3 "Slots">
        ,(render slot-value-group)>>))

;;;;;;
;;; Standard object slot value group

(def component standard-object-slot-value-group-filter-component (abstract-standard-slot-definition-group-component)
  ((slot-values nil :type components)))

(def method (setf component-value-of) :after (new-value (self standard-object-slot-value-group-filter-component))
  (with-slots (the-class slots slot-values) self
    (setf slot-values
          (iter (for slot :in slots)
                (for slot-value = (find slot slot-values :key #'component-value-of))
                (if slot-value
                    (setf (component-value-of slot-value) slot)
                    (setf slot-value (make-instance 'standard-object-slot-value-filter-component :the-class the-class :slot slot)))
                (collect slot-value)))))

(def render standard-object-slot-value-group-filter-component ()
  (with-slots (slot-values) -self-
    (if slot-values
        <table
          <thead
            <tr
              <th "Name">
              <th>
              <th>
              <th "Value">>>
          <tbody ,@(mapcar #'render slot-values)>>
        <span "There are none">)))

;;;;;;
;;; Standard object slot value filter

;; TODO: all predicates
(def (constant :test 'equalp) +filter-predicates+ '(equal like #+nil (< <= > >= between)))

(def component standard-object-slot-value-filter-component (abstract-standard-slot-definition-component)
  ((slot-name)
   (negated #f :type boolean)
   (negate-command :type component)
   (condition 'equal :type (member #.+filter-predicates+))
   (condition-command :type component)
   (label nil :type component)
   (value nil :type component)))

(def method (setf component-value-of) :after (new-value (self standard-object-slot-value-filter-component))
  (with-slots (slot slot-name negated negate-command condition condition-command label value) self
    (setf slot-name (slot-definition-name slot)
          label (make-instance 'label-component :component-value (full-symbol-name slot-name))
          negate-command (make-instance 'command-component
                                        :icon (make-negated/ponated-icon negated)
                                        :action (make-action
                                                  (setf negated (not negated))
                                                  (setf (icon-of negate-command) (make-negated/ponated-icon negated))))
          condition-command (make-instance 'command-component
                                           :icon (make-condition-icon condition)
                                           :action (make-action
                                                     (setf condition (elt +filter-predicates+
                                                                          (mod (1+ (position condition +filter-predicates+))
                                                                               (length +filter-predicates+))))
                                                     (setf (icon-of condition-command) (make-condition-icon condition))))
          value (make-filter-component (slot-definition-type slot) :default-component-type 'reference-component))))

(def function make-negated/ponated-icon (negated)
  (aprog1 (clone-icon (if negated 'negated 'ponated))
    (setf (label-of it) nil)))

(def function make-condition-icon (condition)
  (aprog1 (clone-icon
           (ecase condition
             (equal 'equal)
             (like 'like)))
    (setf (label-of it) nil)))

(def render standard-object-slot-value-filter-component ()
  (with-slots (label negate-command condition-command value) -self-
    <tr (:class ,(odd/even-class -self- (slot-values-of (parent-component-of -self-))))
      <td ,(render label)>
      <td ,(render negate-command)>
      <td ,(render condition-command)>
      <td ,(render value)>>))

;;;;;;
;;; Filter

(def (function e) make-filter-instances-command (filter result)
  (make-replace-and-push-back-command result (delay (make-filter-result-component filter (execute-filter filter (the-class-of filter))))
                                      (list :icon (clone-icon 'filter))
                                      (list :icon (clone-icon 'back))))

(def (generic e) make-filter-result-component (filter result)
  (:method ((filter standard-object-filter-component) (result list))
    (prog1-bind component
        (make-viewer-component result :type `(list ,(class-name (the-class-of filter))))
      (unless result
        (add-user-warning component "No matches were found")))))

(def generic execute-filter (component class)
  (:method ((component standard-object-filter-component) (class standard-class))
    (execute-filter (content-of component) class))

  (:method ((component standard-object-filter-detail-component) (class standard-class))
    (bind ((slot-values (slot-values-of (slot-values-of component)))
           (slot-names (mapcar #'slot-name-of slot-values))
           (values (mapcar (lambda (slot-value)
                             (bind ((component (value-of slot-value)))
                               (typecase component
                                 (atomic-component
                                  (component-value-of component)))))
                           slot-values)))
      (prog1-bind instances nil
        (sb-vm::map-allocated-objects
         (lambda (instance type size)
           (declare (ignore type size))
           (bind ((instance-class (class-of instance)))
             (when (and (typep instance class)
                        (not (eq instance (closer-mop:class-prototype instance-class)))
                        (every (lambda (slot-name value)
                                 (or (not value)
                                     (bind ((slot (find-slot instance-class slot-name)))
                                       (and slot
                                            (slot-boundp-using-class instance-class instance slot)
                                            (equal value (slot-value-using-class instance-class instance slot))))))
                               slot-names values))
               (push instance instances))))
         :dynamic))))

  (:method ((component standard-object-filter-component) (class prc::persistent-class))
    (prc::execute-query (build-filter-query component))))

(def class* filter-query ()
  ((query nil)
   (query-variable-stack nil)))

(def generic build-filter-query (component)
  (:method ((component standard-object-filter-component))
    (bind ((filter-query (make-instance 'filter-query :query (prc::make-instance 'prc::query))))
      (build-filter-query* component filter-query)
      (query-of filter-query))))

(def generic build-filter-query* (component filter-query)
  (:method ((component standard-object-filter-component) filter-query)
    (build-filter-query* (content-of component) filter-query))

  (:method ((component standard-object-filter-detail-component) filter-query)
    (bind ((query (query-of filter-query))
           (query-variable
            (prc::add-query-variable (query-of filter-query)
                                     (gensym (symbol-name (class-name (the-class-of component)))))))
      (unless (query-variable-stack-of filter-query)
        (prc::add-collect query query-variable))
      (push query-variable (query-variable-stack-of filter-query))
      (prc::add-assert query `(typep ,query-variable ',(class-name (the-class-of component))))
      (build-filter-query* (slot-value-group-of component) filter-query)))

  (:method ((component standard-object-filter-reference-component) filter-query)
    (values))

  (:method ((component standard-object-slot-value-group-filter-component) filter-query)
    (dolist (slot-value (slot-values-of component))
      (build-filter-query* slot-value filter-query)))

  (:method ((component standard-object-slot-value-filter-component) filter-query)
    (bind ((value-component (value-of component)))
      ;; TODO: recurse
      (when (typep value-component 'atomic-component)
        (bind ((value (build-filter-query* value-component filter-query)))
          (when (and value
                     (not (string= value "")))
            (bind ((predicate-name
                    (ecase (condition-of component)
                      (equal 'equal)
                      (like 'prc::re-like)))
                   (ponated-predicate
                    (list predicate-name
                          (list (prc::reader-name-of (slot-of component))
                                (first (query-variable-stack-of filter-query)))
                          value)))
              (prc::add-assert (query-of filter-query)
                               (if (negated-p component)
                                   `(not ,ponated-predicate)
                                   ponated-predicate))))))))

  (:method ((component atomic-component) filter-query)
    (component-value-of component)))
