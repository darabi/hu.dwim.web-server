;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Filter

(def component filter-component ()
  ())

;;;;;;
;;; Standard object filter

(def component standard-object-filter (abstract-standard-class-component
                                       filter-component
                                       alternator-component
                                       user-message-collector-component-mixin
                                       remote-identity-component-mixin)
  ((result
    (make-instance 'empty-component)
    :type component)
   (result-component-factory
    #'make-standard-object-filter-result-inspector
    :type function))
  (:default-initargs :alternatives-factory #'make-standard-object-filter-alternatives))

(def (macro e) standard-object-filter (the-class)
  `(make-instance 'standard-object-filter :the-class ,the-class))

(def method refresh-component ((self standard-object-filter))
  (with-slots (result the-class default-component-type alternatives content command-bar) self
    (if the-class
        (setf alternatives (funcall (alternatives-factory-of self) self the-class (class-prototype the-class))
              content (if default-component-type
                          (find-alternative-component alternatives default-component-type)
                          (find-default-alternative-component alternatives)))
        (setf alternatives nil
              content nil))
    (setf command-bar (make-alternator-command-bar self alternatives
                                                   (append (list (make-open-in-new-frame-command self)
                                                                 (make-top-command self))
                                                           (make-standard-object-filter-commands self the-class (class-prototype the-class)))))))

(def (generic e) make-standard-object-filter-alternatives (component class instance)
  (:method ((component standard-object-filter) (class standard-class) (instance standard-object))
    (list (delay-alternative-component-with-initargs 'standard-object-detail-filter :the-class class)
          (delay-alternative-reference-component 'standard-object-filter-reference-component class))))

(def (generic e) make-standard-object-filter-commands (component class prototype)
  (:method ((component standard-object-filter) (class standard-class) (prototype standard-object))
    (list (make-filter-instances-command component (delay (result-of component))))))

(def render standard-object-filter ()
  (with-slots (result content command-bar id) -self-
    <div (:id ,id)
         ,(render-user-messages -self-)
         ,(render content)
         ,@(unless (typep content '(or reference-component atomic-component))
             (list (render command-bar) (render result)))>))

;;;;;;
;;; Standard object filter detail

(def component standard-object-detail-filter (abstract-standard-class-component
                                              filter-component
                                              detail-component
                                              remote-identity-component-mixin)
  ((class-selector :type component)
   (class :accessor nil :type component)
   (slot-value-group :type component)))

(def constructor standard-object-detail-filter ()
  (with-slots (the-class class-selector) -self-
    (setf class-selector
          (when-bind subclasses (subclasses the-class)
            (make-instance 'member-component
                           :edited #t
                           :allow-nil-value #t
                           :component-value the-class
                           :possible-values subclasses)))))

(def method refresh-component ((self standard-object-detail-filter))
  (with-slots (the-class class slot-value-group command-bar) self
    (setf class (make-viewer-component the-class :default-component-type 'reference-component)
          slot-value-group (make-instance 'standard-object-slot-value-group-filter :the-class the-class :slots (collect-standard-object-detail-filter-slots self the-class (class-prototype the-class))))))

(def (generic e) collect-standard-object-detail-filter-slots (component class instance)
  (:method ((component standard-object-detail-filter) (class standard-class) (instance standard-object))
    (class-slots class))

  (:method ((component standard-object-detail-filter) (class prc::persistent-class) (instance prc::persistent-object))
    (remove-if #'prc:persistent-object-internal-slot-p (call-next-method)))

  (:method ((component standard-object-detail-filter) (class dmm::entity) (instance prc::persistent-object))
    (filter-if (lambda (slot)
                 (dmm::authorize-operation 'dmm::filter-entity-property-operation :-entity- class :-property- slot))
               (call-next-method))))

(def render standard-object-detail-filter ()
  (with-slots (class-selector class slot-value-group id) -self-
    <div (:id ,id)
      <div "Filter instances of " ,(render class)>
      ,(if class-selector
           (render class-selector)
           +void+)
      <div
        <h3 "Slots">
        ,(render slot-value-group)>>))

;;;;;;
;;; Standard object slot value group

(def component standard-object-slot-value-group-filter (abstract-standard-slot-definition-group-component
                                                        filter-component
                                                        remote-identity-component-mixin)
  ((slot-values nil :type components)))

(def method refresh-component ((self standard-object-slot-value-group-filter))
  (with-slots (the-class slots slot-values) self
    (setf slot-values
          (iter (for slot :in slots)
                (for slot-value = (find slot slot-values :key #'component-value-of))
                (if slot-value
                    (setf (component-value-of slot-value) slot)
                    (setf slot-value (make-instance 'standard-object-slot-value-filter :the-class the-class :slot slot)))
                (collect slot-value)))))

(def render standard-object-slot-value-group-filter ()
  (with-slots (slot-values id) -self-
    (if slot-values
        <table (:id ,id)
          <thead
            <tr
              <th "Name">
              <th>
              <th>
              <th "Value">>>
          <tbody ,@(mapcar #'render slot-values)>>
        <span (:id ,id) "There are none">)))

;;;;;;
;;; Standard object slot value filter

;; TODO: all predicates
(def (constant :test 'equalp) +filter-predicates+ '(equal like < <= > >= #+nil(between)))

(def component standard-object-slot-value-filter (abstract-standard-slot-definition-component
                                                  filter-component
                                                  remote-identity-component-mixin)
  ((slot-name)
   (negated #f :type boolean)
   (negate-command :type component)
   (predicate 'equal :type (member #.+filter-predicates+))
   (predicate-command :type component)
   (label nil :type component)
   (value nil :type component)))

(def method refresh-component ((self standard-object-slot-value-filter))
  (with-slots (slot slot-name negated negate-command predicate predicate-command label value) self
    (setf slot-name (slot-definition-name slot)
          label (label (localized-slot-name slot))
          negate-command (make-instance 'command-component
                                        :icon (make-negated/ponated-icon negated)
                                        :action (make-action
                                                  (setf negated (not negated))
                                                  (setf (icon-of negate-command) (make-negated/ponated-icon negated))))
          predicate-command (make-instance 'command-component
                                           :icon (make-predicate-icon predicate)
                                           :action (make-action
                                                     (setf predicate (elt +filter-predicates+
                                                                          (mod (1+ (position predicate +filter-predicates+))
                                                                               (length +filter-predicates+))))
                                                     (setf (icon-of predicate-command) (make-predicate-icon predicate))))
          value (make-filter-component (slot-type slot) :default-component-type 'reference-component))))

(def function make-negated/ponated-icon (negated)
  (aprog1 (make-icon-component (if negated 'negated 'ponated))
    (setf (label-of it) nil)))

(def function make-predicate-icon (predicate)
  (aprog1 (make-icon-component predicate)
    (setf (label-of it) nil)))

(def render standard-object-slot-value-filter ()
  (with-slots (label negate-command predicate-command value id) -self-
    <tr (:id ,id :class ,(odd/even-class -self- (slot-values-of (parent-component-of -self-))))
      <td ,(render label)>
      <td ,(render negate-command)>
      <td ,(render predicate-command)>
      <td ,(render value)>>))

;;;;;;
;;; Filter

(def (function e) make-filter-instances-command (filter result)
  (make-replace-and-push-back-command result (delay (funcall (result-component-factory-of filter) filter
                                                             (execute-filter-instances filter (the-class-of filter))))
                                      (list :icon (icon filter))
                                      (list :icon (icon back))))

(def (generic e) make-standard-object-filter-result-inspector (filter result)
  (:method ((filter standard-object-filter) (result list))
    (prog1-bind component
        (make-viewer-component result :type `(list ,(class-name (the-class-of filter))))
      (unless result
        (add-user-warning component "No matches were found")))))

(def generic execute-filter-instances (component class)
  (:method ((component standard-object-filter) (class standard-class))
    (execute-filter-instances (content-of component) class))

  (:method ((component standard-object-detail-filter) (class standard-class))
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
                        (not (eq instance (class-prototype instance-class)))
                        (every (lambda (slot-name value)
                                 (or (not value)
                                     (bind ((slot (find-slot instance-class slot-name)))
                                       (and slot
                                            (slot-boundp-using-class instance-class instance slot)
                                            (equal value (slot-value-using-class instance-class instance slot))))))
                               slot-names values))
               (push instance instances))))
         :dynamic))))

  (:method ((component standard-object-filter) (class prc::persistent-class))
    (prc::execute-query (build-filter-query component))))

;;;;;
;;; Query builder

(def class* filter-query ()
  ((query nil)
   (query-variable-stack nil)))

(def function call-with-new-query-variable (component filter-query thunk)
  (bind ((query (query-of filter-query))
         (query-variable
          (prc::add-query-variable (query-of filter-query)
                                   (gensym (symbol-name (class-name (the-class-of component)))))))
    (push query-variable (query-variable-stack-of filter-query))
    (prc::add-assert query `(typep ,query-variable ',(class-name (the-class-of component))))
    (funcall thunk query-variable)
    (pop (query-variable-stack-of filter-query))))

(def generic build-filter-query (component)
  (:method ((component standard-object-filter))
    (bind ((query (prc::make-instance 'prc::query))
           (filter-query (make-instance 'filter-query :query query)))
      (call-with-new-query-variable component filter-query
                                    (lambda (query-variable)
                                      (prc::add-collect query query-variable)
                                      (build-filter-query* component filter-query)))
      query)))

(def generic build-filter-query* (component filter-query)
  (:method ((component standard-object-filter) filter-query)
    (build-filter-query* (content-of component) filter-query))

  (:method ((component standard-object-detail-filter) filter-query)
    (when-bind class-selector (class-selector-of component)
      (when-bind selected-class (component-value-of class-selector)
        (prc::add-assert (query-of filter-query) `(typep ,(first (query-variable-stack-of filter-query)) ,selected-class))))
    (build-filter-query* (slot-value-group-of component) filter-query))

  (:method ((component standard-object-filter-reference-component) filter-query)
    (values))

  (:method ((component standard-object-slot-value-group-filter) filter-query)
    (dolist (slot-value (slot-values-of component))
      (build-filter-query* slot-value filter-query)))

  (:method ((component standard-object-slot-value-filter) filter-query)
    (bind ((value-component (value-of component)))
      (cond ((typep value-component 'atomic-component)
             (bind ((value (component-value-of value-component)))
               (when (and value
                          (or (not (stringp value))
                              (not (string= value ""))))
                 (bind ((predicate (predicate-of component))
                        (predicate-name (if (eq 'like predicate)
                                            'prc::re-like
                                            predicate))
                        (ponated-predicate `(,predicate-name
                                             (,(prc::reader-name-of (slot-of component))
                                               ,(first (query-variable-stack-of filter-query)))
                                             (quote ,value))))
                   (prc::add-assert (query-of filter-query)
                                    (if (negated-p component)
                                        `(not ,ponated-predicate)
                                        ponated-predicate))))))
            ((and (typep value-component 'standard-object-filter)
                  (not (typep (content-of value-component) 'standard-object-filter-reference-component)))
             (call-with-new-query-variable value-component filter-query
                                           (lambda (query-variable)
                                             (prc::add-assert (query-of filter-query)
                                                              `(eq ,query-variable
                                                                   (,(prc::reader-name-of (slot-of component))
                                                                     ,(second (query-variable-stack-of filter-query)))))
                                             (build-filter-query* value-component filter-query))))))))
