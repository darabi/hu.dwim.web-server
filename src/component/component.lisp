;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Component definer

(def (definer e :available-flags "eas") component (name supers slots &rest options)
  `(def (class* ,@-options-) ,name ,supers
     ,slots
     (:export-class-name-p #t)
     (:metaclass component-class)
     ,@options))

;;;;;;
;;; Component

(def component component ()
  ((parent-component nil :export :accessor)
   ;; TODO: use only one slot for these flags (beware of boolean like slots which might have delayed computations inside)
   (visible #t :type boolean :documentation "True means the component must be visible on the client side, while false means the opposite.")
   ;; TODO expanded flag in the base component class?!
   (expanded #t :type boolean :documentation "True mans the component should display itself with full detail, while false means it should be minimized.")
   (dirty #t :type boolean :documentation "True means the component must be sent to the client to refresh its content.")
   (outdated #t :type boolean :documentation "True means the component must be refreshed before render.")))

(def render :around component ()
  (with-component-environment -self-
    (if (force (visible-p -self-))
        (prog1
            (render-with-debug-component-hierarchy -self- #'call-next-method)
          (setf (dirty-p -self-) #f))
        +void+)))

(def render :before component ()
  ;; we put it in a :before so that more specialized :before's can happend before it
  (ensure-uptodate -self-))

(def function render-component-in-environment (component call-next-method)
  (with-component-environment component 
    (when (force (visible-p component))
      (ensure-uptodate component)
      (funcall call-next-method))))

(def render-csv :around component
  (render-component-in-environment -self- #'call-next-method))

(def render-pdf :around component
  (render-component-in-environment -self- #'call-next-method))

(def render-odt :around component
  (render-component-in-environment -self- #'call-next-method))

(def render-ods :around component
  (render-component-in-environment -self- #'call-next-method))

(def render string
  `xml,-self-)

(def render-csv string
  (render-csv-value -self-))

(def render-ods string
  <text:p ,-self- >)

(def (layered-function e) render-onclick-handler (component))

(def (type e) components ()
  'sequence)

(def (generic e) component-value-of (component))

(def (generic e) (setf component-value-of) (new-value component)
  (:method :after (new-value (component component))
    (mark-outdated component)))

(def (function e) mark-dirty (component)
  (setf (dirty-p component) #t)
  component)

(def (function e) mark-outdated (component)
  (unless (stringp component)
    (setf (outdated-p component) #t))
  component)

(def (function e) mark-top-content-outdated (component)
  (mark-outdated (find-top-component-content component)))

(def function ensure-uptodate (component)
  (when (or (outdated-p component)
            (some (lambda (slot)
                    (not (computed-slot-valid-p component slot)))
                  (computed-slots-of (class-of component))))
    (refresh-component component))
  component)

;;;;;;
;;; Debug

(def function render-with-debug-component-hierarchy (self next-method)
  (declare (type function next-method))
  (restart-case
      (if (and *debug-component-hierarchy*
               ;; TODO: the <table><tr><td> has various constraints, so rows are not displayed in debug mode
               (not (typep self '(or frame-component row-component node-component))))
          (bind ((class-name (string-downcase (symbol-name (class-name (class-of self)))))
                 (*debug-component-hierarchy* (not (typep self 'command-component))))
            <div (:class "debug-component")
              <div (:class "debug-component-name")
                ,class-name
                <span
                  <a (:href ,(register-action/href (make-copy-to-repl-action self))) "REPL">
                  " "
                  <a (:href ,(register-action/href (make-inspect-in-repl-action self))) "INSPECT">>>
              ,(funcall next-method)>)
          (funcall next-method))
    (skip-rendering-component ()
      :report (lambda (stream)
                (format stream "Skip rendering ~A and put an error marker in place" self))
      <div (:class "rendering-error") "Error during rendering " ,(princ-to-string self)>)))

(def function make-inspect-in-repl-action (component)
  (make-action
    (awhen (or swank::*emacs-connection*
               (swank::default-connection))
      (swank::with-connection (it)
        (bind ((swank::*buffer-package* *package*)
               (swank::*buffer-readtable* *readtable*))
          (swank::inspect-in-emacs component))))))

(def function make-copy-to-repl-action (component)
  (make-action
    (awhen (or swank::*emacs-connection*
               (swank::default-connection))
      (swank::with-connection (it)
        (swank::present-in-emacs component)
        (swank::present-in-emacs #.(string #\Newline))))))

(def special-variable *component-print-object-level* 0)

(def method print-object ((self component) *standard-output*)
  (print-component self))

(def generic print-component (component &optional stream))

(def method print-component (self &optional (*standard-output* *standard-output*))
  (if (and *print-level*
           (>= *component-print-object-level* *print-level*))
      (write-char #\#)
      (bind ((*component-print-object-level* (1+ *component-print-object-level*)))
        (handler-bind ((error (lambda (error)
                                (declare (ignore error))
                                (write-string "<<error printing component>>")
                                (return-from print-component))))
          (pprint-logical-block (*standard-output* nil :prefix "#<" :suffix ">")
            (pprint-indent :current 1)
            (bind ((*print-circle* #f))
              (princ (symbol-name (class-name (class-of self)))))
            (iter (with class = (class-of self))
                  (repeat (or *print-length* most-positive-fixnum))
                  (for slot :in (class-slots class))
                  (when (bound-child-component-slot-p class self slot)
                    (bind ((initarg (first (slot-definition-initargs slot)))
                           (value (slot-value-using-class class self slot)))
                      (write-char #\Space)
                      (pprint-newline :fill)
                      (prin1 initarg)
                      (write-char #\Space)
                      (pprint-newline :fill)
                      (prin1 value))))))))
  self)

;;;;;;
;;; Parent child relationship

(def method (setf slot-value-using-class) (new-value (class component-class) (instance component) (slot standard-effective-slot-definition))
  (unless (eq 'dirty (slot-definition-name slot))
    (unless (eq (standard-instance-access instance (slot-definition-location slot)) new-value)
      (call-next-method)
      (mark-dirty instance))))

(def method slot-makunbound-using-class ((class component-class) (instance component) (slot standard-effective-slot-definition))
  (unless (eq 'dirty (slot-definition-name slot))
    (unless (slot-boundp-using-class class instance slot)
      (call-next-method)
      (mark-dirty instance))))

(def method (setf slot-value-using-class) :after (new-value (class component-class) (instance component) (slot component-effective-slot-definition))
  (setf-parent-component-references new-value instance))

(def (function o) setf-parent-component-references (new-value instance &optional parent-component-slot-index)
  (macrolet ((setf-parent (child)
               ;; TODO: (assert (not (parent-component-of child)) nil "The ~A is already under a parent" child)
               `(if parent-component-slot-index
                    (setf (standard-instance-access ,child parent-component-slot-index) instance)
                    (setf (parent-component-of ,child) instance))))
    (typecase new-value
      (component
       (setf-parent new-value))
      (list
       (dolist (element new-value)
         (when (typep element 'component)
           (setf-parent element))))
      (hash-table
       (iter (for (key value) :in-hashtable new-value)
             (when (typep value 'component)
               (setf-parent value))
             (when (typep key 'component)
               (setf-parent key)))))))

(def (function io) bound-child-component-slot-p (class instance slot)
  (and (typep slot 'component-effective-slot-definition)
       (not (eq (slot-definition-name slot) 'parent-component))
       (slot-boundp-using-class class instance slot)))

(def (function e) find-ancestor-component (component predicate)
  (find-ancestor component #'parent-component-of predicate))

(def (function e) find-ancestor-component-with-type (component type)
  (find-ancestor-component component [typep !1 type]))

(def (function e) find-descendant-component (component predicate)
  (map-descendant-components component (lambda (child)
                                         (when (funcall predicate child)
                                           (return-from find-descendant-component child)))))

(def (function e) find-descendant-component-with-type (component type)
  (find-descendant-component component [typep !1 type]))

(def function map-child-components (component visitor)
  (ensure-functionf visitor)
  (iter (with class = (class-of component))
        (for slot :in (class-slots class))
        (when (bound-child-component-slot-p class component slot)
          (bind ((value (slot-value-using-class class component slot)))
            (typecase value
              (component
               (funcall visitor value))
              (list
               (dolist (element value)
                 (when (typep element 'component)
                   (funcall visitor element))))
              (hash-table
               (iter (for (key element) :in-hashtable value)
                     (when (typep element 'component)
                       (funcall visitor element)))))))))

(def function map-descendant-components (component visitor &key (include-self #f))
  (ensure-functionf visitor)
  (labels ((traverse (parent-component)
             (map-child-components parent-component
                                   (lambda (child-component)
                                     (funcall visitor child-component)
                                     (traverse child-component)))))
    (when include-self
      (funcall visitor component))
    (traverse component)))

(def function map-ancestor-components (component visitor &key (include-self #f))
  (ensure-functionf visitor)
  (labels ((traverse (current)
             (awhen (parent-component-of current)
               (funcall visitor it)
               (traverse it))))
    (when include-self
      (funcall visitor component))
    (traverse component)))

(def function collect-component-ancestors (component &key (include-self #f))
  (nconc
   (when include-self
     (list component))
   (iter (for parent :first component :then (parent-component-of parent))
         (while parent)
         (collect parent))))

(def (generic e) clone-component (component)
  (:method ((self string))
    self)

  (:method ((self component))
    (make-instance (class-of self)))

  (:method :around ((self component))
    ;; this must be done at last after all primary method customization
    (prog1-bind clone (call-next-method)
      (setf (component-value-of clone) (component-value-of self)))))
