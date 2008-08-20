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
   (expanded #t :type boolean :documentation "True mans the component should display itself with full detail, while false means it should be minimized.")
   (dirty #t :type boolean :documentation "True means the component must be sent to the client to refresh its content.")
   (outdated #t :type boolean :documentation "True means the component must be refreshed before render.")))

(def render :around component ()
  (if (force (visible-p -self-))
      (progn
        (ensure-uptodate -self-)
        (prog1 (render-with-debug-component-hierarchy -self- #'call-next-method)
          (setf (dirty-p -self-) #f)))
      +void+))

(def (type e) components ()
  'sequence)

(def (generic e) component-value-of (component))

(def (generic e) (setf component-value-of) (new-value component)
  (:method :after (new-value (component component))
    (setf (outdated-p component) #t)))

(def (function e) mark-dirty (component)
  (setf (dirty-p component) #t))

(def function ensure-uptodate (component)
  (when (outdated-p component)
    (refresh-component component)))

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
                  <a (:href ,(action-to-href (register-action *frame* (make-copy-to-repl-action self)))) "REPL">
                  " "
                  <a (:href ,(action-to-href (register-action *frame* (make-inspect-in-repl-action self)))) "INSPECT">>>
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

(def special-variable *component-print-object-depth* 3)

(def method print-object ((self component) stream)
  (bind ((*print-level* nil)
         (*component-print-object-level* (1+ *component-print-object-level*))
         (*standard-output* stream))
    (handler-bind ((error (lambda (error)
                            (declare (ignore error))
                            (write-string "<<error printing component>>")
                            (return-from print-object))))
      (pprint-logical-block (stream nil :prefix "#<" :suffix ">")
        (pprint-indent :current 1 stream)
        (iter (with class = (class-of self))
              (with class-name = (symbol-name (class-name class)))
              (initially (princ class-name))
              (for slot :in (class-slots class))
              (when (bound-child-component-slot-p class self slot)
                (bind ((initarg (first (slot-definition-initargs slot)))
                       (value (slot-value-using-class class self slot)))
                  (write-char #\Space)
                  (pprint-newline :fill)
                  (prin1 initarg)
                  (write-char #\Space)
                  (pprint-newline :fill)
                  (if (<= *component-print-object-level* *component-print-object-depth*)
                      (prin1 value)
                      (princ "#"))))))))
  self)

;;;;;;
;;; Parent child relationship

(def method (setf slot-value-using-class) :after (new-value (class component-class) (instance component) (slot standard-effective-slot-definition))
  (unless (eq 'dirty (slot-definition-name slot))
    (mark-dirty instance)))

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

(def function map-child-components (component thunk)
  (iter (with class = (class-of component))
        (for slot :in (class-slots class))
        (when (bound-child-component-slot-p class component slot)
          (bind ((value (slot-value-using-class class component slot)))
            (typecase value
              (component
               (funcall thunk value))
              (list
               (dolist (element value)
                 (when (typep element 'component)
                   (funcall thunk element)))))))))

(def function map-descendant-components (component visitor &key (include-self #f))
  (labels ((traverse (parent-component)
             (map-child-components parent-component
                                   (lambda (child-component)
                                     (funcall visitor child-component)
                                     (traverse child-component)))))
    (when include-self
      (funcall visitor component))
    (traverse component)))

(def function map-ancestor-components (component visitor &key (include-self #f))
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
  (:method ((self component))
    (make-instance (class-of self)))

  (:method :around ((self component))
    ;; this must be done at last after all primary method customization
    (prog1-bind clone (call-next-method)
      (setf (component-value-of clone) (component-value-of self)))))
