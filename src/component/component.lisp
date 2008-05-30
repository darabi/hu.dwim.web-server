;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Component

(def (definer e) component (name supers slots &rest options)
  `(def class* ,name ,supers
     ,slots
     (:export-class-name-p #t)
     (:metaclass component-class)
     ,@options))

(def component component (ui-syntax-node)
  ((parent-component nil)))

(def method render :around ((component component))
  ;; TODO: maybe create a style-class-mixin?
  (restart-case
      (bind ((class-name (string-downcase (symbol-name (class-name (class-of component)))))
             (css-name (subseq class-name 0 (- (length class-name) (length "-component")))))
        ;; TODO: this does not work for <tr> elements under tables (goes out of order into the output)
        <div (:class ,(concatenate 'string css-name (when (debug-component-hierarchy-p *frame*) " debug-component")))
          ,(if (debug-component-hierarchy-p *frame*)
               <div (:class "debug-component-name")
                 ,class-name ,(if (typep component '(or icon-component command-component))
                                  +void+
                                  <a (:href ,(action-to-href (register-action *frame* (make-copy-to-repl-action component)))) "REPL">)>
               +void+)
          ,(call-next-method)>)
    (skip-rendering-component ()
      :report (lambda (stream)
                (format stream "Skip rendering ~A and put an error marker in place" component))
      <div (:class "rendering-error") "Error during rendering " ,(princ-to-string component)>)))

(def function make-copy-to-repl-action (component)
  (make-action
    (awhen (or swank::*emacs-connection*
               (swank::default-connection))
      (swank::with-connection (it)
        (swank::present-in-emacs component)
        (swank::present-in-emacs #.(string #\Newline))))))

(def (type e) components ()
  'sequence)

(def method print-object ((self component) stream)
  (bind ((*print-level* nil)
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
                  (prin1 value)))))))
  self)

;;;;;;
;;; Parent child relationship

(def method (setf slot-value-using-class) :after (new-value (class component-class) (instance component) (slot component-effective-slot-definition))
  (macrolet ((setf-parent (child)
               ;; TODO: (assert (not (parent-component-of child)) nil "The ~A is already under a parent" child)
               `(setf (parent-component-of ,child) instance)))
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

(def function find-ancestor-component (component predicate)
  (find-ancestor component #'parent-component-of predicate))

(def function find-ancestor-component-with-type (component type)
  (find-ancestor-component component (lambda (component)
                                       (typep component type))))

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

(def function map-descendant-components (component thunk &key (include-self #f))
  (labels ((traverse (parent-component)
             (map-child-components parent-component
                                   (lambda (child-component)
                                     (funcall thunk child-component)
                                     (traverse child-component)))))
    (when include-self
      (funcall thunk component))
    (traverse component)))

(def function map-ancestor-components (component thunk &key (include-self #f))
  (labels ((traverse (current)
             (awhen (parent-component-of current)
               (funcall thunk it)
               (traverse it))))
    (when include-self
      (funcall thunk component))
    (traverse component)))

(def function collect-component-ancestors (component &key (include-self #f))
  (nconc
   (when include-self
     (list component))
   (iter (for parent :first component :then (parent-component-of parent))
         (while parent)
         (collect parent))))
