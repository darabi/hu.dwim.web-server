;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Abstract expression component

(def (component e) abstract-expression-component (editable/mixin)
  ((the-type t)))

;;;;;;
;;; Expression component

(def (component e) expression-component (abstract-expression-component)
  ((input (make-instance 'string/inspector) :type component)
   (kind :type (member :atom :application))
   (expression :type function)
   (arguments nil :type components)
   (command-bar nil :type component)))

(def constructor expression-component
  (setf (command-bar-of -self-) (command-bar (make-edit-expression-command -self-)
                                             (make-accept-expression-command -self-)
                                             (make-remove-expression-argument-command -self-))))

(def render-xhtml expression-component
  (bind (((:read-only-slots input the-type arguments command-bar) -self-))
    <div <span ,(render-component input) " : " ,(symbol-name the-type) ,(when command-bar (render-component command-bar)) >
         ;; TODO: use :class
         <div (:style "margin-left: 20px;")
              ,(foreach #'render-component arguments)>>))

(def method collect-possible-filter-predicates ((self expression-component))
  nil)

(def type any ()
  t)

(def generic expression-application-type (component name)
  (:method ((component abstract-expression-component) (name symbol))
    '(function (&rest any) any)))

(def generic make-expression-component (expression &rest args)
  (:method (expression &rest args)
    (apply #'make-instance 'expression-component args))

  (:method ((expression list) &rest args)
    (apply #'make-expression-component (first expression) args)))

(def method component-value-of ((self expression-component))
  (awhen (expression-of self)
    (force it)))

(def method (setf component-value-of) (new-value (self expression-component))
  (bind (((:slots input the-type kind expression arguments command-bar) self))
    (etypecase new-value
      ((or string number keyword symbol)
       (if (typep new-value the-type)
           (progn
             (setf (component-value-of input) (write-to-string new-value)
                   kind :atom
                   expression new-value)
             #t)
           (progn
             (add-component-error-message self "Literal value has invalid type: ~A" new-value)
             #f)))
      (cons
       (bind ((name (car new-value))
              (ftype (expression-application-type self name)))
         (bind ((arg-types (second ftype))
                (return-type (third ftype))
                ((:values required-parameters nil rest-parameter nil)
                 (parse-ordinary-lambda-list arg-types)))
           (if (subtypep return-type the-type)
               (progn
                 (setf (component-value-of input) (write-to-string name)
                       kind :application
                       expression (delay (list* name
                                                (mapcar 'component-value-of arguments)))
                       arguments (mapcar (lambda (argument-type argument-value)
                                           (aprog1 (make-expression-component argument-value :the-type argument-type)
                                             (setf (component-value-of it) argument-value)))
                                         (append required-parameters
                                                 (when rest-parameter
                                                   (iter (repeat (- (length new-value)
                                                                    (length required-parameters)))
                                                         (collect rest-parameter))))
                                         (rest new-value))
                       command-bar (when rest-parameter
                                     (command-bar (make-accept-expression-command self)
                                                  (make-add-expression-argument-command self rest-parameter)
                                                  (make-remove-expression-argument-command self))))
                 #t)
               (progn
                 (add-component-error-message self "Function does not return expected type: ~A" new-value)
                 #f))))))))

(def function accept-expression (component)
  (bind ((input (input-of component))
         (value (and (slot-boundp input 'component-value)
                     (component-value-of input))))
    (when (and value
               (edited? input))
      (bind ((*read-eval* #f)
             (expression (read-from-string value))
             (success? (setf (component-value-of component) expression)))
        (when success?
          (setf (edited? component) #f))))))

;;;;;;
;;; Commands

(def function make-edit-expression-command (component)
  (command ()
    (icon edit)
    (make-component-action component
      (join-editing component))))

(def function make-accept-expression-command (component)
  (command (:default #t)
    (icon store)
    (make-component-action component
      (map-descendant-components component
                                 (lambda (descendant)
                                   (when (typep descendant 'expression-component)
                                     (accept-expression descendant)))
                                 :include-self #t))))

(def (icon e) add-expression-argument)

(def function make-add-expression-argument-command (component type)
  (command ()
    (icon add-expression-argument)
    (make-component-action component
      (setf (arguments-of component)
            (append (arguments-of component)
                    (list (make-expression-component nil :the-type type :edited #t)))))))

(def (icon e) remove-expression-argument)

(def function make-remove-expression-argument-command (component)
  (command ()
    (icon remove-expression-argument)
    (make-component-action component
      (bind ((parent (parent-component-of component)))
        (if (typep parent 'expression-component)
            (setf (arguments-of parent)
                  (remove component (arguments-of parent)))
            (setf (edited? (input-of component)) #t
                  (arguments-of component) nil))))))

;;;;;;
;;; Expression maker

(def (component e) expression-maker (expression-component)
  ())

;;;;;;
;;; Expression inspector

(def (component e) expression/inspector (expression-component)
  ())

;;;;;;
;;; Expression filter

(def (component e) expression-filter (expression-component)
  ())
