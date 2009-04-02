;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Abstract expression component

(def component abstract-expression-component (editable-component)
  ((the-type t)))

;;;;;;
;;; Expression component

(def component expression-component (abstract-expression-component)
  ((input (make-instance 'string-inspector) :type component)
   (kind :type (member :atom :application))
   (expression :type function)
   (arguments nil :type components)
   (command-bar nil :type component)))

(def constructor expression-component
  (setf (command-bar-of -self-) (command-bar (make-edit-expression-command -self-)
                                             (make-accept-expression-command -self-)
                                             (make-remove-expression-argument-command -self-))))

(def render expression-component
  (bind (((:read-only-slots input the-type arguments command-bar) -self-))
    <div <span ,(render input) " : " ,(symbol-name the-type) ,(when command-bar (render command-bar)) >
         ;; TODO: use :class
         <div (:style "margin-left: 20px;")
              ,(foreach #'render arguments)>>))

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
             (add-user-error self "Literal value has invalid type: ~A" new-value)
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
                 (add-user-error self "Function does not return expected type: ~A" new-value)
                 #f))))))))

(def function accept-expression (component)
  (bind ((input (input-of component))
         (value (and (slot-boundp input 'component-value)
                     (component-value-of input))))
    (when (and value
               (edited-p input))
      (bind ((*read-eval* #f)
             (expression (read-from-string value))
             (success? (setf (component-value-of component) expression)))
        (when success?
          (setf (edited-p component) #f))))))

;;;;;;
;;; Commands

(def function make-edit-expression-command (component)
  (command (icon edit)
           (make-action
             (join-editing component))))

(def function make-accept-expression-command (component)
  (command (icon store)
           (make-action
             (map-descendant-components component
                                        (lambda (descendant)
                                          (when (typep descendant 'expression-component)
                                            (accept-expression descendant)))
                                        :include-self #t))
           :default #t))

(def icon add-expression-argument)
(def resources hu
  (icon-label.add-expression-argument "Hozzáadás")
  (icon-tooltip.add-expression-argument "Új paraméter hozzáadása"))
(def resources en
  (icon-label.add-expression-argument "Add")
  (icon-tooltip.add-expression-argument "Add new argument"))

(def function make-add-expression-argument-command (component type)
  (command (icon add-expression-argument)
           (make-action
             (setf (arguments-of component)
                   (append (arguments-of component)
                           (list (make-expression-component nil :the-type type :edited #t)))))))

(def icon remove-expression-argument)
(def resources hu
  (icon-label.remove-expression-argument "Törlés")
  (icon-tooltip.remove-expression-argument "Paraméter törlése"))
(def resources en
  (icon-label.remove-expression-argument "Remove")
  (icon-tooltip.remove-expression-argument "Remove argument"))

(def function make-remove-expression-argument-command (component)
  (command (icon remove-expression-argument)
           (make-action
             (bind ((parent (parent-component-of component)))
               (if (typep parent 'expression-component)
                   (setf (arguments-of parent)
                         (remove component (arguments-of parent)))
                   (setf (edited-p (input-of component)) #t
                         (arguments-of component) nil))))))

;;;;;;
;;; Expression maker

(def component expression-maker (expression-component)
  ())

;;;;;;
;;; Expression inspector

(def component expression-inspector (expression-component)
  ())

;;;;;;
;;; Expression filter

(def component expression-filter (expression-component)
  ())
