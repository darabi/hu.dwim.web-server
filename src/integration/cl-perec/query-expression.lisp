;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Select expression component

(def (component ea) select-expression-component (abstract-expression-component)
  ((select-clause (make-instance 'expression-component :the-type 'list) :type component)
   (from-clause (make-class-selector (sort (filter-if (lambda (class)
                                                        (dmm::authorize-operation 'dmm::filter-entity-operation :-entity- class))
                                                      (dmm::collect-entities))
                                           #'dmm::dwim-string<
                                           :key #'localized-class-name)) :type component)
   (where-clause (make-instance 'expression-component :the-type 'boolean) :type component)))

(def render-xhtml select-expression-component
  (bind (((:read-only-slots select-clause from-clause where-clause) -self-))
    <div <div ,#"label.select-clause" ,(render-component select-clause)>
         <div ,#"label.from-clause" ,(render-component from-clause)>
         <div ,#"label.where-clause" ,(render-component where-clause)>>))

(def method component-value-of ((self select-expression-component))
  (bind (((:slots select-clause from-clause where-clause) self))
    `(cl-perec::select ,(component-value-of select-clause)
       (cl-perec::from (cl-perec::-instance- ,(component-value-of from-clause)))
       ,(component-value-of where-clause))))

(def method (setf component-value-of) (new-value (self select-expression-component))
  (bind (((:slots select-clause from-clause where-clause) self))
    (setf (component-value-of select-clause) (second new-value)
          (component-value-of from-clause) (find-class (second (second (third new-value))))
          (component-value-of where-clause) (fourth new-value))))

(def method make-expression-component ((expression (eql 'cl-perec::select)) &rest args)
  (apply #'make-instance 'select-expression-component args))

;;;;;;
;;; Generic filter

(def (component ea) generic-filter (filter/abstract title-mixin user-messages/mixin id/mixin)
  ((expression (make-instance 'select-expression-component) :type component)
   (command-bar :type component)
   (result (make-instance 'empty-component) :type component)))

(def constructor generic-filter
  (setf (command-bar-of -self-) (command-bar (make-execute-filter-command -self-))
        (title-of -self-) "Generic filter"))

(def render-xhtml generic-filter
  (bind (((:read-only-slots expression command-bar result id) -self-))
    <div (:id ,id :class "generic-filter")
         ,(render-title -self-)
         ,(render-user-messages -self-)
         ,(render-component expression)
         ,(render-component command-bar)
         ,(render-component result)>
    (render-remote-setup -self-)))

(def function make-execute-filter-command (component)
  (make-replace-and-push-back-command (delay (result-of component))
                                      (delay (with-restored-component-environment component
                                               (make-viewer (execute-filter component))))
                                      (list :content (icon filter) :default #t)
                                      (list :content (icon back))))

;; TODO: use this generic function for standard-object-filter
(def generic execute-filter (component)
  (:method ((component component))
    (bind ((query (prc::make-instance 'prc::query))
           (class-name (component-value-of (from-clause-of component)))
           (query-variable (prc::add-query-variable query 'cl-perec::-instance-)))
      (prc::add-assert query `(typep ,query-variable ',class-name))
      (prc::add-collect query query-variable)
      (awhen (component-value-of (where-clause-of component))
        (prc::add-assert query it))
      (prc::execute-query query))))
