;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Reference

(def (component e) reference-component (id/mixin)
  ((target)
   (expand-command :type component)))

(def method component-value-of ((self reference-component))
  (target-of self))

(def method (setf component-value-of) (new-value (self reference-component))
  (setf (target-of self) new-value))

(def refresh-component reference-component
  (bind (((:slots target expand-command) -self-))
    (when (typep expand-command 'command/basic)
      (setf (label-of (content-of expand-command)) (make-reference-label -self- (class-of target) target)))))

(def render-xhtml reference-component
  <span (:id ,(id-of -self-) :class "reference")
    ,(render-component (expand-command-of -self-))>)

(def render-component reference-component
  (render-component (expand-command-of -self-)))

(def (generic e) make-reference-label (component class target)
  (:method ((component reference-component) class target)
    (princ-to-string target)))

(def (layered-function e) make-expand-reference-command (reference class target expansion)
  (:method ((reference reference-component) class target expansion)
    (make-replace-command reference expansion :content (icon expand) :ajax (delay (id-of reference)))))

;;;;;;
;;; Reference list

(def (component e) reference-list-component ()
  ((targets)
   (references :type components)))

(def constructor reference-list-component ()
  (bind (((:slots targets references) -self-))
    (setf references
          (mapcar (lambda (target)
                    (make-viewer target :initial-alternative-type 'reference-component))
                  targets))))

(def render-xhtml reference-list-component
  <div ,(foreach #'render-component (references-of -self-))>)

;;;;;;
;;; Standard object reference

(def (component e) standard-object-inspector-reference (reference-component)
  ())

(def layered-method make-expand-reference-command :before ((reference standard-object-inspector-reference) (class standard-class) (target standard-object) expansion)
  (reuse-standard-object-inspector-reference reference))

(def function reuse-standard-object-inspector-reference (self)
  (bind ((instance (target-of self)))
    (setf (target-of self) (reuse-standard-object-instance (class-of instance) instance))))

(def layered-method refresh-component :before ((self standard-object-inspector-reference))
  (reuse-standard-object-inspector-reference self))

(def render-xhtml :before standard-object-inspector-reference
  (reuse-standard-object-inspector-reference -self-))

;;;;;;
;;; Standard object list reference

(def (component e) standard-object-list-inspector-reference (reference-component)
  ())

(def method make-reference-label ((reference standard-object-list-inspector-reference) class (list list))
  (apply #'concatenate-string
         (append (list "(")
                 (iter (for index :from 0 :below 3)
                       (for element :in list)
                       (collect (unless (first-iteration-p) "; "))
                       (collect (make-reference-label reference (class-of element) element))
                       (when (= index 2)
                         (collect " ...")))
                 (list ")"))))

(def function reuse-standard-object-list-inspector-reference (self)
  ;; TODO: performance killer
  (setf (target-of self)
        (mapcar (lambda (instance)
                  (reuse-standard-object-instance (class-of instance) instance))
                (target-of self))))

(def layered-method refresh-component :before ((self standard-object-list-inspector-reference))
  (reuse-standard-object-list-inspector-reference self))

(def render-xhtml :before standard-object-list-inspector-reference
  (reuse-standard-object-list-inspector-reference -self-))

;;;;;;
;;; Standard object tree reference

(def (component e) standard-object-tree-inspector-reference (reference-component)
  ())

(def method make-reference-label ((reference standard-object-tree-inspector-reference) (class standard-class) (instance standard-object))
  (concatenate-string "Tree rooted at: " (princ-to-string instance)))

;;;;;;
;;; Standard object filter reference

(def (component e) standard-object-filter-reference (reference-component)
  ())

(def method make-reference-label ((reference standard-object-filter-reference) (metaclass standard-class) (class standard-class))
  (localized-class-name class :capitalize-first-letter #t))

;;;;;;
;;; Standard object maker reference

(def (component e) standard-object-maker-reference (reference-component)
  ())

(def method make-reference-label ((reference standard-object-maker-reference) (metaclass standard-class) (class standard-class))
  (localized-class-name class :capitalize-first-letter #t))
