;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Place component

(def component place-component (content-component)
  ((command-bar nil :type component)))

(def generic place-component-value-of (component)
  (:method ((self component))
    (component-value-of self)))

(def generic (setf place-component-value-of) (new-value component)
  (:method (new-value (self component))
    (setf (component-value-of self) new-value)))

(def generic make-place-component-content (place-component))

(def generic make-place-component-command-bar (place-component)
  (:method ((self place-component))
    nil))

(def method refresh-component ((self place-component))
  (setf (content-of self) (make-place-component-content self)
        (command-bar-of self) (make-place-component-command-bar self)))

(def render place-component ()
  (with-slots (content command-bar) -self-
    (if command-bar
        (render-vertical-list (list command-bar content))
        (call-next-method))))

(def (function e) make-set-place-to-nil-command (place-component)
  (command (icon set-to-nil)
           (make-action
             (setf (component-value-of (content-of place-component)) nil))
           :visible (delay (bind ((content (content-of place-component)))
                             (and (typep content 'inspector-component)
                                  (not (null (component-value-of content))))))))

(def (function e) make-set-place-to-unbound-command (place-component)
  (command (icon set-to-unbound)
           (make-action
             (when-bind command-bar (find-command-bar (content-of place-component))
               (when-bind command (find-command-bar-command command-bar 'back)
                 (execute-command command)))
             (setf (content-of place-component) (make-instance 'unbound-component)))
           :visible (delay (not (typep (content-of place-component) 'unbound-component)))))

(def (function e) make-set-place-to-new-instance-command (place-component)
  (make-replace-and-push-back-command (delay (content-of place-component))
                                      (make-maker (the-type-of place-component))
                                      (list :icon (icon new) )
                                      (list :icon (icon back))))

(def layer* set-place-to-find-instance-layer ()
  ;; TODO: KLUDGE: FIXME: make this a special slot (this way it is not thread safe)
  ((place-component :type component)))

(def layered-method make-standard-object-row-inspector-commands :in-layer set-place-to-find-instance-layer :around
     ((component standard-object-row-inspector) (class standard-class) (instance standard-object))
     ;; TODO: check for being the row of the result component of the filter in the place component
  (bind ((place-component
          (parent-component-of (find-ancestor-component-with-type component 'standard-object-filter))
          #+nil
          (place-component-of (current-layer))))
    (list* (command (icon select)
                    (make-action
                      (execute-command-bar-command (command-bar-of (find-ancestor-component-with-type component 'standard-object-filter)) 'back)
                      (setf (content-of place-component) (make-viewer (instance-of component)))))
           (call-next-method))))

(def (function e) make-set-place-to-find-instance-command (place-component)
  (make-replace-and-push-back-command (delay (content-of place-component))
                                      (with-active-layers ((set-place-to-find-instance-layer :place-component place-component))
                                        (make-filter (the-type-of place-component)))
                                      (list :icon (icon find))
                                      (list :icon (icon back))))

;;;;;;
;;; Place inspector

(def component place-inspector (place-component inspector-component editable-component)
  ((place nil :type place)))

(def render place-inspector ()
  (with-slots (edited content command-bar) -self-
    (if (and edited
             command-bar)
        (render-vertical-list (list command-bar content))
        (render content))))

(def method the-type-of ((self place-inspector))
  (place-type (place-of self)))

(def method make-place-component-content ((self place-inspector))
  (with-slots (place edited content command-bar) self
    (if content
        (progn
          (unless (edited-p content)
            (revert-place-inspector-content self))
          content)
        (make-place-inspector-content self))))

(def method (setf place-of) :after (new-value (self place-inspector))
  (setf (outdated-p self) #t))

(def function update-component-value-from-place (place component)
  (when (place-bound-p place)
    (bind ((value (value-at-place place)))
      (setf (component-value-of component) value))))

(def function make-place-inspector-content (component)
  (bind ((place (place-of component)))
    (prog1-bind content
        (make-inspector (place-type place) :default-component-type 'reference-component)
      (update-component-value-from-place place content))))

(def method map-editable-child-components ((self place-inspector) function)
  (bind ((content (content-of self)))
    (when (and (place-editable-p (place-of self))
               (typep content 'editable-component))
      (funcall function content))))

(def function revert-place-inspector-content (place-inspector)
  (update-component-value-from-place (place-of place-inspector) (content-of place-inspector)))

(def method revert-editing :after ((place-inspector place-inspector))
  (revert-place-inspector-content place-inspector))

(def method store-editing :after ((place-inspector place-inspector))
  (setf (value-at-place (place-of place-inspector)) (place-component-value-of (content-of place-inspector))))

(def (function e) make-revert-place-command (place-inspector)
  (command (icon revert)
           (make-action (revert-place-inspector-content place-inspector))))

;;;;;;
;;; Place maker

(def component place-maker (place-component maker-component)
  ((name nil)
   (the-type nil)
   (initform)))

(def method make-place-component-content ((self place-maker))
  (make-maker (the-type-of self)))

;;;;;;
;;; Place filter

(def component place-filter (place-component filter-component)
  ((name nil)
   (the-type)
   (use-in-filter #f :type boolean)
   (use-in-filter-id)
   (negated #f :type boolean)
   (selected-predicate nil :type symbol)))

(def method make-place-component-content ((self place-filter))
  (make-filter (the-type-of self)))

(def method use-in-filter-p ((self component))
  (use-in-filter-p (parent-component-of self)))

(def method use-in-filter-id-of ((self component))
  (use-in-filter-id-of (parent-component-of self)))

(def generic collect-possible-filter-predicates (component)
  (:method ((self primitive-filter))
    nil))

(def function localize-predicate (predicate)
  (lookup-resource (concatenate-string "predicate." (symbol-name predicate)) nil))

(def function predicate-class (predicate)
  (ecase predicate
    (= "predicate-equal")
    (~ "predicate-like")
    (< "predicate-less-than")
    (≤ "predicate-less-than-or-equal")
    (> "predicate-greater-than")
    (≥ "predicate-greater-than-or-equal")))

(def generic predicate-function (component class predicate)
  (:method ((component place-filter) (class standard-class) predicate)
    (predicate-function (content-of component) class predicate))

  (:method ((component component) (class standard-class) (predicate null))
    'equal)

  (:method ((component component) (class standard-class) (predicate (eql '=)))
    'equal)

  (:method ((component string-component) (class standard-class) (predicate (eql '~)))
    (lambda (string-1 string-2)
      (cl-ppcre:all-matches string-2 (concatenate-string ".*" string-1 ".*"))))

  (:method ((component string-component) (class standard-class) (predicate (eql '<)))
    'string<)

  (:method ((component string-component) (class standard-class) (predicate (eql '≤)))
    'string<=)

  (:method ((component string-component) (class standard-class) (predicate (eql '>)))
    'string>)

  (:method ((component string-component) (class standard-class) (predicate (eql '≥)))
    'string>=)

  (:method ((component number-component) (class standard-class) (predicate (eql '<)))
    '<)

  (:method ((component number-component) (class standard-class) (predicate (eql '≤)))
    '<=)

  (:method ((component number-component) (class standard-class) (predicate (eql '>)))
    '>)

  (:method ((component number-component) (class standard-class) (predicate (eql '≥)))
    '>=))

(def method collect-possible-filter-predicates ((self place-filter))
  (collect-possible-filter-predicates (content-of self)))

(def function render-filter-predicate (self)
  (bind (((:slots negated selected-predicate) self)
         (possible-predicates (collect-possible-filter-predicates self)))
    (if possible-predicates
        (progn
          (unless selected-predicate
            (setf selected-predicate (first possible-predicates)))
          <td ,(render-checkbox-field negated
                                      :value-sink (lambda (value) (setf negated value))
                                      :checked-image "static/wui/icons/20x20/thumb-down.png"
                                      :unchecked-image "static/wui/icons/20x20/thumb-up.png")>
          <td ,(if (length= 1 possible-predicates)
                   <div (:class ,(predicate-class (first possible-predicates)))>
                   (render-popup-menu-select-field (localize-predicate selected-predicate)
                                                   (mapcar #'localize-predicate possible-predicates)
                                                   :value-sink (lambda (value)
                                                                 (setf (selected-predicate-of self)
                                                                       (find value possible-predicates :key #'localize-predicate :test #'string=)))
                                                   :classes (mapcar #'predicate-class possible-predicates)))>)
        <td (:colspan 2)>)))

(def function render-use-in-filter-marker (self)
  (bind ((id (generate-frame-unique-string)))
    (setf (use-in-filter-id-of self) id)
    <td ,(render-checkbox-field (use-in-filter-p self)
                                :id id
                                :value-sink (lambda (value) (setf (use-in-filter-p self) value))
                                :checked-image "static/wui/icons/20x20/checkmark.png"
                                :unchecked-image "static/wui/icons/20x20/checkmark.png"
                                :checked-class "use-in-filter"
                                :unchecked-class "ignore-in-filter")>))

(def render place-filter ()
  (render-filter-predicate -self-)
  (render-use-in-filter-marker -self-)
  <td ,(call-next-method)>)

(defresources en
  (predicate.= "Equal")
  (predicate.~ "Like")
  (predicate.< "Smaller than")
  (predicate.≤ "Smaller than or equal")
  (predicate.> "Greater than")
  (predicate.≥ "Greater than or equal"))

(defresources hu
  (predicate.= "Egyenlő")
  (predicate.~ "Hasonló")
  (predicate.< "Kisebb")
  (predicate.≤ "Kisebb vagy egyenlő")
  (predicate.> "Nagyobb")
  (predicate.≥ "Nagyobb vagy egyenlő"))
