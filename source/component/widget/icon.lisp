;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Icons cache

;; TODO use strings as icon names instead of symbols
(def special-variable *icons* (make-hash-table))

(def (function e) find-icon (name &key (otherwise :error otherwise?))
  (or (gethash name *icons*)
      (handle-otherwise (error "The icon ~S cannot be found" name))))

(def function (setf find-icon) (icon name)
  (setf (gethash name *icons*) icon))

;;;;;;
;;; icon/widget

(def (component e) icon/widget (widget/abstract style/mixin tooltip/mixin)
  ((name :type symbol)
   (label :type (or null component))
   (image-path nil :type (or null string))))

(def (macro e) icon/widget (name &rest args)
  `(make-icon/widget ',name ,@args))

(def (function e) make-icon/widget (name &rest args)
  (bind ((icon (find-icon name :otherwise nil)))
    (if icon
        (if args
            (apply #'make-instance 'icon/widget
                   :name name (append args
                                      (list :label (label-of icon)
                                            :image-path (image-path-of icon)
                                            :tooltip (tooltip-of icon))))
            icon)
        (if args
            (apply #'make-instance 'icon/widget :name name args)
            (error "The icon ~A cannot be found and no arguments were specified" name)))))

(def method supports-debug-component-hierarchy? ((self icon/widget))
  #f)

(def method clone-component ((self icon/widget))
  self)

(def render-component icon/widget
  (awhen (force (label-of -self-))
    (render-component it)))

(def render-xhtml icon/widget
  (render-icon :icon -self-))

(def layered-function render-icon-label (icon label)
  (:method (icon label)
    `xml,label))

(def (function e) render-icon (&key icon (name nil name?) (label nil label?) (image-path nil image-path?) (tooltip nil tooltip?) (style-class nil style-class?))
  (when (and icon
             (not (stringp icon)))
    (unless name?
      (setf name (name-of icon)))
    (unless label?
      (setf label (label-of icon)))
    (unless image-path?
      (setf image-path (image-path-of icon)))
    (unless tooltip?
      (setf tooltip (tooltip-of icon)))
    (unless style-class?
      (setf style-class (style-class-of icon))))
  (bind ((tooltip (force tooltip))
         (id (generate-unique-component-id)))
    ;; render the `js first, so the return value contract of qq is kept.
    (when tooltip
      (render-tooltip tooltip id))
    ;; NOTE: this preserve-whitespace is needed from chrome when there's no label and no image and the icon is setup from css
    {with-quasi-quoted-xml-to-binary-emitting-form-syntax/preserve-whitespace
      <span (:id ,id :class ,style-class)
        ,(when image-path
          <img (:src ,(string+ (path-prefix-of *application*) image-path))>)
        ,(awhen (force label)
          (render-icon-label icon it))>}))

(def function icon-style-class (component)
  (string+ "icon " (string-downcase (symbol-name (name-of component)))))

(def method component-style-class ((self icon/widget))
  (string+ "widget " (icon-style-class self)))

(def method command-position ((self icon/widget))
  ;; TODO: can't we make it faster/better (what about a generic method or something?)
  (or (position (name-of self)
                ;; TODO: this name thingie is quite fragile
                '(answer navigate-back focus-out open-in-new-frame focus-in collapse-component collapse-to-reference refresh-component begin-editing save-editing cancel-editing store-editing revert-editing new-instance delete-instance))
      most-positive-fixnum))

;;;;;;
;;; Definer

(def (macro e) icon (name &rest args)
  `(icon/widget ,name ,@args))

(def (definer e :available-flags "e") icon (name &key image-path (label nil label-p) (tooltip nil tooltip-p))
  (bind ((name-as-string (string-downcase name)))
    `(progn
       (setf (find-icon ',name)
             (make-instance 'icon/widget
                            :name ',name
                            :image-path ,image-path
                            :label ,(if label-p
                                        label
                                        `(delay (lookup-resource ,(string+ "icon-label." name-as-string))))
                            :tooltip ,(if tooltip-p
                                          tooltip
                                          `(delay (lookup-resource ,(string+ "icon-tooltip." name-as-string))))))
       ,@(when (getf -options- :export)
               `((export ',name))))))
