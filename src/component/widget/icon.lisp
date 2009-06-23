;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Icons cache

(def special-variable *icons* (make-hash-table))

(def (function e) find-icon (name &key (otherwise (list :error "The icon ~A cannot be found" name)))
  (prog1-bind icon (gethash name *icons*)
    (unless icon
      (handle-otherwise otherwise))))

(def function (setf find-icon) (icon name)
  (setf (gethash name *icons*) icon))

;;;;;;
;;; Icon basic

(def (component e) icon/basic (tooltip/mixin)
  ((name :type symbol)
   (label :type (or null component))
   (image-path :type (or null string))))

(def (macro e) icon/basic (name &rest args)
  `(make-icon/basic ',name ,@args))

(def (function e) make-icon/basic (name &rest args)
  (bind ((icon (find-icon name :otherwise nil)))
    (if icon
        (if args
            (apply #'make-instance 'icon/basic
                   :name name (append args
                                      (list :label (label-of icon)
                                            :image-path (image-path-of icon)
                                            :tooltip (tooltip-of icon))))
            icon)
        (if args
            (apply #'make-instance 'icon/basic :name name args)
            (error "The icon ~A cannot be found and no arguments were specified" name)))))

(def method supports-debug-component-hierarchy? ((self icon/basic))
  #f)

(def method clone-component ((self icon/basic))
  self)

(def render-component icon/basic
  (render-component (label-of -self-)))

(def render-xhtml icon/basic
  (render-icon :icon -self-))

(def layered-function render-icon-label (icon label)
  (:method (icon label)
    `xml,label))

(def (function e) render-icon (&key icon (name nil name?) (label nil label?) (image-path nil image-path?) (tooltip nil tooltip?) (class nil class?))
  (when (and icon
             (not (stringp icon)))
    (unless name?
      (setf name (name-of icon)))
    (unless label?
      (setf label (label-of icon)))
    (unless image-path?
      (setf image-path (image-path-of icon)))
    (unless tooltip?
      (setf tooltip (tooltip-of icon))))
  (bind ((tooltip (force tooltip))
         (id (generate-response-unique-string))
         (class (if class?
                    class
                    (icon-class name))))
    ;; render the `js first, so the return value contract of qq is kept.
    (when tooltip
      (render-tooltip id tooltip))
    <span (:id ,id
           :class ,class)
      ,(when image-path
         <img (:src ,(concatenate-string (path-prefix-of *application*) image-path))>)
      ,(awhen (force label)
         (render-icon-label icon it))>))

(def function icon-class (name)
  (concatenate-string "icon " (string-downcase (symbol-name name)) "-icon"))

;;;;;;
;;; Icon

(def (macro e) icon (name &rest args)
  `(icon/basic ,name ,@args))

(def (definer e :available-flags "e") icon (name &key image-path (label nil label-p) (tooltip nil tooltip-p))
  (bind ((name-as-string (string-downcase name)))
    `(progn
       (setf (find-icon ',name)
             (make-instance 'icon/basic
                            :name ',name
                            :image-path ,image-path
                            :label ,(if label-p
                                        label
                                        `(lookup-resource ,(concatenate-string "icon-label." name-as-string)))
                            :tooltip ,(if tooltip-p
                                          tooltip
                                          `(lookup-resource ,(concatenate-string "icon-tooltip." name-as-string)))))
       ,@(when (getf -options- :export)
               `((export ',name))))))