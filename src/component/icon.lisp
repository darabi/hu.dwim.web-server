;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Icon

(def component icon-component ()
  ((name)
   (label nil)
   (image-url nil)
   (tooltip nil)))

(def render icon-component ()
  (with-slots (name label image-url tooltip) self
    <span (:title ,(or (force tooltip) ""))
      ,@(when image-url (list <img (:src ,image-url)>))
      ,@(when label (list (force label)))>))

(def (macro e) make-icon-component (name &rest args)
  `(make-instance 'icon-component :name ,name ,@args))

(def definer icon (name image-url)
  (bind ((icon-name (format-symbol *package* "*~A*" name))
         (name-as-string (string-downcase name)))
    `(def special-variable ,icon-name
         (make-icon-component ',name
                              :image-url ,image-url
                              :label (delay (lookup-resource ,(format nil "icon-label.~A" name-as-string)))
                              :tooltip (delay (lookup-resource ,(format nil "icon-tooltip.~A" name-as-string)))))))

;; TODO: a list of predefined icons
#+nil
(def icon save-icon "icons/disc.gif")
