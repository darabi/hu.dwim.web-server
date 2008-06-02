;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Maker

(def component standard-object-maker-component (content-component editable-component)
  ((the-class)
   (command-bar :type component)))

(def render standard-object-maker-component ()
  (with-slots (content command-bar) -self-
    (render-vertical-list (list content command-bar))))

(def constructor standard-object-maker-component ()
  (with-slots (the-class content command-bar) -self-
    (setf content (make-instance 'standard-object-detail-component :instance (make-instance the-class :persistent #f) :edited #t) ;; TODO: need a special component?
          command-bar (make-instance 'command-bar-component :commands (list (make-new-command-component the-class))))))

(def (function e) make-new-command-component (class)
  (make-instance 'command-component
                 :icon (make-icon-component 'new :label "New" :tooltip "Create new instance")
                 :action (make-action
                           (break "TODO")
                           (make-instance class))))
