;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; List

(def function render-vertical-list (elements)
  <table
      ,@(mapcar (lambda (element)
                  <tr
                   <td ,(render element)>>)
                elements)>)

(def function render-horizontal-list (elements)
  <table
      <tr
       ,@(mapcar (lambda (element)
                   <td ,(render element)>)
                 elements)>>)

(def component list-component ()
  ((orientation :vertical :type (member :vertical :horizontal))
   (elements)
   (components :type components)))

(def constructor list-component ()
  (with-slots (elements components) self
    (when (slot-boundp self 'elements)
      (setf components (mapcar #'make-viewer-component elements)))))

(def render list-component ()
  (with-slots (orientation components) self
    (if (eq orientation :vertical)
        (render-vertical-list components)
        (render-horizontal-list components))))

(def component horizontal-list-component (list-component)
  ()
  (:default-initargs :orientation :horizontal))

(def component vertical-list-component (list-component)
  ()
  (:default-initargs :orientation :vertical))
