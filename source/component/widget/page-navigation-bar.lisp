;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; page-size-selector/widget

;; TODO: revive
(def (component e) page-size-selector/widget (widget/basic member/inspector)
  ()
  (:default-initargs
   :edited #t
   :possible-values '(10 20 50 100)
   :client-name-generator [string+ (integer-to-string !2) #"page-size-selector.rows/page"]))

(def refresh-component page-size-selector/widget
  (setf (page-size-of (parent-component-of -self-)) (component-value-of -self-)))

;;;;;;
;;; page-navigation-bar/widget

;; TODO: clickable pages: first, 4, 5, previous, (jumper 7), next, 9, 10, last
(def (component e) page-navigation-bar/widget (widget/style)
  ((position 0 :type integer)
   (total-count :type integer)
   (first-command :type component)
   (previous-command :type component)
   (next-command :type component)
   (last-command :type component)
   (jumper :type component)
   (page-size 10 :type integer)
   (page-size-selector :type component))
  (:documentation "A COMPONENT to navigate through a series of pages."))

(def (macro e) page-navigation-bar/widget (&rest args &key &allow-other-keys)
  `(make-instance 'page-navigation-bar/widget ,@args))

(def refresh-component page-navigation-bar/widget
  (bind (((:slots position first-command previous-command next-command last-command jumper page-size page-size-selector) -self-))
    (setf first-command (make-go-to-first-page-command -self-)
          previous-command (make-go-to-previous-page-command -self-)
          next-command (make-go-to-next-page-command -self-) 
          last-command (make-go-to-last-page-command -self-)
          jumper (make-instance 'integer/inspector :edited #t :component-value position)
          page-size-selector (make-instance 'page-size-selector/widget :component-value page-size))))

(def render-xhtml page-navigation-bar/widget
  (bind (((:read-only-slots total-count first-command previous-command next-command last-command jumper page-size page-size-selector) -self-))
    (when (< page-size total-count)
      (with-render-style/abstract (-self-)
        ;; TODO: revive page-size-selector (does not work with ajax)
        (foreach #'render-component (list first-command previous-command jumper #+nil page-size-selector next-command last-command))))))

(def layered-method render-component :in passive-layer ((self page-navigation-bar/widget))
  (values))

(def function make-page-navigation-contents (component sequence)
  (bind (((:read-only-slots position page-size total-count) component))
    (subseq sequence position (min total-count (+ position page-size)))))

;;;;;;
;;; Icon

(def (icon e) go-to-first-page)

(def (icon e) go-to-previous-page)

(def (icon e) go-to-next-page)

(def (icon e) go-to-last-page)

;;;;;;
;;; Command factory

(def layered-method make-go-to-first-page-command ((component page-navigation-bar/widget))
  (bind (((:slots parent-component position page-size total-count jumper) component))
    (command/widget (:enabled (delay (> position 0))
                     :ajax (ajax-of parent-component))
      (icon go-to-first-page)
      (make-action
        (setf (component-value-of jumper) (setf position 0))
        (mark-to-be-rendered-component parent-component)))))

(def layered-method make-go-to-previous-page-command ((component page-navigation-bar/widget))
  (bind (((:slots parent-component position page-size total-count jumper) component))
    (command/widget (:enabled (delay (> position 0))
                     :ajax (ajax-of parent-component))
      (icon go-to-previous-page)
      (make-action
        (setf (component-value-of jumper) (decf position (min position page-size)))
        (mark-to-be-rendered-component parent-component)))))

(def layered-method make-go-to-next-page-command ((component page-navigation-bar/widget))
  (bind (((:slots parent-component position page-size total-count jumper) component))
    (command/widget (:enabled (delay (< position (- total-count page-size)))
                     :ajax (ajax-of parent-component))
      (icon go-to-next-page)
      (make-action
        (setf (component-value-of jumper) (incf position (min page-size (- total-count page-size))))
        (mark-to-be-rendered-component parent-component)))))

(def layered-method make-go-to-last-page-command ((component page-navigation-bar/widget))
  (bind (((:slots parent-component position page-size total-count jumper) component))
    (command/widget (:enabled (delay (< position (- total-count page-size)))
                     :ajax (ajax-of parent-component))
      (icon go-to-last-page)
      (make-action
        (setf (component-value-of jumper) (setf position (- total-count page-size)))
        (mark-to-be-rendered-component parent-component)))))
