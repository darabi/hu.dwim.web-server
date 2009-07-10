;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Page size selector widget

;; TODO: revive
(def (component e) page-size-selector/widget (widget/basic member/inspector)
  ()
  (:default-initargs
   :edited #t
   :possible-values '(10 20 50 100)
   :client-name-generator [concatenate-string (integer-to-string !2) #"page-size-selector.rows/page"]))

(def refresh-component page-size-selector/widget
  (setf (page-size-of (parent-component-of -self-)) (component-value-of -self-)))

;;;;;;
;;; Page navigation bar widget

;; TODO: clickable pages: first, 4, 5, previous, (jumper 7), next, 9, 10, last
(def (component e) page-navigation-bar/widget (widget/basic)
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
  (bind (((:read-only-slots first-command previous-command next-command last-command jumper page-size-selector) -self-))
    <span (:class "page-navigation-bar")
      ;; TODO: revive page-size-selector (does not work with ajax)
      ,(foreach #'render-component (list first-command previous-command jumper #+nil page-size-selector next-command last-command))>))

;;;;;;
;;; Icon

(def (icon e) go-to-first-page)

(def (icon e) go-to-previous-page)

(def (icon e) go-to-next-page)

(def (icon e) go-to-last-page)

;;;;;;
;;; Command factory

(def layered-method make-go-to-first-page-command ((component page-navigation-bar/widget))
  (bind (((:slots position page-size total-count jumper) component))
    (command/widget (:enabled (delay (> position 0))
                     :ajax (ajax-of (parent-component-of component)))
      (icon go-to-first-page)
      (make-action
        (setf (component-value-of jumper) (setf position 0))))))

(def layered-method make-go-to-previous-page-command ((component page-navigation-bar/widget))
  (bind (((:slots position page-size total-count jumper) component))
    (command/widget (:enabled (delay (> position 0))
                     :ajax (ajax-of (parent-component-of component)))
      (icon go-to-previous-page)
      (make-action
        (setf (component-value-of jumper) (decf position (min position page-size)))))))

(def layered-method make-go-to-next-page-command ((component page-navigation-bar/widget))
  (bind (((:slots position page-size total-count jumper) component))
    (command/widget (:enabled (delay (< position (- total-count page-size)))
                     :ajax (ajax-of (parent-component-of component)))
      (icon go-to-next-page)
      (make-action
        (setf (component-value-of jumper) (incf position (min page-size (- total-count page-size))))))))

(def layered-method make-go-to-last-page-command ((component page-navigation-bar/widget))
  (bind (((:slots position page-size total-count jumper) component))
    (command/widget (:enabled (delay (< position (- total-count page-size)))
                     :ajax (ajax-of (parent-component-of component)))
      (icon go-to-last-page)
      (make-action
        (setf (component-value-of jumper) (setf position (- total-count page-size)))))))
