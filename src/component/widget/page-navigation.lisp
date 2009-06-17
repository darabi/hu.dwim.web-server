;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Page size mixin

(def (component ea) page-size/mixin ()
  ((page-size 10 :type integer)))

;;;;;;
;;; Page size selector

(def (component ea) page-size/selector (member/inspector)
  ()
  (:default-initargs
   :edited #t
   :possible-values '(10 20 50 100)
   :client-name-generator [concatenate-string (integer-to-string !2) #"page-size/selector.rows/page"]))

(def refresh page-size/selector
  (setf (page-size-of (parent-component-of -self-)) (component-value-of -self-)))

;;;;;;
;;; Page navigation bar mixin

(def (component ea) page-navigation-bar/mixin ()
  ((page-navigation-bar :type component))
  (:documentation "A component with a page navigation bar."))

;;;;;;
;;; Page navigation bar basic

;; TODO: clickable pages: ... 4, 5, 6, (jumper 7), 8, 9, 10 ...
(def (component ea) page-navigation-bar/basic (page-size/mixin)
  ((position 0 :type integer)
   (total-count :type integer)
   (first-command :type component)
   (previous-command :type component)
   (next-command :type component)
   (last-command :type component)
   (jumper :type component)
   (page-size-selector :type component))
  (:documentation "A component to navigate through a series of pages."))

(def (macro e) page-navigation-bar/basic (&rest args &key &allow-other-keys)
  `(make-instance 'page-navigation-bar/basic ,@args))

(def refresh page-navigation-bar/basic
  (bind (((:slots position first-command previous-command next-command last-command jumper page-size page-size-selector) -self-))
    (setf first-command (make-go-to-first-page-command -self-)
          previous-command (make-go-to-previous-page-command -self-)
          next-command (make-go-to-next-page-command -self-) 
          last-command (make-go-to-last-page-command -self-)
          jumper (make-instance 'integer-inspector :edited #t :component-value position)
          page-size-selector (make-instance 'page-size/selector :component-value page-size))))

(def render-xhtml page-navigation-bar/basic
  (bind (((:read-only-slots first-command previous-command next-command last-command jumper page-size) -self-))
    ;; FIXME: the select field rendered for page-count does not work by some fucking dojo reason
    (render-horizontal-list (list first-command previous-command jumper #+nil page-size-selector next-command last-command))))

;;;;;;
;;; Icon

(def icon go-to-first-page)

(def icon go-to-previous-page)

(def icon go-to-next-page)

(def icon go-to-last-page)

;;;;;;
;;; Command factory

(def (layered-function e) make-go-to-first-page-command (component)
  (:method ((component page-navigation-bar/basic))
    (bind (((:slots position page-size total-count jumper) component))
      (command (:enabled (delay (> position 0))
                :ajax (ajax-id (parent-component-of component)))
        (icon go-to-first)
        (make-action
          (setf (component-value-of jumper) (setf position 0)))))))

(def (layered-function e) make-go-to-previous-page-command (component)
  (:method ((component page-navigation-bar/basic))
    (bind (((:slots position page-size total-count jumper) component))
      (command (:enabled (delay (> position 0))
               :ajax (ajax-id (parent-component-of component)))
        (icon previous)
        (make-action
          (setf (component-value-of jumper) (decf position (min position page-size))))))))

(def (layered-function e) make-go-to-next-page-command (component)
  (:method ((component page-navigation-bar/basic))
    (bind (((:slots position page-size total-count jumper) component))
      (command (:enabled (delay (< position (- total-count page-size)))
                :ajax (ajax-id (parent-component-of component)))
        (icon next)
        (make-action
          (setf (component-value-of jumper) (incf position (min page-size (- total-count page-size)))))))))

(def (layered-function e) make-go-to-last-page-command (component)
  (:method ((component page-navigation-bar/basic))
    (bind (((:slots position page-size total-count jumper) component))
      (command (:enabled (delay (< position (- total-count page-size)))
                :ajax (ajax-id (parent-component-of component)))
        (icon last)
        (make-action
          (setf (component-value-of jumper) (setf position (- total-count page-size))))))))

;;;;;;
;;; Localization

(def resources hu
  (icon-label.go-to-first-page "Első")
  (icon-tooltip.go-to-first-page "Ugrás az első lapra")

  (icon-label.go-to-previous-page "Előző")
  (icon-tooltip.go-to-previous-page "Lapozás a előző lapra")

  (icon-label.go-to-next-page "Következő")
  (icon-tooltip.go-to-next-page "Lapozás a következő lapra")

  (icon-label.go-to-last-page "Utolsó")
  (icon-tooltip.go-to-last-page "Ugrás az utolsó lapra")

  (page-size/selector.rows/page " sor/oldal"))

(def resources en
  (icon-label.go-to-first-page "First")
  (icon-tooltip.go-to-first-page "Jump to first page")

  (icon-label.go-to-previous-page "Previous")
  (icon-tooltip.go-to-previous-page "Move to previous page")

  (icon-label.go-to-next-page "Next")
  (icon-tooltip.go-to-next-page "Move to next page")

  (icon-label.go-to-last-page "Last")
  (icon-tooltip.go-to-last-page "Jump to last page")

  (page-size/selector.rows/page " rows/page"))
