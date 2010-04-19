;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; tab-page/widget

(def (component e) tab-page/widget (widget/basic content/abstract)
  ((selector (icon/widget switch-to-tab-page) :type component))
  (:documentation "A TAB-PAGE/WIDGET has a CONTENT and a SELECTOR."))

(def (macro e) tab-page/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'tab-page/widget ,@args :content ,(the-only-element content)))

(def render-component tab-page/widget
  (render-content-for -self-))

;;;;;;
;;; tab-page-selector-bar/widget

(def (component e) tab-page-selector-bar/widget (command-bar/widget)
  ()
  (:documentation "A TAB-PAGE-SELECTOR-BAR/WIDGET is a special COMMAND-BAR/WIDGET that allows the user to select between TAB-PAGE/WIDGETs of a TAB-CONTAINER/WIDGET."))

(def (macro e) tab-page-selector-bar/widget ((&rest args &key &allow-other-keys) &body commands)
  `(make-instance 'tab-page-selector-bar/widget ,@args :commands (list ,@commands)))

;;;;;;
;;; tab-container/widget

(def (component e) tab-container/widget (widget/style content/abstract)
  ((tab-pages :type list)
   (tab-page-selector-bar :type component))
  (:documentation "A TAB-CONTAINER/WIDGET allows the user to select between its TAB-PAGE/WIDGETs."))

(def (macro e) tab-container/widget ((&rest args &key &allow-other-keys) &body tab-pages)
  `(make-instance 'tab-container/widget ,@args :content nil :tab-pages (optional-list ,@tab-pages)))

(def refresh-component tab-container/widget
  (bind (((:slots tab-pages tab-page-selector-bar content) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-class -self-))
         (component-value (component-value-of -self-)))
    (setf tab-page-selector-bar
          (make-instance 'tab-page-selector-bar/widget
                         :commands (iter (for tab-page :in tab-pages)
                                         (setf (parent-component-of tab-page) -self-)
                                         (collect (make-switch-to-tab-page-command tab-page dispatch-class dispatch-prototype component-value)))))
    (unless content
      (setf content (find-default-tab-page -self-)))))

(def render-xhtml tab-container/widget
  (bind (((:read-only-slots content tab-page-selector-bar) -self-))
    (with-render-style/abstract (-self-)
      (render-component tab-page-selector-bar)
      (render-component content))))

(def (generic e) find-default-tab-page (component)
  (:method ((self tab-container/widget))
    (first (tab-pages-of self))))

;;;;;;
;;; switch-to-tab-page/widget

(def (icon e) switch-to-tab-page)

(def (component e) switch-to-tab-page/widget (command/widget)
  ())

(def layered-method make-switch-to-tab-page-command ((component tab-page/widget) class prototype value)
  (bind ((tab-container (find-ancestor-component-of-type 'tab-container/widget component)))
    (make-instance 'switch-to-tab-page/widget
                   :action (make-action
                             (execute-replace (delay (content-of tab-container)) component))
                   :content (clone-component (selector-of component))
                   :subject-component tab-container)))
