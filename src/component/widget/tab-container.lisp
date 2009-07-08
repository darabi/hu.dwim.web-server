;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tab page widget

(def (component e) tab-page/widget (widget/basic content/abstract)
  ((selector (icon switch-to-tab-page) :type component))
  (:documentation "A TAB-PAGE/WIDGET has a CONTENT and a SELECTOR."))

(def (macro e) tab-page/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'tab-page/widget ,@args :content ,(the-only-element content)))

(def render-component tab-page/widget
  (render-content-for -self-))

;;;;;;
;;; Tab page selector bar widget

(def (component e) tab-page-selector-bar/widget (command-bar/widget)
  ()
  (:documentation "A TAB-PAGE-SELECTOR-BAR/WIDGET is a special COMMAND-BAR/WIDGET that allows the user to select between TAB-PAGE/WIDGETs of a TAB-CONTAINER/WIDGET."))

(def (macro e) tab-page-selector-bar/widget ((&rest args &key &allow-other-keys) &body commands)
  `(make-instance 'tab-page-selector-bar/widget ,@args :commands (list ,@commands)))

;;;;;;
;;; Tab container widget

(def (component e) tab-container/widget (widget/basic content/abstract)
  ((tab-pages :type components)
   (tab-page-selector-bar :type component))
  (:documentation "A TAB-CONTAINER/WIDGET allows the user to select between its TAB-PAGE/WIDGETs."))

(def (macro e) tab-container/widget ((&rest args &key &allow-other-keys) &body tab-pages)
  `(make-instance 'tab-container/widget ,@args :content nil :tab-pages (optional-list ,@tab-pages)))

(def render-xhtml tab-container/widget
  (bind (((:read-only-slots content tab-page-selector-bar) -self-))
    <div (:class "tab-container")
      ,(render-component tab-page-selector-bar)
      ,(render-component content)>))

(def refresh-component tab-container/widget
  (bind (((:slots tab-pages tab-page-selector-bar content) -self-))
    (setf tab-page-selector-bar
          (make-instance 'tab-page-selector-bar/widget
                         :commands (mapcar [make-switch-to-tab-page-command !1 nil nil nil] tab-pages)))
    (unless content
      (setf content (find-default-tab-page -self-)))))

(def (generic e) find-default-tab-page (component)
  (:method ((self tab-container/widget))
    (first (tab-pages-of self))))

(def (icon e) switch-to-tab-page)

(def layered-method make-switch-to-tab-page-command ((component tab-page/widget) class prototype value)
  (bind ((tab-container (find-ancestor-component-with-type component 'tab-container/widget)))
    (assert tab-container)
    (make-replace-command (delay (content-of tab-container))
                          component
                          :content (clone-component (selector-of component))
                          :ajax (ajax-of tab-container))))
