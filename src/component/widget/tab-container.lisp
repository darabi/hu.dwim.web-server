;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tab page

(def (component e) tab-page ()
  ()
  (:documentation "A TAB-PAGE base class."))

(def (macro e) tab-page ((&rest args &key &allow-other-keys) &body content)
  `(tab-page/widget ,args ,@content))

;;;;;;
;;; Tab page abstract

(def (component e) tab-page/abstract (tab-page widget/abstract content/abstract)
  ((selector (icon switch-to-tab-page) :type component))
  (:documentation "A TAB-PAGE with a SELECTOR."))

(def render-component tab-page/abstract
  (render-content-for -self-))

;;;;;;
;;; Tab page widget

(def (component e) tab-page/widget (tab-page/abstract widget/basic)
  ())

(def (macro e) tab-page/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'tab-page/widget ,@args :content ,(the-only-element content)))

;;;;;;
;;; Tab page selector bar widget

(def (component e) tab-page-selector-bar/widget (command-bar/widget)
  ())

(def (macro e) tab-page-selector-bar/widget ((&rest args &key &allow-other-keys) &body commands)
  `(make-instance 'tab-page-selector-bar/widget ,@args :commands (list ,@commands)))

;;;;;;
;;; Tab container abstract

(def (component e) tab-container/abstract (content/abstract widget/abstract)
  ((tab-pages :type components)
   (tab-page-selector-bar :type component)))

(def (generic e) find-default-tab-page (component)
  (:method ((self tab-container/abstract))
    (first (tab-pages-of self))))

;;;;;;
;;; Tab container widget

(def (component e) tab-container/widget (widget/basic tab-container/abstract)
  ())

(def (macro e) tab-container/widget ((&rest args &key &allow-other-keys) &body tab-pages)
  `(make-instance 'tab-container/widget ,@args :content nil :tab-pages (optional-list ,@tab-pages)))

(def (macro e) tab-container ((&rest args &key &allow-other-keys) &body content)
  `(tab-container/widget ,args ,@content))

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

(def layered-method make-switch-to-tab-page-command ((component tab-page/widget) class prototype value)
  (bind ((tab-container (find-ancestor-component-with-type component 'tab-container/abstract)))
    (assert tab-container)
    (make-replace-command (delay (content-of tab-container))
                          component
                          :content (clone-component (selector-of component))
                          :ajax (ajax-of tab-container))))

(def (icon e) switch-to-tab-page)
