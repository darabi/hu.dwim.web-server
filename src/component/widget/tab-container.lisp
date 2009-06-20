;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tab page

(def (component e) tab-page ()
  ()
  (:documentation "A TAB-PAGE base class."))

(def (macro e) tab-page ((&rest args &key remote-setup &allow-other-keys) &body content)
  (remove-from-plistf args :remote-setup)
  (if remote-setup
      `(tab-page/full ,args ,@content)
      `(tab-page/basic ,args ,@content)))

;;;;;;
;;; Tab page abstract

(def (component e) tab-page/abstract (tab-page component/abstract content/abstract)
  ((selector (icon swith-to-tab-page) :type component))
  (:documentation "A TAB-PAGE with a SELECTOR."))

;;;;;;
;;; Tab page basic

(def (component e) tab-page/basic (tab-page/abstract component/basic)
  ())

(def (macro e) tab-page/basic ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'tab-page/basic ,@args :content ,(the-only-element content)))

;;;;;;
;;; Tab page full

(def (component e) tab-page/full (tab-page/basic component/full)
  ())

(def (macro e) tab-page/full ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'tab-page/full ,@args :content ,(the-only-element content)))

;;;;;;
;;; Tab page selector bar basic

(def (component e) tab-page-selector-bar/basic (command-bar/basic)
  ())

(def (macro e) tab-page-selector-bar/basic ((&rest args &key &allow-other-keys) &body commands)
  `(make-instance 'tab-page-selector-bar/basic ,@args :commands (list ,@commands)))

;;;;;;
;;; Tab container abstract

(def (component e) tab-container/abstract (component/abstract content/abstract)
  ((tab-pages :type components)
   (tab-page-selector-bar :type component)))

(def (generic e) find-default-tab-page (component)
  (:method ((self tab-container/abstract))
    (first (tab-pages-of self))))

;;;;;;
;;; Tab container basic

(def (component e) tab-container/basic (component/basic tab-container/abstract)
  ())

(def (macro e) tab-container/basic ((&rest args &key &allow-other-keys) &body tab-pages)
  `(make-instance 'tab-container/basic ,@args :content nil :tab-pages (optional-list ,@tab-pages)))

(def (macro e) tab-container ((&rest args &key &allow-other-keys) &body content)
  `(tab-container/basic ,args ,@content))

(def render-xhtml tab-container/basic
  (bind (((:read-only-slots content tab-page-selector-bar) -self-))
    <div (:class "tab-container")
         ,(render-component tab-page-selector-bar)
         ,(render-component content)>))

(def refresh-component tab-container/basic
  (bind (((:slots tab-pages tab-page-selector-bar content) -self-))
    (setf tab-page-selector-bar
          (make-instance 'tab-page-selector-bar/basic
                         :commands (mapcar [make-switch-to-tab-page-command !1 nil nil nil] tab-pages)))
    (unless content
      (setf content (find-default-tab-page -self-)))))

(def layered-method make-switch-to-tab-page-command ((component tab-page/basic) class prototype value)
  (bind ((tab-container (find-ancestor-component-with-type component 'tab-container/abstract)))
    (assert tab-container)
    (make-replace-command (delay (content-of tab-container))
                          component
                          :content (clone-component (selector-of component))
                          :ajax (ajax-of tab-container))))

(def (icon e) swith-to-tab-page)
