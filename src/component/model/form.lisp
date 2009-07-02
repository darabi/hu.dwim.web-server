;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Lisp form viewer

(def (component e) lisp-form/viewer (viewer/basic)
  ((parse-tree :type source-text:source-object)))

(def (macro e) lisp-form/viewer ((&rest args &key &allow-other-keys) &body form)
  `(make-instance 'lisp-form/viewer ,@args :component-value ',(make-lisp-form-component-value (the-only-element form))))

(def function make-lisp-form-component-value (form)
  (if (stringp form)
      form
      (bind ((*print-case* :downcase))
        (with-output-to-string (*standard-output*)
          (pprint form)))))

(def refresh-component lisp-form/viewer
  (bind (((:slots component-value parse-tree) -self-))
    (setf parse-tree
          (with-input-from-string (src component-value)
            (source-text:source-read src #f src #f #t)))))

(def render-component lisp-form/viewer
  <pre (:class "lisp-form viewer")
    ,(render-source-object (parse-tree-of -self-))>)

{(with-quasi-quoted-xml-to-binary-emitting-form-syntax '*xml-stream* :with-inline-emitting #t)
 (def layered-function render-source-object (instance)
   (:method ((instance source-text:source-list))
     (render-source-list (first (source-text:source-sequence-elements instance)) instance))

   (:method :in xhtml-layer ((instance source-text:source-string))
     <span (:class "string") ,(source-text:source-object-text instance)>)

   (:method :in xhtml-layer ((instance source-text:source-number))
     <span (:class "number") ,(source-text:source-object-text instance)>)

   (:method ((instance source-text:source-symbol))
     (render-source-symbol (source-text:source-symbol-value instance) instance))

   (:method :in xhtml-layer ((instance source-text:source-token))
     <span (:class "token") ,(source-text:source-object-text instance)>))}

{(with-quasi-quoted-xml-to-binary-emitting-form-syntax '*xml-stream* :with-inline-emitting #t)
 (def layered-function render-source-list (first instance)
   (:method :in xhtml-layer (first (instance source-text:source-list))
     <span (:class "list") "(" ,(foreach #'render-source-object (source-text:source-sequence-elements instance)) ")">)

   (:method ((first source-text:source-symbol) (instance source-text:source-list))
     (render-source-list (source-text:source-symbol-value first) instance))

   (:method :in xhtml-layer ((first (eql 'def)) (instance source-text:source-list))
     (bind ((elements (source-text:source-sequence-elements instance)))
       <span (:class "definition")
             "("
             <span (:class "def") ,(source-text:source-object-text (first elements))>
             <span (:class "kind") ,(source-text:source-object-text (second elements))>
             <span (:class "name") ,(source-text:source-object-text (third elements))>
             ,(foreach #'render-source-object (cdddr (source-text:source-sequence-elements instance)))
             ")">)))}

{(with-quasi-quoted-xml-to-binary-emitting-form-syntax '*xml-stream* :with-inline-emitting #t)
 (def layered-function render-source-symbol (value instance)
   (:method :in xhtml-layer (value (instance source-text:source-symbol))
     (bind ((style-class (cond ((member value '(&optional &rest &allow-other-keys &key &aux &whole &body &environment))
                                "lambda-list-keyword")
                               ((member value '(if let let* progn prog1 block return-from tagbody go throw catch flet labels))
                                "special-form")
                               (t "symbol"))))
      <span (:class ,style-class) ,(source-text:source-object-text instance)>)))}

;;;;;;
;;; Lisp form invoker

(def (component e) lisp-form/invoker (lisp-form/viewer frame-unique-id/mixin commands/mixin)
  ((evaluation-mode :single :type (member :single :multiple))
   (result (empty/layout) :type component)))

(def (macro e) lisp-form/invoker ((&rest args &key &allow-other-keys) &body form)
  `(make-instance 'lisp-form/invoker ,@args :component-value ',(make-lisp-form-component-value (the-only-element form))))

(def render-xhtml lisp-form/invoker
  <div (:class "lisp-form invoker")
       ,(call-next-method)
       ,(render-component (command-bar-of -self-))
       <div (:class "result") ,(render-component (result-of -self-))>>)

(def (icon e) evaluate-form)

(def layered-method make-command-bar-commands ((component lisp-form/invoker) class prototype value)
  (optional-list* (make-evaluate-form-command component class prototype value) (call-next-method)))

(def layered-method make-evaluate-form-command ((component lisp-form/invoker) class prototype value)
  (command/widget (:visible (delay (or (eq :multiple (evaluation-mode-of component))
                                       (empty-layout? (result-of component))))
                   :ajax (ajax-of component))
    (icon evaluate-form)
    (make-component-action component
      (setf (result-of component)
            (make-value-viewer (handler-case (evaluate-form component class prototype value)
                                 (error (e)
                                   (make-value-viewer e))))))))

(def (layered-function e) evaluate-form (component class prototype value)
  (:method ((component lisp-form/invoker) class prototype value)
    (eval (read-from-string (component-value-of component)))))
