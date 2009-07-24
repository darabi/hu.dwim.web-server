;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/lisp-form/inspector

(def special-variable *lisp-form*)

(def special-variable *previous-source-position*)

(def (component e) t/lisp-form/inspector (inspector/basic)
  ((source-objects :type list)))

(def (macro e) t/lisp-form/inspector ((&rest args &key &allow-other-keys) &body form)
  `(make-instance 't/lisp-form/inspector ,@args :component-value ',(make-lisp-form-component-value* (the-only-element form))))

(def function make-lisp-form-component-value (form)
  (bind ((*print-case* :downcase))
    (with-output-to-string (*standard-output*)
      (pprint form))))

(def function make-lisp-form-component-value* (form)
  (if (stringp form)
      form
      (make-lisp-form-component-value form)))

(def refresh-component t/lisp-form/inspector
  (bind (((:slots component-value source-objects) -self-)
         (source-text:*source-readtable* (source-text::make-source-readtable)))
    ;; TODO: take over the original syntax
    (source-text::enable-sharp-boolean-syntax)
    (setf source-objects (with-input-from-string (stream component-value)
                           (iter (for element = (source-text:source-read stream #f stream #f #f))
                                 (until (eq element stream))
                                 (collect element)
                                 (until (typep element 'source-text:source-lexical-error)))))))

(def render-component t/lisp-form/inspector
  (bind ((*lisp-form* -self-)
         (*previous-source-position* 0))
    <pre (:class "lisp-form inspector")
      ,(foreach #'render-source-object (source-objects-of -self-))>))

;;;;;;
;;; Render lisp form

(eval-always
  (def function with-quasi-quoted-xml-to-binary-emitting-form-syntax/lisp-form ()
    "Unconditionally turns off XML indent to keep original whitespaces for the XHTML pre element."
    (with-quasi-quoted-xml-to-binary-emitting-form-syntax '*xml-stream* :with-inline-emitting #t)))

(def with-macro with-render-source-object-whitespace (source-object)
  (bind ((position (source-text:source-object-position source-object)))
    `xml,(subseq (component-value-of *lisp-form*) *previous-source-position* position)
    (setf *previous-source-position* position)
    (-body-)
    (setf *previous-source-position* (+ (source-text:source-object-position source-object)
                                        (length (source-text:source-object-text source-object))))
    (values)))

;; TODO: some factoring could make this code shorter
{with-quasi-quoted-xml-to-binary-emitting-form-syntax/lisp-form
 (def layered-function render-source-object (instance)
   (:method :in xhtml-layer ((instance source-text:source-token))
     <span (:class "token") ,(render-source-object-text instance)>)

   (:method :in xhtml-layer ((instance source-text:source-semicolon-comment))
     <span (:class "comment") ,(render-source-object-text instance)>)

   (:method :in xhtml-layer ((instance source-text:source-boolean))
     <span (:class "boolean") ,(render-source-object-text instance)>)

   (:method :in xhtml-layer ((instance source-text:source-number))
     <span (:class "number") ,(render-source-object-text instance)>)

   (:method :in xhtml-layer ((instance source-text:source-character))
     <span (:class "character") ,(render-source-object-text instance)>)

   (:method :in xhtml-layer ((instance source-text:source-string))
     <span (:class "string") ,(render-source-object-text instance)>)

   (:method ((instance source-text:source-symbol))
     (render-source-symbol (source-text:source-symbol-value instance) instance))

   (:method :in xhtml-layer ((instance source-text:source-function))
     (with-render-source-object-whitespace instance
       (incf *previous-source-position* 2)
       <span (:class "function") "#" ,(princ-to-string (source-text:dispatch-macro-sub-character instance))>
       (render-source-object (source-text:source-object-subform instance))))

   (:method :in xhtml-layer ((instance source-text:source-quote))
     (with-render-source-object-whitespace instance
       (incf *previous-source-position*)
       <span (:class "quote") ,(princ-to-string (source-text:macro-character instance))>
       (render-source-object (source-text:source-object-subform instance))))

   (:method :in xhtml-layer ((instance source-text:source-backquote))
     (with-render-source-object-whitespace instance
       (incf *previous-source-position*)
       <span (:class "backquote") ,(princ-to-string (source-text:macro-character instance))>
       (render-source-object (source-text:source-object-subform instance))))

   (:method :in xhtml-layer ((instance source-text:source-unquote))
     (with-render-source-object-whitespace instance
       (incf *previous-source-position*)
       <span (:class "unquote") ,(princ-to-string (source-text:macro-character instance))>
       (render-source-object (source-text:source-object-subform instance))))

   (:method :in xhtml-layer ((instance source-text:source-splice))
     (with-render-source-object-whitespace instance
       (incf *previous-source-position*)
       <span (:class "unquote-splicing") ,(princ-to-string (source-text:macro-character instance))>
       (render-source-object (source-text:source-object-subform instance))))

   (:method :in xhtml-layer ((instance source-text:source-feature))
     (with-render-source-object-whitespace instance
       (incf *previous-source-position*)
       <span (:class "feature") ,(princ-to-string (source-text:dispatch-macro-sub-character instance))>
       (render-source-object (source-text:source-object-subform instance))))
   
   (:method :in xhtml-layer ((instance source-text:source-read-eval))
     (with-render-source-object-whitespace instance
       (incf *previous-source-position*)
       <span (:class "read-eval") ,(princ-to-string (source-text:dispatch-macro-sub-character instance))>
       (render-source-object (source-text:source-object-subform instance))))

   (:method ((instance source-text:source-list))
     (render-source-list (first (source-text:source-sequence-elements instance)) instance))

   (:method :in xhtml-layer ((instance source-text:source-vector))
     (with-render-source-object-whitespace instance
       (incf *previous-source-position* 2)
       <span (:class "vector") "#(" ,(foreach #'render-source-object (source-text:source-sequence-elements instance)) ")">))

   (:method ((instance source-text:source-lexical-error))
     <span (:class "lexical-error") ,(princ-to-string (source-text:source-lexical-error-error instance))>
     (render-source-object-text instance)))}

{with-quasi-quoted-xml-to-binary-emitting-form-syntax/lisp-form
 (def layered-function render-source-list (first instance)
   (:method :in xhtml-layer (first (instance source-text:source-list))
     (with-render-source-object-whitespace instance
       (incf *previous-source-position*)
       <span (:class "list") "(" ,(foreach #'render-source-object (source-text:source-sequence-elements instance)) ")">))

   (:method ((first source-text:source-symbol) (instance source-text:source-list))
     (render-source-list (source-text:source-symbol-value first) instance))

   (:method :in xhtml-layer ((first (eql 'def)) (instance source-text:source-list))
     (with-render-source-object-whitespace instance
       (incf *previous-source-position*)
       (bind ((elements (source-text:source-sequence-elements instance)))
         <span (:class "definition")
               "("
               <span (:class "def") ,(render-source-object-text (first elements))>
               <span (:class "kind") ,(render-source-object-text (second elements))>
               <span (:class "name") ,(render-source-object-text (third elements))>
               ,(foreach #'render-source-object (cdddr (source-text:source-sequence-elements instance)))
               ")">))))}

{with-quasi-quoted-xml-to-binary-emitting-form-syntax/lisp-form
 (def layered-function render-source-symbol (value instance)
   (:method :in xhtml-layer (value (instance source-text:source-symbol))
     (bind ((id (generate-response-unique-string))
            (style-class (concatenate-string (cond ((keywordp value)
                                                    "keyword")
                                                   ((member value '(&optional &rest &allow-other-keys &key &aux &whole &body &environment))
                                                    "lambda-list-keyword")
                                                   ((member value '(if let let* progn prog1 block return-from tagbody go throw catch flet labels))
                                                    "special-form")
                                                   ((eq (symbol-package value) #.(find-package :common-lisp))
                                                    "common-lisp")
                                                   (t nil))
                                             " symbol")))
       <span (:id ,id :class ,style-class)
         ,(render-source-object-text instance)>
       (awhen (ignore-errors (fdefinition value))
         (render-tooltip (make-action
                           (make-component-rendering-response
                            (tooltip/widget ()
                              (etypecase it
                                (standard-generic-function
                                 (make-instance 'standard-method-sequence/lisp-form-list/inspector :component-value (generic-function-methods it)))
                                (function
                                 (make-instance 'function/lisp-form/inspector :component-value it))))))
                         id)))))}

(def function render-source-object-text (source-object)
  (with-render-source-object-whitespace source-object
    `xml,(source-text:source-object-text source-object)))

;;;;;;
;;; t/lisp-form/invoker

(def (component e) t/lisp-form/invoker (t/lisp-form/inspector frame-unique-id/mixin commands/mixin)
  ((evaluation-mode :single :type (member :single :multiple))
   (result (empty/layout) :type component)))

(def (macro e) t/lisp-form/invoker ((&rest args &key &allow-other-keys) &body form)
  `(make-instance 't/lisp-form/invoker ,@args :component-value ',(make-lisp-form-component-value* (the-only-element form))))

(def render-xhtml t/lisp-form/invoker
  <div (:class "lisp-form invoker")
    ,(call-next-method)
    ,(render-command-bar-for -self-)
    <div (:class "result") ,(render-component (result-of -self-))>>)

(def (icon e) evaluate-form)

(def layered-method make-command-bar-commands ((component t/lisp-form/invoker) class prototype value)
  (optional-list* (make-evaluate-form-command component class prototype value) (call-next-method)))

(def layered-method make-evaluate-form-command ((component t/lisp-form/invoker) class prototype value)
  (command/widget (:visible (delay (or (eq :multiple (evaluation-mode-of component))
                                       (empty-layout? (result-of component))))
                   :ajax (ajax-of component))
    (icon evaluate-form)
    (make-component-action component
      (setf (result-of component)
            (handler-case (make-value-inspector (evaluate-form component class prototype value))
              (error (e)
                (make-value-inspector e)))))))

(def (layered-function e) evaluate-form (component class prototype value)
  (:method ((component t/lisp-form/invoker) class prototype value)
    (eval (read-from-string (component-value-of component)))))

;;;;;;
;;; Read source

#+sbcl
(def function read-definition-lisp-source (definition)
  ;; TODO: use swank
  ;; TODO: find a portable way
  ;; TODO: bind *package*, etc.
  ;; TODO: cache result
  (bind ((definition-source (sb-introspect:find-definition-source definition))
         (pathname (sb-introspect:definition-source-pathname definition-source)))
    (if pathname
        (with-input-from-file (stream (translate-logical-pathname pathname))
          ;; TODO: handle form path? (not needed with swank)
          (iter (with index = (first (aprog1 (sb-introspect:definition-source-form-path definition-source)
                                       ;; FIXME:
                                       (unless (length= 1 it)
                                         (return-from read-definition-lisp-source
                                           (format nil ";; form path ~A in source file is too complex for ~A" it definition))))))
                (for element = (source-text:source-read stream #f stream #f #t))
                (until (eq element stream))
                ;; TODO: breaks on #t, #f, <> syntax, etc.
                (when (typep element 'source-text:source-lexical-error)
                  (return (format nil ";; source file cannot be read for ~A due to ~A" definition element)))
                (if (typep element 'source-text:comment)
                    (collect element :into comments)
                    (progn
                      (when (zerop index)
                        (return (concatenate-string (reduce #'concatenate-string (mapcar #'source-text:source-object-text comments))
                                                    (source-text:source-object-text element))))
                      (decf index)
                      (setf comments nil)))
                (finally
                 (return (format nil ";; source file not found for ~A in ~A" definition pathname)))))
        (format nil ";; no source file path for ~A" definition))))
