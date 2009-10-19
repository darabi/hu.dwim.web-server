;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/lisp-form/inspector

(def special-variable *lisp-form*)

(def (component e) t/lisp-form/inspector (inspector/style)
  ((source-objects :type list)
   (line-count :type integer)))

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
  (bind (((:slots source-objects line-count component-value) -self-)
         (source-text:*source-readtable* (make-source-readtable)))
    (setf source-objects (with-input-from-string (stream component-value)
                           (iter (for element = (source-text:source-read stream #f stream #f #t))
                                 (until (eq element stream))
                                 (collect element)
                                 (until (typep element 'source-text:source-lexical-error))))
          line-count (+ (count #\Newline component-value)
                        (if (char= #\Newline (last-elt component-value))
                            0
                            1)))))

;;;;;;
;;; Render lisp form

{with-quasi-quoted-xml-to-binary-emitting-form-syntax/preserve-whitespace
  (def render-component t/lisp-form/inspector
    (bind (((:read-only-slots line-count) -self-)
           (*lisp-form* -self-))
      (with-render-style/abstract (-self-)
        <pre (:class "gutter")
             ,(iter (for line-number :from 1 :to line-count)
                    <span (:class `str("line-number " ,(element-style-class (1- line-number) line-count)))
                          ,(format nil "~3,' ',D" line-number)>
                    ;; NOTE: this has to be separate to workaround an Opera browser issue related to
                    ;;       newline handling in pre elements
                    <span (:class "new-line") ,(format nil "~%")>)>
        <pre (:class "content")
             ,(foreach #'render-source-object (source-objects-of -self-))>)))}

;; TODO: some factoring could make this code shorter
{with-quasi-quoted-xml-to-binary-emitting-form-syntax/preserve-whitespace
 (def layered-function render-source-object (instance)
   (:method :in xhtml-layer ((instance source-text:source-token))
     <span (:class "token") ,(render-source-object-text instance)>)

   (:method :in xhtml-layer ((instance source-text:source-whitespace))
     <span (:class "whitespace") ,(render-source-object-text instance)>)

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
     <span (:class "function") "#" ,(princ-to-string (source-text:dispatch-macro-sub-character instance))>
     (render-source-object (source-text:source-object-subform instance)))

   (:method :in xhtml-layer ((instance source-text:source-quote))
     <span (:class "quote") ,(princ-to-string (source-text:macro-character instance))>
     (render-source-object (source-text:source-object-subform instance)))

   (:method :in xhtml-layer ((instance source-text:source-backquote))
     <span (:class "backquote") ,(princ-to-string (source-text:macro-character instance))>
     (render-source-object (source-text:source-object-subform instance)))

   (:method :in xhtml-layer ((instance source-text:source-unquote))
     <span (:class "unquote") ,(princ-to-string (source-text:macro-character instance))>
     (render-source-object (source-text:source-object-subform instance)))

   (:method :in xhtml-layer ((instance source-text:source-splice))
     <span (:class "unquote-splicing") ,(princ-to-string (source-text:macro-character instance))>
     (render-source-object (source-text:source-object-subform instance)))

   (:method :in xhtml-layer ((instance source-text:source-feature))
     <span (:class "feature") "#" ,(princ-to-string (source-text:dispatch-macro-sub-character instance))>
     (render-source-object (source-text:source-object-subform instance)))

   (:method :in xhtml-layer ((instance source-text:source-not-feature))
     <span (:class "feature") "#" ,(princ-to-string (source-text:dispatch-macro-sub-character instance))>
     (render-source-object (source-text:source-object-subform instance)))

   (:method :in xhtml-layer ((instance source-text:source-read-eval))
     <span (:class "read-eval") "#" ,(princ-to-string (source-text:dispatch-macro-sub-character instance))>
     (render-source-object (source-text:source-object-subform instance)))

   (:method :in xhtml-layer ((instance source-text:source-bit-vector))
     <span (:class "bit-vector") "#" ,(princ-to-string (source-text:dispatch-macro-sub-character instance))>)

   (:method :in xhtml-layer ((instance source-text:source-pathname))
     <span (:class "pathname") "#" ,(princ-to-string (source-text:dispatch-macro-sub-character instance))>
     (render-source-object (source-text:source-object-subform instance)))

   (:method ((instance source-text:source-list))
     (render-source-list (first (source-text:source-sequence-elements instance)) instance))

   (:method :in xhtml-layer ((instance source-text:source-vector))
     <span (:class "vector") "#(" ,(foreach #'render-source-object (source-text:source-sequence-elements instance)) ")">)

   (:method ((instance source-text:source-lexical-error))
     <span (:class "lexical-error") ,(princ-to-string (source-text:source-lexical-error-error instance))>
     (render-source-object-text instance)))}

{with-quasi-quoted-xml-to-binary-emitting-form-syntax/preserve-whitespace
 (def layered-function render-source-list (first instance)
   (:method :in xhtml-layer (first (instance source-text:source-list))
     <span (:class "list") "(" ,(foreach #'render-source-object (source-text:source-sequence-elements instance)) ")">)

   (:method ((first source-text:source-symbol) (instance source-text:source-list))
     (render-source-list (source-text:source-symbol-value first) instance))

   (:method :in xhtml-layer ((first (eql 'def)) (instance source-text:source-list))
     (bind ((elements (source-text:source-sequence-elements instance)))
       <span (:class "definition")
             "("
             <span (:class "def") ,(render-source-object-text (pop elements))>
             ,(render-source-object (pop elements))
             <span (:class "kind") ,(render-source-object-text (pop elements))>
             ,(render-source-object (pop elements))
             <span (:class "name") ,(render-source-object-text (pop elements))>
             ,(foreach #'render-source-object elements)
             ")">)))}

{with-quasi-quoted-xml-to-binary-emitting-form-syntax/preserve-whitespace
 (def layered-function render-source-symbol (value instance)
   (:method :in xhtml-layer (value (instance source-text:source-symbol))
     (bind ((id (generate-frame-unique-string))
            (style-class (string+ (cond ((keywordp value)
                                                    "keyword ")
                                                   ((member value '(&optional &rest &allow-other-keys &key &aux &whole &body &environment))
                                                    "lambda-list-keyword ")
                                                   ((member value '(if let let* progn prog1 block return-from tagbody go throw catch flet labels))
                                                    "special-form ")
                                                   ((eq (symbol-package value) #.(find-package :common-lisp))
                                                    "common-lisp ")
                                                   (t nil))
                                             "symbol")))
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
  `xml,(source-text:source-object-text source-object))

;;;;;;
;;; t/lisp-form/invoker

(def (component e) t/lisp-form/invoker (t/lisp-form/inspector frame-unique-id/mixin commands/mixin)
  ((evaluation-mode :single :type (member :single :multiple))
   (result (empty/layout) :type component)))

(def (macro e) t/lisp-form/invoker ((&rest args &key &allow-other-keys) &body form)
  `(make-instance 't/lisp-form/invoker ,@args :component-value ',(make-lisp-form-component-value* (the-only-element form))))

(def render-xhtml t/lisp-form/invoker
  (with-render-style/abstract (-self-)
    (call-next-method)
    (render-command-bar-for -self-)
    <div (:class "result") ,(render-component (result-of -self-))>))

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

(def function read-lisp-source (pathname)
  (string-trim (coerce '(#\Newline #\Space) 'simple-string)
               (read-file-into-string pathname)))

#+sbcl
(def function read-definition-lisp-source (definition)
  ;; TODO: use swank?
  ;; TODO: find a portable way?!
  ;; TODO: bind, setf *package* to intern symbols in the right package, etc.
  ;; TODO: cache, reuse, share result
  (bind ((definition-source (sb-introspect:find-definition-source definition))
         (pathname (sb-introspect:definition-source-pathname definition-source))
         (*package* *package*)
         (source-text:*source-readtable* (make-source-readtable)))
    (values (if pathname
                ;; KLUDGE: external-format must be explicitly set, neither :default nor omitting helps even though (sb-impl::default-external-format) returns :utf-8 which is what we want
                (with-input-from-file (stream (translate-logical-pathname pathname) :external-format (sb-impl::default-external-format))
                  ;; TODO: handle form path? (probably not needed with swank)
                  (iter (with index = (first (aprog1 (sb-introspect:definition-source-form-path definition-source)
                                               ;; FIXME:
                                               (unless (length= 1 it)
                                                 (return-from read-definition-lisp-source
                                                   (format nil ";; source form path ~A in source file ~A is too complex for ~A" it pathname definition))))))
                        (for element = (source-text:source-read stream #f stream #f #t))
                        (until (eq element stream))
                        (when (typep element 'source-text:source-lexical-error)
                          (return (format nil ";; error ~A during reading ~A for ~A" element pathname definition)))
                        (when (typep element 'source-text:source-list)
                          (bind ((first-element (first (source-text:source-sequence-elements element))))
                            (when (and (typep first-element 'source-text:source-symbol)
                                       (eq 'in-package (source-text:source-symbol-value first-element)))
                              (bind ((name-element (find-if (of-type '(or source-text:source-string
                                                                          source-text:source-symbol))
                                                              (cdr (source-text:source-sequence-elements element))))
                                     (package-name (etypecase name-element
                                                     (source-text:source-string (source-text:source-string-value name-element))
                                                     (source-text:source-symbol (symbol-name (source-text:source-symbol-value name-element)))))
                                     (fixed-package-name (substitute #\- #\! package-name))
                                     (package (find-package fixed-package-name)))
                                (assert package)
                                (setf *package* package)))))
                        (if (typep element '(or source-text:comment
                                                source-text:source-whitespace))
                            (collect element :into comments)
                            (progn
                              (when (zerop index)
                                (return (string-trim (coerce '(#\Newline #\Space) 'simple-string)
                                                     (string+ (reduce #'string+ (mapcar #'source-text:source-object-text comments))
                                                                         (source-text:source-object-text element)))))
                              (decf index)
                              (setf comments nil)))
                        (finally
                         (return (format nil ";; source file ~A cannot be found for ~A" pathname definition)))))
                (format nil ";; cannot determine source file for ~A" definition))
            *package*)))

(def function make-source-readtable ()
  ;; TODO: use the original readtable (at least derive from it)
  (prog1-bind readtable (source-text::make-source-readtable)
    (source-text::enable-sharp-boolean-syntax readtable)
    (source-text::enable-shebang-syntax readtable)))

;;;;;;
;;; TODO: Move to reader? or kill this code fragment

(def special-variable *form-path->source-text*)

(def special-variable *form-path*)

(def function build-form (instance)
  (bind ((*form-path->source-text* (make-hash-table :test #'equal))
         (*form-path* nil))
    (values (build-form* instance) *form-path->source-text*)))

(def generic build-form* (instance)
  (:method :after (instance)
    (setf (gethash (reverse *form-path*) *form-path->source-text*) instance))

  (:method ((instance source-text:source-symbol))
    (source-text::source-symbol-value instance))

  (:method ((instance source-text:source-number))
    (source-text::source-number-value instance))

  (:method ((instance source-text:source-sequence))
    (iter (with index = 0)
          (for element :in (source-text::source-sequence-elements instance))
          (unless (typep element 'source-text:source-whitespace)
            (bind ((*form-path* (cons index *form-path*)))
              (collect (build-form* element)))
            (incf index)))))
