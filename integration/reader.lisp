;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/lisp-form/inspector

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

(def function make-source-readtable ()
  ;; TODO: use the original readtable (at least derive from it)
  (prog1-bind readtable (source-text::make-source-readtable)
    (source-text::enable-sharp-boolean-syntax readtable)
    (source-text::enable-shebang-syntax readtable)))

;;;;;;
;;; Render lisp form

;; TODO: some factoring could make this code shorter
{with-quasi-quoted-xml-to-binary-emitting-form-syntax/preserve-whitespace
 (def layered-methods render-source-object
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
       (awhen (ignore-errors (symbol-function value))
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
