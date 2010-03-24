;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/toc/inspector

(def (component e) t/toc/inspector (inspector/style t/detail/inspector contents/widget)
  ())

(def refresh-component t/toc/inspector
  (bind (((:slots contents component-value) -self-))
    (setf contents (mapcar [make-value-inspector !1
                                                 :initial-alternative-type 't/toc/inspector
                                                 :edited (edited-component? -self-)
                                                 :editable (editable-component? -self-)]
                           (collect-if (of-type 'chapter) (contents-of component-value))))))

(def method render-command-bar-for-alternative? ((component t/toc/inspector))
  #f)

;;;;;;
;;; book/toc/inspector

(def (component e) book/toc/inspector (t/toc/inspector)
  ())

(def refresh-component book/toc/inspector
  )

(def render-xhtml book/toc/inspector
  <div (:class "toc") "Tartalomjegyzék">
  (render-contents-for -self-))

(def function toc-numbering (component)
  (bind ((component-value (component-value-of component)))
    ;; TODO: toc/mixin?
    (awhen (find-descendant-component (toc-of (find-ancestor-component-with-type component 'book/text/inspector))
                                      (lambda (descendant)
                                        (eq (component-value-of descendant) component-value)))
      (numbering-of (content-of it)))))

;;;;;;
;;; chapter/toc/inspector

(def (component e) chapter/toc/inspector (t/toc/inspector)
  ((numbering :type string)
   (reference :type component)))

(def refresh-component chapter/toc/inspector
  (bind (((:slots numbering reference component-value) -self-))
    (setf numbering (awhen (find-ancestor-component-with-type (parent-component-of -self-) 't/toc/inspector)
                      (bind ((numbering (integer-to-string (1+ (position -self- (contents-of it) :key 'content-of)))))
                        (if (typep it 'chapter/toc/inspector)
                            (string+ (numbering-of it) "." numbering)
                            numbering)))
          reference (make-instance 't/reference/inspector :component-value component-value))))

(def render-xhtml chapter/toc/inspector
  (bind (((:read-only-slots reference) -self-))
    (render-component reference)
    (render-contents-for -self-)))
