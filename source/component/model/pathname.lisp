;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; pathname/inspector

(def (component e) pathname/inspector (t/inspector)
  ())

(def (macro e) pathname/inspector (pathname &rest args &key &allow-other-keys)
  `(make-instance 'pathname/inspector :component-value ,pathname ,@args))

(def layered-method find-inspector-type-for-prototype ((prototype pathname))
  'pathname/inspector)

(def layered-method make-alternatives ((component pathname/inspector) class prototype value)
  (bind ((file? (pathname-name value))
         (file-type (when file?
                      (guess-file-type value))))
    (optional-list* (unless file?
                      (delay-alternative-component-with-initargs 'pathname/directory/tree/inspector :component-value value))
                    (when (eq file-type :text)
                      (delay-alternative-component-with-initargs 'pathname/text-file/inspector :component-value value))
                    (when (member file-type '(:asd :lisp))
                      (delay-alternative-component-with-initargs 'pathname/lisp-file/inspector :component-value value))
                    (when (eq file-type :binary)
                      (delay-alternative-component-with-initargs 'pathname/binary-file/inspector :component-value value))
                    (call-next-method))))

;;;;;;
;;; pathname/text-file/inspector

(def (component e) pathname/text-file/inspector (inspector/basic content/widget)
  ())

(def (macro e) pathname/text-file/inspector (pathname &rest args &key &allow-other-keys)
  `(make-instance 'pathname/text-file/inspector :component-value ,pathname ,@args))

(def refresh-component pathname/text-file/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (read-file-into-string component-value))))

(def render-xhtml pathname/text-file/inspector
  (with-render-style/mixin (-self- :element-name "pre")
    (render-content-for -self-)))

;;;;;;
;;; pathname/binary-file/inspector

(def (component e) pathname/binary-file/inspector (inspector/basic content/widget)
  ())

(def (macro e) pathname/binary-file/inspector (pathname &rest args &key &allow-other-keys)
  `(make-instance 'pathname/binary-file/inspector :component-value ,pathname ,@args))

(def refresh-component pathname/binary-file/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (with-output-to-string (string)
                    (iter (for index :from 1)
                          (for byte :in-vector (read-file-into-byte-vector component-value))
                          (format string "~2,'0',X " byte)
                          (when (zerop (mod index 40))
                            (terpri string)))))))

(def render-xhtml pathname/binary-file/inspector
  (with-render-style/mixin (-self- :element-name "pre")
    (render-content-for -self-)))

;;;;;;
;;; pathname/lisp-file/inspector

(def (component e) pathname/lisp-file/inspector (inspector/basic content/widget)
  ())

(def (macro e) pathname/lisp-file/inspector ((&rest args &key &allow-other-keys) &body file)
  `(make-instance 'pathname/lisp-file/inspector ,@args :component-value ,(the-only-element file)))

(def refresh-component pathname/lisp-file/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (make-instance 't/lisp-form/inspector :component-value (read-lisp-source component-value)))))

;;;;;;
;;; pathname/directory-tree/inspector

(def (component e) pathname/directory/tree/inspector (t/tree/inspector)
  ())

(def layered-method make-tree/root-node ((component pathname/directory/tree/inspector) (class structure-class) (prototype pathname) (value pathname))
  (make-instance 'pathname/directory/node/inspector :component-value value))

;;;;;;
;;; pathname/directory/node/inspector

(def (component e) pathname/directory/node/inspector (t/node/inspector)
  ())

(def layered-method collect-tree/children ((component pathname/directory/node/inspector) (class structure-class) (prototype pathname) (value pathname))
  (sort (directory (merge-pathnames "*.*" value)) #'string< :key #'namestring))

(def layered-method make-node/child-node ((component pathname/directory/node/inspector) (class structure-class) (prototype pathname) (value pathname))
  (if (pathname-name value)
      (make-instance 'pathname/file/node/inspector :component-value value :expanded #f)
      (make-instance 'pathname/directory/node/inspector :component-value value :expanded #f)))

;;;;;;
;;; pathname/file/node/inspector

(def (component e) pathname/file/node/inspector (t/node/inspector)
  ())

(def layered-method collect-tree/children ((component pathname/file/node/inspector) (class structure-class) (prototype pathname) (value pathname))
  nil)
