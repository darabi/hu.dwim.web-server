;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def render :around component
  (restart-case
      (if (and *debug-component-hierarchy*
               ;; TODO: the <table><tr><td> has various constraints, so rows are not displayed in debug mode
               (not (typep -self- '(or frame-component row-component))))
          (bind ((class-name (string-downcase (symbol-name (class-name (class-of -self-)))))
                 (*debug-component-hierarchy* (and *debug-component-hierarchy*
                                                   (not (typep -self- 'command-component)))))
            <div (:class "debug-component")
              <div (:class "debug-component-name")
                ,class-name
                <span
                  <a (:href ,(action-to-href (register-action *frame* (make-copy-to-repl-action -self-)))) "REPL">
                  " "
                  <a (:href ,(action-to-href (register-action *frame* (make-inspect-in-repl-action -self-)))) "INSPECT">>>
              ,(call-next-method)>)
          (call-next-method))
    (skip-rendering-component ()
      :report (lambda (stream)
                (format stream "Skip rendering ~A and put an error marker in place" -self-))
      <div (:class "rendering-error") "Error during rendering " ,(princ-to-string -self-)>)))

(def function make-inspect-in-repl-action (component)
  (make-action
    (awhen (or swank::*emacs-connection*
               (swank::default-connection))
      (swank::with-connection (it)
        (bind ((swank::*buffer-package* *package*)
               (swank::*buffer-readtable* *readtable*))
          (swank::inspect-in-emacs component))))))

(def function make-copy-to-repl-action (component)
  (make-action
    (awhen (or swank::*emacs-connection*
               (swank::default-connection))
      (swank::with-connection (it)
        (swank::present-in-emacs component)
        (swank::present-in-emacs #.(string #\Newline))))))

(def special-variable *component-print-object-level* 0)

(def special-variable *component-print-object-depth* 3)

(def method print-object ((self component) stream)
  (bind ((*print-level* nil)
         (*component-print-object-level* (1+ *component-print-object-level*))
         (*standard-output* stream))
    (handler-bind ((error (lambda (error)
                            (declare (ignore error))
                            (write-string "<<error printing component>>")
                            (return-from print-object))))
      (pprint-logical-block (stream nil :prefix "#<" :suffix ">")
        (pprint-indent :current 1 stream)
        (iter (with class = (class-of self))
              (with class-name = (symbol-name (class-name class)))
              (initially (princ class-name))
              (for slot :in (class-slots class))
              (when (bound-child-component-slot-p class self slot)
                (bind ((initarg (first (slot-definition-initargs slot)))
                       (value (slot-value-using-class class self slot)))
                  (write-char #\Space)
                  (pprint-newline :fill)
                  (prin1 initarg)
                  (write-char #\Space)
                  (pprint-newline :fill)
                  (if (<= *component-print-object-level* *component-print-object-depth*)
                      (prin1 value)
                      (princ "#"))))))))
  self)
