;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def method render :around ((self component))
  (restart-case
      ;; TODO: the <table><tr><td> has various constraints, so rows are not displayed in debug mode
      (if (typep self 'row-component)
          (call-next-method)
          (if (and *frame*
                   (debug-component-hierarchy-p *frame*))
              (bind ((class-name (string-downcase (symbol-name (class-name (class-of self))))))
                <div (:class "debug-component")
                  <div (:class "debug-component-name")
                       ,class-name
                       ,(if (typep self '(or icon-component command-component))
                            <a (:href ,(action-to-href (register-action *frame* (make-copy-to-repl-action self))))
                               "REPL">
                            +void+)>
                  ,(call-next-method)>)
              (call-next-method)))
    (skip-rendering-component ()
      :report (lambda (stream)
                (format stream "Skip rendering ~A and put an error marker in place" self))
      <div (:class "rendering-error") "Error during rendering " ,(princ-to-string self)>)))

(def function make-copy-to-repl-action (component)
  (make-action
    (awhen (or swank::*emacs-connection*
               (swank::default-connection))
      (swank::with-connection (it)
        (swank::present-in-emacs component)
        (swank::present-in-emacs #.(string #\Newline))))))

(def method print-object ((self component) stream)
  (bind ((*print-level* nil)
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
                  (prin1 value)))))))
  self)
