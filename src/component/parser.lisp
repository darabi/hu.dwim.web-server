;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Parser

(def function ui-syntax-node-name (name)
  (format-symbol (find-package :hu.dwim.wui) "~A-COMPONENT" name))

(def function parse-quasi-quoted-ui (form)
  (if (typep form 'qq::syntax-node)
      form
      (parse-quasi-quoted-ui* (ui-syntax-node-name (first form)) form)))

(def generic parse-quasi-quoted-ui* (first whole))

(def method parse-quasi-quoted-ui* (first whole)
  (apply #'make-instance first
         (iter (for element :in (cdr whole))
               (collect
                   (if (consp element)
                       (parse-quasi-quoted-ui element)
                       element)))))

(def method parse-quasi-quoted-ui* ((first (eql 'action-component)) whole)
  (make-ui-unquote `(make-action ,@(cdr whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'icon-component)) whole)
  (make-icon-component nil :label (second whole) :image-url (third whole) :tooltip (fourth whole)))

(def method parse-quasi-quoted-ui* ((first (eql 'command-component)) whole)
  (make-instance 'command-component :icon (parse-quasi-quoted-ui (second whole)) :action (parse-quasi-quoted-ui (third whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'command-bar-component)) whole)
  (make-instance 'command-bar-component :commands (mapcar #'parse-quasi-quoted-ui (cdr whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'frame-component)) whole)
  (apply #'make-instance 'frame-component :content (parse-quasi-quoted-ui (third whole)) (second whole)))

(def method parse-quasi-quoted-ui* ((first (eql 'top-component)) whole)
  (make-instance 'top-component :content (parse-quasi-quoted-ui (second whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'inline-component)) whole)
  (make-instance 'inline-component :thunk (make-ui-unquote `(lambda () ,@(cdr whole)))))

(def method parse-quasi-quoted-ui* ((first (eql 'label-component)) whole)
  (make-instance 'label-component :component-value (second whole)))

(def method parse-quasi-quoted-ui* ((first (eql 'boolean-component)) whole)
  (make-instance 'boolean-component :component-value (second whole)))

(def method parse-quasi-quoted-ui* ((first (eql 'string-component)) whole)
  (make-instance 'string-component :component-value (second whole)))

(def method parse-quasi-quoted-ui* ((first (eql 'integer-component)) whole)
  (make-instance 'integer-component :component-value (second whole)))

(def method parse-quasi-quoted-ui* ((first (eql 'vertical-list-component)) whole)
  (make-instance 'vertical-list-component :components (mapcar #'parse-quasi-quoted-ui (cdr whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'horizontal-list-component)) whole)
  (make-instance 'horizontal-list-component :components (mapcar #'parse-quasi-quoted-ui (cdr whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'standard-process-component)) whole)
  (make-instance 'standard-process-component :form (second whole)))

(def method parse-quasi-quoted-ui* ((first (eql 'menu-component)) whole)
  (make-instance 'menu-component :menu-items (mapcar #'parse-quasi-quoted-ui (cdr whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'menu-item-component)) whole)
  (make-instance 'menu-item-component
                 :command (parse-quasi-quoted-ui (second whole))
                 :menu-items (mapcar #'parse-quasi-quoted-ui (cddr whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'replace-menu-target-command-component)) whole)
  (make-instance 'replace-menu-target-command-component :icon (parse-quasi-quoted-ui (second whole)) :component (parse-quasi-quoted-ui (third whole))))

(def method parse-quasi-quoted-ui* ((first (eql 'answer-command-component)) whole)
  (make-instance 'answer-command-component :value (second whole)))
