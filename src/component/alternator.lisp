;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Alternator component

(def component alternator-component ()
  ((default-component-type nil)
   (alternatives-factory nil :type function)
   (alternatives nil)
   (content nil :type component)
   (command-bar nil :type component)))

(def method clone-component ((self alternator-component))
  (prog1-bind clone (call-next-method)
    (setf (default-component-type-of clone) (default-component-type-of self))))

(def render alternator-component ()
  (bind (((:read-only-slots content command-bar) -self-))
    (if (typep content '(or reference-component primitive-component))
        (render content)
        (render-vertical-list (list content command-bar)))))

(def render-csv alternator-component ()
  (render-csv (content-of -self-)))

(def render-pdf alternator-component ()
  (render-pdf (content-of -self-)))

(def function make-alternative-commands (component alternatives)
  (delete nil
          (mapcar (lambda (alternative)
                    (make-replace-with-alternative-command component alternative))
                  alternatives)))

(def function make-alternator-command-bar (component alternatives commands)
  (make-instance 'command-bar-component
                 :commands (append commands (make-alternative-commands component alternatives))
                 :visible (delay (not (typep (content-of component) 'reference-component)))))

(def generic make-replace-with-alternative-command (component alternative)
  (:method ((component alternator-component) alternative)
    (bind ((prototype (class-prototype (the-class-of alternative)))
           (reference? (typep prototype 'reference-component)))
      (make-replace-command (delay (content-of component)) alternative
                            :content (make-replace-with-alternative-command-icon prototype)
                            :visible (delay (and (not (has-edited-descendant-component-p (content-of component)))
                                                 (not (eq (the-class-of alternative) (class-of (content-of component))))
                                                 (or (not reference?)
                                                     (find-ancestor-component-with-type (parent-component-of component) 'inspector-component))))))))

(def (generic e) make-replace-with-alternative-command-icon (prototype)
  (:method ((prototype component))
    (bind ((name (string-capitalize (substitute #\Space #\- (trim-suffix "-component" (string-downcase (class-name (class-of prototype))))))))
      (icon view :label name :tooltip name)))

  (:method ((prototype reference-component))
    (icon collapse)))

(def function (setf component-value-for-alternatives) (new-value alternator)
  (dolist (alternative (alternatives-of alternator))
    (bind ((component (component-of alternative)))
      (when component
        (setf (component-value-of component) new-value)))))

(def method join-editing ((alternator alternator-component))
  (unless (typep (content-of alternator) 'reference-component)
    (call-next-method)))

(def class* component-factory ()
  ((the-class)
   (component nil))
  (:metaclass funcallable-standard-class))

(def function find-alternative-component (alternatives type)
  (some (lambda (class)
          (awhen (find-if (lambda (alternative)
                            (subtypep (the-class-of alternative) class))
                          alternatives)
            (force it)))
        (bind ((class (find-class type)))
          (class-precedence-list class))))

(def function find-default-alternative-component (alternatives)
  (find-alternative-component alternatives 'detail-component))

(def (macro e) delay-alternative-component (type &body forms)
  `(aprog1 (make-instance 'component-factory :the-class (find-class ,type))
     (set-funcallable-instance-function it (delay (or (component-of it)
                                                      (setf (component-of it) (progn ,@forms)))))))

(def (macro e) delay-alternative-component-with-initargs (type &rest args)
  `(delay-alternative-component ,type (make-instance ,type ,@args)))

(def (macro e) delay-alternative-reference-component (type target)
  `(delay-alternative-component ,type
     (prog1-bind reference (make-instance ,type :target ,target)
       (setf-expand-reference-to-default-alternative-command reference))))

(def function setf-expand-reference-to-default-alternative-command (reference)
  (bind ((target (target-of reference)))
    (setf (expand-command-of reference)
          (make-expand-reference-command reference (class-of target) target
                                         (delay (find-default-alternative-component (alternatives-of (parent-component-of reference)))))))
  reference)
