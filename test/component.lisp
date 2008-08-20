;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :wui-test)

;;;;;;
;;; Test classes

(def class* sister-test ()
  ((boolean-slot :type boolean)))

(def class* super-test ()
  ((string-slot :type string)
   (sister-slot :type (or null sister-test))))

(def class* sub-test (super-test)
  ((integer-slot :type integer)))

(def special-variable *test-instances* nil)

(def function make-test-instances ()
  (setf *test-instances*
        (append *test-instances*
                (list (make-instance 'super-test :string-slot "Hello World" :sister-slot (make-instance 'sister-test :boolean-slot #f))
                      (make-instance 'sub-test :string-slot "Hello World" :integer-slot 42 :sister-slot (make-instance 'sister-test :boolean-slot #t))
                      (make-instance 'sub-test :string-slot "I'll be back" :integer-slot 42)))))

(make-test-instances)

;;;;;;
;;; Telephely

(def class* hely ()
  ((azonosító nil :type (or null string))
   (név nil :type (or null string))))

(def class* telephely (hely)
  ((termek nil :type (list terem))))

(def class* terem (hely)
  ((polcok nil :type (list polc))))

(def class* polc (hely)
  ())

;;;;;;
;;; Frame

(def special-variable *test-string* "Edit this text")

(def class* test-object ()
  ((test-slot :type (or null integer))))

(def component session-information-component (content-component)
  ((some-input "foo")))

(def render session-information-component ()
  <div
    <p "session: " ,(princ-to-string *session*)>
    <p "frame: " ,(princ-to-string *frame*)>
    <p "root component: " ,(princ-to-string -self-)>
    ;;<span (:onclick `js-inline(alert "fooo")) "click-me!">
    <a (:href ,(concatenate-string (path-prefix-of *application*) (if *session* "delete/" "new/")))
      ,(if *session* "drop session" "new session")>
    <hr>
    <form (:method "post")
      <input (:name ,(id-of (register-client-state-sink *frame* (lambda (value)
                                                                  (setf (some-input-of -self-) value))))
              :value ,(some-input-of -self-))>
      <input (:type "submit")>>
    <hr>
    ,(render-request *request*)>)

(def component counter-component ()
  ((value 0)))

(def render counter-component ()
  (with-slots (value) -self-
    <p
      "Counter: " ,value
      <br>
      <a (:href ,(action-to-href (make-action (incf value))))
      "increment">
      <br>
      <a (:href ,(action-to-href (make-action (decf value))))
      "decrement">>))

(def function make-process-menu-item ()
  (menu-item (replace-menu-target-command (label "Process")
                                          (standard-process
                                            (iter (with step = "starting")
                                                  (repeat 3)
                                                  (setf step (call-component (vertical-list ()
                                                                               (label (concatenate 'string "Odd page after " step))
                                                                               (standard-process
                                                                                 (progn
                                                                                   (call (label "First")
                                                                                         (answer-command))
                                                                                   (call (label "Second")
                                                                                         (answer-command))
                                                                                   (call (labelstring "Third")
                                                                                         (answer-command)))))
                                                                             (answer-command "Odd")))
                                                  (setf step (call-component (label (concatenate 'string "Even page after " step))
                                                                             (answer-command "Even"))))))))

(def function make-test-menu ()
  (menu nil
    (menu "Debug"
      (menu-item (command (label "Start Over") (make-action (setf (root-component-of *frame*) nil))))
      (menu-item (command (label "Hierarchy") (make-action (hu.dwim.wui::toggle-debug-component-hierarchy *frame*)))))
    (menu "Simple"
      (menu-item (replace-menu-target-command (label "Hello World") (string "Hello World")))
      (menu-item (replace-menu-target-command (label "Lexical Counter")
                                              (bind ((counter 0))
                                                (vertical-list ()
                                                  (delay (label (princ-to-string counter)))
                                                  (command-bar
                                                    (command (label "Increment") (make-action (incf counter)))
                                                    (command (label "Decrement") (make-action (decf counter))))))))
      (menu-item (replace-menu-target-command (label "Counter Component")
                                              (make-instance 'counter-component)))
      (menu-item (replace-menu-target-command (label "Special variable")
                                              (make-special-variable-place-inspector '*test-string* '(or null string))))
      (menu-item (replace-menu-target-command (label "Lexical variable")
                                              (bind ((test-boolean #t)) (make-lexical-variable-place-inspector test-boolean 'boolean)))))
    (menu "Complex"
      (menu-item (replace-menu-target-command (label "Inspect") (make-viewer *server*)))
      (menu-item (replace-menu-target-command (label "Filter") (make-filter (find-class 'super-test))))
      (menu-item (replace-menu-target-command (label "List") (make-viewer *test-instances*)))
      (menu-item (replace-menu-target-command (label "Edit") (make-editor (first *test-instances*))))
      (menu-item (replace-menu-target-command (label "New") (make-maker (find-class 'sub-test))))
      (make-process-menu-item))))

(def function make-test-frame ()
  (bind ((menu (make-test-menu))
         (menu-target (empty)))
    (prog1 (frame (:title "Demo" :stylesheet-uris '("static/wui/css/test.css"))
             (horizontal-list () menu (top menu-target)))
      (setf (target-place-of menu) (make-component-place menu-target)))))
