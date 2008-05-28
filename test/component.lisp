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

(def special-variable *test-instances*)

(def function make-test-instances ()
  (setf *test-instances*
        (list (make-instance 'super-test :string-slot "Hello World" :sister-slot (make-instance 'sister-test :boolean-slot #f))
              (make-instance 'sub-test :string-slot "Hello World" :integer-slot 42 :sister-slot (make-instance 'sister-test :boolean-slot #t))
              (make-instance 'sub-test :string-slot "I'll be back" :integer-slot 42))))

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
    <p "root component: " ,(princ-to-string self)>
    ;;<span (:onclick `js-inline(alert "fooo")) "click-me!">
    <a (:href ,(concatenate-string (path-prefix-of *application*) (if *session* "delete/" "new/")))
      ,(if *session* "drop session" "new session")>
    <hr>
    <form (:method "post")
      <input (:name ,(id-of (register-client-state-sink *frame* (lambda (value)
                                                                  (setf (some-input-of self) value))))
              :value ,(some-input-of self))>
      <input (:type "submit")>>
    <hr>
    ,(render-request *request*)>)

(def component counter-component ()
  ((value 0)))

(def render counter-component ()
  (with-slots (value) self
    <p
      "Counter: " ,value
      <br>
      <a (:href ,(action-to-href (make-action (incf value))))
      "increment">
      <br>
      <a (:href ,(action-to-href (make-action (decf value))))
      "decrement">>))

(def function make-process-menu-item ()
  `ui(menu-item
      (replace-menu-target-command (icon "Process")
                                   (process
                                    ,`(iter (with step = "starting")
                                            (repeat 3)
                                            (setf step (call `ui(vertical-list
                                                                 (string ,(concatenate 'string "Odd page after " step))
                                                                 (process
                                                                  ,`(progn
                                                                      (call `ui(vertical-list
                                                                                (string "First")
                                                                                (answer-command)))
                                                                      (call `ui(vertical-list
                                                                                (string "Second")
                                                                                (answer-command)))
                                                                      (call `ui(vertical-list
                                                                                (string "Third")
                                                                                (answer-command)))))
                                                                 (answer-command "Odd"))))
                                            (setf step (call `ui(vertical-list
                                                                 (string ,(concatenate 'string "Even page after " step))
                                                                 (answer-command "Even")))))))))

(def function make-test-menu ()
  `ui(menu
      (menu-item (string "Debug")
                 (menu-item (command (icon "Start Over") (action #+nil (value)))) ;; TODO
                 (menu-item (command (icon "Hierarchy") (action (setf (hu.dwim.wui::debug-component-hierarchy-p *frame*) (not (hu.dwim.wui::debug-component-hierarchy-p *frame*)))))))
      (menu-item (string "Simple")
                 (menu-item (replace-menu-target-command (icon "Hello World") (string "Hello World")))
                 (menu-item (replace-menu-target-command (icon "Lexical Counter")
                                                         ,(bind ((counter 0))
                                                                `ui(vertical-list
                                                                    (delay `ui(string ,(princ-to-string counter)))
                                                                    (command-bar
                                                                     (command (icon "Increment") (action (incf counter)))
                                                                     (command (icon "Decrement") (action (decf counter))))))))
                 (menu-item (replace-menu-target-command (icon "Counter Component")
                                                         ,(make-instance 'counter-component)))
                 (menu-item (replace-menu-target-command (icon "Special variable")
                                                         ,(make-special-variable-place-component '*test-string* '(or null string))))
                 (menu-item (replace-menu-target-command (icon "Lexical variable")
                                                         ,(bind ((test-boolean #t)) (make-lexical-variable-place-component test-boolean 'boolean)))))
      (menu-item (string "Complex")
                 (menu-item (replace-menu-target-command (icon "Inspect") ,(make-viewer-component *server*)))
                 (menu-item (replace-menu-target-command (icon "Filter") ,(make-filter-component (find-class 'super-test))))
                 (menu-item (replace-menu-target-command (icon "List") ,(make-viewer-component *test-instances*)))
                 (menu-item (replace-menu-target-command (icon "Edit") ,(make-editor-component (first *test-instances*))))
                 (menu-item (replace-menu-target-command (icon "New") ,(make-maker-component (find-class 'sub-test))))
                 ,(make-process-menu-item))
      (menu-item (string "Demo")
                 (menu-item (string "Telephely")
                            (menu-item (replace-menu-target-command (icon "Felvétel") ,(make-maker-component (find-class 'telephely))))
                            (menu-item (replace-menu-target-command (icon "Karbantartás") ,(make-filter-component (find-class 'telephely)))))
                 (menu-item (string "Terem")
                            (menu-item (replace-menu-target-command (icon "Felvétel") ,(make-maker-component (find-class 'terem))))
                            (menu-item (replace-menu-target-command (icon "Karbantartás") ,(make-filter-component (find-class 'terem)))))
                 (menu-item (string "Polc")
                            (menu-item (replace-menu-target-command (icon "Felvétel") ,(make-maker-component (find-class 'polc))))
                            (menu-item (replace-menu-target-command (icon "Karbantartás") ,(make-filter-component (find-class 'polc)))))
                 (menu-item (string "Hely")
                            (menu-item (replace-menu-target-command (icon "Felvétel") ,(make-maker-component (find-class 'hely))))
                            (menu-item (replace-menu-target-command (icon "Karbantartás") ,(make-filter-component (find-class 'hely))))))))

(def function make-test-frame ()
  (bind ((menu (make-test-menu))
         (menu-target `ui(empty)))
    (prog1 `ui(frame (:title "Demo" :stylesheets ("static/test/test.css"))
                     (vertical-list (horizontal-list ,menu (top ,menu-target))
                                    ,(make-instance 'session-information-component)))
           (setf (hu.dwim.wui::target-place-of menu) (make-component-place menu-target)))))
