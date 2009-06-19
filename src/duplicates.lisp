;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def macro if-bind (var test &body then/else)
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(def macro when-bind (var test &body body)
  `(if-bind ,var ,test (progn ,@body)))

(def macro prog1-bind (var ret &body body)
  `(let ((,var ,ret))
    ,@body
    ,var))

(def macro cond-bind (var &body clauses)
  "Just like COND but VAR will be bound to the result of the
  condition in the clause when executing the body of the clause."
  (if clauses
      (destructuring-bind ((test &rest body) &rest others)
          clauses
        `(if-bind ,var ,test
           (progn ,@(if body body (list var)))
           (cond-bind ,var ,@others)))
      nil))

(def function system-relative-pathname (system path)
  (merge-pathnames path (asdf:component-pathname (asdf:find-system system))))

(def macro rebind (bindings &body body)
  `(let ,(loop
            :for symbol-name :in bindings
            :collect (list symbol-name symbol-name))
     ,@body))

;; from arnesi
(def macro dolist* ((iterator list &optional return-value) &body body)
  "Like DOLIST but destructuring-binds the elements of LIST.

If ITERATOR is a symbol then dolist* is just like dolist EXCEPT
that it creates a fresh binding."
  (if (listp iterator)
      (let ((i (gensym "DOLIST*-I-")))
        `(dolist (,i ,list ,return-value)
           (destructuring-bind ,iterator ,i
             ,@body)))
      `(dolist (,iterator ,list ,return-value)
         (let ((,iterator ,iterator))
           ,@body))))

(def function not-yet-implemented (&optional (datum "Not yet implemented." datum-p) &rest args)
  (when datum-p
    (setf datum (concatenate-string "Not yet implemented: " datum)))
  (apply #'cerror "Ignore and continue" datum args))

(def function operation-not-supported (&optional (datum "Operation not supported." datum-p) &rest args)
  (when datum-p
    (setf datum (concatenate-string "Operation not supported: " datum)))
  (apply #'error datum args))

(def function map-subclasses (class fn &key proper?)
  "Applies fn to each subclass of class. If proper? is true, then
the class itself is not included in the mapping. Proper? defaults to nil."
  (let ((mapped (make-hash-table :test #'eq)))
    (labels ((mapped-p (class)
               (gethash class mapped))
             (do-it (class root)
               (unless (mapped-p class)
                 (setf (gethash class mapped) t)
                 (unless (and proper? root)
                   (funcall fn class))
                 (mapc (lambda (class)
                         (do-it class nil))
                       (class-direct-subclasses class)))))
      (do-it (etypecase class
               (symbol (find-class class))
               (class class)) t))))

(def function subclasses (class &key (proper? t))
  "Returns all of the subclasses of the class including the class itself."
  (let ((result nil))
    (map-subclasses class (lambda (class)
                            (push class result))
                    :proper? proper?)
    (nreverse result)))

(def function sbcl-with-symbol (package name)
  #+sbcl (if (and (find-package (string package))
                  (find-symbol (string name) (string package)))
             '(:and)
             '(:or))
  #-sbcl '(:or))
