;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (js-lisp-macro-alias e) awhen)
(def (js-lisp-macro-alias e) aif)
(def (js-lisp-macro-alias e) acond)

(def (js-macro e) |in-package| (package)
  (declare (ignore package))
  (values))

(def (js-macro e) |on-load| (&body body)
  {(with-readtable-case :preserve)
   `(dojo.add-on-load
     (lambda ()
       ,@BODY))})

(def (js-macro e) $ (&body things)
  (if (length= 1 things)
      {(with-readtable-case :preserve)
       `(dojo.by-id ,(FIRST THINGS))}
      {(with-readtable-case :preserve)
       `(map 'dojo.by-id ,THINGS)}))

(def (js-macro e) |defun*| (name args &body body)
  (bind ((name-pieces (cl-ppcre:split "\\." (symbol-name name))))
    (if (length= 1 name-pieces)
        {(with-readtable-case :preserve) `(defun ,NAME ,ARGS ,@BODY)}
        {(with-readtable-case :preserve)
         `(progn
            (setf ,NAME (lambda ,ARGS
                          ,@BODY)))})))
