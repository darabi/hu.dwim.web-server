;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

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
