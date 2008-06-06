;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (js-macro e) |in-package| (package)
  (declare (ignore package))
  (values))

(def (js-macro e) |on-load| (&body |body|)
  {(with-readtable-case :preserve)
   `(dojo.add-on-load
     (lambda ()
       ,@body))})
