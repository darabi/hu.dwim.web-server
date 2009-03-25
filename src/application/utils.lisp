;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;; a second wave of utils after the one loaded by the wui-core system

(def function find-latest-dojo-directory-name (wwwroot-directory)
  (bind ((dojo-dir (first (sort (remove-if [not (starts-with-subseq "dojo" !1)]
                                           (mapcar [last-elt (pathname-directory !1)]
                                                   (cl-fad:list-directory wwwroot-directory)))
                                #'string>=))))
    (assert dojo-dir () "Seems like there's not any dojo directory in ~S. Hint: see wui/etc/build-dojo.sh" wwwroot-directory)
    (concatenate-string dojo-dir "/")))
