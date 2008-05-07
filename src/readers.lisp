;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(define-syntax js-sharpquote ()
  (set-dispatch-macro-character
   #\# #\"
   (lambda (s c1 c2)
     (declare (ignore c2))
     (unread-char c1 s)
     (let ((key (read s)))
       `(ucw.i18n.lookup ,key)))))

(define-syntax sharpquote<> ()
  "Enable quote reader for the rest of the file (being loaded or compiled).
#\"my i18n text\" parts will be replaced by a LOOKUP-RESOURCE call for the string."
  (set-dispatch-macro-character
   #\# #\"
   (lambda (s c1 c2)
     (declare (ignore c2))
     (unread-char c1 s)
     (let ((key (read s)))
       (if (ends-with-subseq "<>" key)
           `(bind (((:values str foundp) (lookup-resource ,(subseq key 0 (- (length key) 2)) nil)))
              ,(when (and (> (length key) 0)
                          (upper-case-p (elt key 0)))
                     `(setf str (capitalize-first-letter str)))
              (if foundp
                  (<:as-html str)
                  (<:span :class +missing-resource-css-class+
                          (<:as-html str))))
           `(bind (((:values str foundp) (lookup-resource ,key nil)))
              (declare (ignorable foundp))
              ,(when (and (> (length key) 0)
                          (upper-case-p (elt key 0)))
                     `(when foundp
                        (setf str (capitalize-first-letter str))))
              str))))))

