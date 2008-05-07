;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (function eio) content-type-for (mime-type &optional (encoding (encoding-name-of *response*)))
  (or (case encoding
        (:utf-8    (switch (mime-type :test #'string=)
                     (+html-mime-type+ +utf-8-html-content-type+)
                     (+css-mime-type+  +utf-8-css-content-type+)))
        (:us-ascii (switch (mime-type :test #'string=)
                     (+html-mime-type+ +us-ascii-html-content-type+)
                     (+css-mime-type+  +us-ascii-css-content-type+)))
        (:iso8859-1 (switch (mime-type :test #'string=)
                      (+html-mime-type+ +iso-8859-1-html-content-type+)
                      (+css-mime-type+  +iso-8859-1-css-content-type+))))
      (concatenate 'string mime-type "; charset=" (string-downcase encoding))))

(def (macro e) with-simple-html-body ((&key title) &body body)
  `<html
     <head
       <meta (:content ,(header-value *response* +header/content-type+)
              :http-equiv #.+header/content-type+)>
       <title ,,(or title "")>>
     <body ,,@body>>)
