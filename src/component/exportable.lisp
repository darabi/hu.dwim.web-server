;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def macro with-output-to-export-stream ((stream-name &key external-format content-type) &body body)
  (with-unique-names (response-body content-type-tmp)
    `(bind ((,response-body (with-output-to-sequence (,stream-name :external-format ,external-format
                                                                   :initial-buffer-size 256)
                              ,@body)))
       (make-byte-vector-response* ,response-body :headers (bind ((,content-type-tmp ,content-type))
                                                             (when ,content-type-tmp
                                                               `((+header/content-type+ . ,,content-type-tmp))))))))

;;;;;;
;;; Exportable component

(def component exportable-component ()
  ())

(def layered-method make-standard-commands ((component exportable-component) (class standard-class) (prototype-or-instance standard-object))
  (append (optional-list (make-export-command :csv component class prototype-or-instance)
                         (make-export-command :pdf component class prototype-or-instance)
                         #+nil ;; not yet implemented
                         (make-export-command :odf component class prototype-or-instance))
          (call-next-method)))

(def (layered-function e) make-export-command (format component class prototype-or-instance))

(def (layered-function e) export-file-name (format component)
  (:method :around (format (component component))
    (awhen (call-next-method)
      (concatenate-string it "." (string-downcase format))))

  (:method (format (component component))
    nil))

;;;;;;
;;; CSV

(def icon export-csv)

(def resources hu
  (icon-label.export-csv "CSV")
  (icon-tooltip.export-csv "A tartalom mentése CSV formátumban"))

(def resources en
  (icon-label.export-csv "CSV")
  (icon-tooltip.export-csv "Export content in CSV format"))

(def layered-method make-export-command ((format (eql :csv)) (component component) (class standard-class) (prototype-or-instance standard-object))
  (command (icon export-csv)
           (make-action
             (with-output-to-export-stream (*csv-stream* :content-type +csv-mime-type+ :external-format :utf-8)
               (execute-export-csv component)))
           :delayed-content #t
           :path (export-file-name format component)))

(def (layered-function e) execute-export-csv (component)
  (:method ((component component))
    (render-csv component)))

;;;;;;
;;; PDF

(def icon export-pdf)

(def resources hu
  (icon-label.export-pdf "PDF")
  (icon-tooltip.export-pdf "A tartalom mentése PDF formátumban"))

(def resources en
  (icon-label.export-pdf "PDF")
  (icon-tooltip.export-pdf "Export content in PDF format"))

(def special-variable *pdf-stream*)

(def layered-method make-export-command ((format (eql :pdf)) (component component) (class standard-class) (prototype-or-instance standard-object))
  (command (icon export-pdf)
           (make-action
             (with-output-to-export-stream (*pdf-stream* :content-type +pdf-mime-type+ :external-format :iso-8859-1)
               (execute-export-pdf component)))
           :delayed-content #t
           :path (export-file-name format component)))

(def (layered-function e) execute-export-pdf (component))

;;;;;;
;;; ODF

(def icon export-odf)

(def resources hu
  (icon-label.export-odf "ODF")
  (icon-tooltip.export-odf "A tartalom mentése ODF formátumban"))

(def resources en
  (icon-label.export-odf "ODF")
  (icon-tooltip.export-odf "Export content in ODF format"))

(def special-variable *odf-stream*)

(def layered-method make-export-command ((format (eql :odf)) (component component) (class standard-class) (prototype-or-instance standard-object))
  (command (icon export-odf)
           (make-action
             (with-output-to-export-stream (*odf-stream* :content-type +odf-mime-type+ :external-format :utf-8)
               (execute-export-odf component)))
           :delayed-content #t
           :path (export-file-name format component)))

(def (layered-function e) execute-export-odf (component)
  (:method ((component component))
    (render-odf component)))
