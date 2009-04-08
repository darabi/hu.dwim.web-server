;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def macro with-output-to-export-stream ((stream-name &key external-format content-type) &body body)
  (with-unique-names (response-body content-type-tmp)
    `(bind ((,response-body (with-output-to-sequence (,stream-name :external-format ,external-format
                                                                   :initial-buffer-size 256)
                              ,@body)))
       (make-byte-vector-response* ,response-body :headers (nconc (bind ((,content-type-tmp ,content-type))
                                                                    (when ,content-type-tmp
                                                                      (list (cons +header/content-type+ ,content-type-tmp))))
                                                                  (list (cons +header/content-disposition+ "attachment")))))))

;;;;;;
;;; Exportable component

(def component exportable-component ()
  ())

(def layered-method make-standard-commands ((component exportable-component) (class standard-class) (prototype-or-instance standard-object))
  (append (optional-list (make-export-command :csv component class prototype-or-instance)
                         (make-export-command :pdf component class prototype-or-instance)
                         (make-export-command :ods component class prototype-or-instance)
                         (make-export-command :odt component class prototype-or-instance))
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
           (make-component-action component
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
           (make-component-action component
             (with-output-to-export-stream (*pdf-stream* :content-type +pdf-mime-type+ :external-format :iso-8859-1)
               (execute-export-pdf component)))
           :delayed-content #t
           :path (export-file-name format component)))

(def (layered-function e) execute-export-pdf (component))

;;;;;;
;;; ODT

(def icon export-odt)

(def resources hu
  (icon-label.export-odt "ODT")
  (icon-tooltip.export-odt "A tartalom mentése ODT formátumban"))

(def resources en
  (icon-label.export-odt "ODT")
  (icon-tooltip.export-odt "Export content in ODT format"))

(def special-variable *odt-stream*)

(def layered-method make-export-command ((format (eql :odt)) (component component) (class standard-class) (prototype-or-instance standard-object))
  (command (icon export-odt)
           (make-component-action component
             (with-output-to-export-stream (*odt-stream* :content-type +odt-mime-type+ :external-format :utf-8)
               (execute-export-odt component)))
           :delayed-content #t
           :path (export-file-name format component)))

(def (layered-function e) execute-export-odt (component)
  (:method ((component component))
    (render-odt component)))

;;;;;;
;;; ODS

(def icon export-ods)

(def resources hu
  (icon-label.export-ods "ODS")
  (icon-tooltip.export-ods "A tartalom mentése ODS formátumban"))

(def resources en
  (icon-label.export-ods "ODS")
  (icon-tooltip.export-ods "Export content in ODS format"))

(def special-variable *ods-stream*)

(def layered-method make-export-command ((format (eql :ods)) (component component) (class standard-class) (prototype-or-instance standard-object))
  (command (icon export-ods)
           (make-component-action component
             (with-output-to-export-stream (*ods-stream* :content-type +ods-mime-type+ :external-format :utf-8)
               (execute-export-ods component)))
           :delayed-content #t
           :path (export-file-name format component)))

(def (layered-function e) execute-export-ods (component)
  (:method ((component component))
    (render-ods component)))
