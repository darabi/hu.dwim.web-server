;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Exportable component

(def (component e) exportable/abstract ()
  ())

(def layered-method make-context-menu-items ((component exportable/abstract) (class standard-class) (prototype standard-object) (instance standard-object))
  (append (call-next-method)
          (list (make-menu-item (icon menu :label "Mentés")
                                (make-export-commands component class prototype instance)))))

(def layered-method make-export-commands (component class prototype value)
  (optional-list (make-export-command :csv component class prototype value)
                 (make-export-command :pdf component class prototype value)
                 (make-export-command :ods component class prototype value)
                 (make-export-command :odt component class prototype value)))

(def (layered-function e) export-file-name (format component)
  (:method :around (format (component component))
    (awhen (call-next-method)
      (concatenate-string it "." (string-downcase format))))

  (:method (format (component component))
    nil))

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
;;; Text format

(def (icon e) export-text)

(def layered-method make-export-command ((format (eql :text)) (component component) (class standard-class) (prototype standard-object) (instance standard-object))
  (command (:delayed-content #t
            :path (export-file-name format component))
    (icon export-text)
    (make-component-action component
      (with-output-to-export-stream (*text-stream* :content-type +text-mime-type+ :external-format :utf-8)
        (execute-export-text component)))))

(def (layered-function e) execute-export-text (component)
  (:method ((component component))
    (render-text component)))

;;;;;;
;;; CSV format

(def (icon e) export-csv)

(def layered-method make-export-command ((format (eql :csv)) (component component) (class standard-class) (prototype standard-object) (instance standard-object))
  (command (:delayed-content #t
            :path (export-file-name format component))
    (icon export-csv)
    (make-component-action component
      (with-output-to-export-stream (*csv-stream* :content-type +csv-mime-type+ :external-format :utf-8)
        (execute-export-csv component)))))

(def (layered-function e) execute-export-csv (component)
  (:method ((component component))
    (render-csv component)))

;;;;;;
;;; PDF format

(def (icon e) export-pdf)

(def special-variable *pdf-stream*)

(def layered-method make-export-command ((format (eql :pdf)) (component component) (class standard-class) (prototype standard-object) (instance standard-object))
  (command (:delayed-content #t
            :path (export-file-name format component))
    (icon export-pdf)
    (make-component-action component
      (with-output-to-export-stream (*pdf-stream* :content-type +pdf-mime-type+ :external-format :iso-8859-1)
        (execute-export-pdf component)))))

(def (layered-function e) execute-export-pdf (component))

;;;;;;
;;; ODT format

(def (icon e) export-odt)

(def layered-method make-export-command ((format (eql :odt)) (component component) (class standard-class) (prototype standard-object) (instance standard-object))
  (command (:delayed-content #t
            :path (export-file-name format component))
    (icon export-odt)
    (make-component-action component
      (with-output-to-export-stream (*xml-stream* :content-type +odt-mime-type+ :external-format :utf-8)
        (execute-export-odt component)))))

(def (layered-function e) execute-export-odt (component)
  (:method ((component component))
    (render-odt component)))

;;;;;;
;;; ODS format

(def (icon e) export-ods)

(def layered-method make-export-command ((format (eql :ods)) (component component) (class standard-class) (prototype standard-object) (instance standard-object))
  (command (:delayed-content #t
            :path (export-file-name format component))
    (icon export-ods)
    (make-component-action component
      (with-output-to-export-stream (*xml-stream* :content-type +ods-mime-type+ :external-format :utf-8)
        (execute-export-ods component)))))

(def (layered-function e) execute-export-ods (component)
  (:method ((component component))
    (emit-xml-prologue)
    <office:document (xmlns:office "urn:oasis:names:tc:opendocument:xmlns:office:1.0" xmlns:style "urn:oasis:names:tc:opendocument:xmlns:style:1.0" xmlns:text "urn:oasis:names:tc:opendocument:xmlns:text:1.0" xmlns:table "urn:oasis:names:tc:opendocument:xmlns:table:1.0" xmlns:draw "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0" xmlns:fo "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0" xmlns:xlink "http://www.w3.org/1999/xlink" xmlns:dc "http://purl.org/dc/elements/1.1/" xmlns:meta "urn:oasis:names:tc:opendocument:xmlns:meta:1.0" xmlns:number "urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0" xmlns:presentation "urn:oasis:names:tc:opendocument:xmlns:presentation:1.0" xmlns:svg "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0" xmlns:chart "urn:oasis:names:tc:opendocument:xmlns:chart:1.0" xmlns:dr3d "urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0" xmlns:math "http://www.w3.org/1998/Math/MathML" xmlns:form "urn:oasis:names:tc:opendocument:xmlns:form:1.0" xmlns:script "urn:oasis:names:tc:opendocument:xmlns:script:1.0" xmlns:config "urn:oasis:names:tc:opendocument:xmlns:config:1.0" xmlns:ooo "http://openoffice.org/2004/office" xmlns:ooow "http://openoffice.org/2004/writer" xmlns:oooc "http://openoffice.org/2004/calc" xmlns:dom "http://www.w3.org/2001/xml-events" xmlns:xforms "http://www.w3.org/2002/xforms" xmlns:xsd "http://www.w3.org/2001/XMLSchema" xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance" xmlns:field "urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:field:1.0" office:version "1.1" office:mimetype "application/vnd.oasis.opendocument.spreadsheet")
    <office:body
      <office:spreadsheet
       ,(render-ods component)>>>))
