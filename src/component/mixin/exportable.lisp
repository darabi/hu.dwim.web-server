;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Exportable component

(def (component e) exportable/abstract ()
  ())

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

(def layered-method export-text ((component exportable/abstract))
  (render-text component))

;;;;;;
;;; CSV format

(def layered-method export-csv ((component exportable/abstract))
  (render-csv component))

;;;;;;
;;; ODT format

(def layered-method export-odt ((component exportable/abstract))
  (render-odt component))

;;;;;;
;;; ODS format

(def layered-method export-ods ((component exportable/abstract))
  (emit-xml-prologue)
  <office:document (xmlns:office "urn:oasis:names:tc:opendocument:xmlns:office:1.0" xmlns:style "urn:oasis:names:tc:opendocument:xmlns:style:1.0" xmlns:text "urn:oasis:names:tc:opendocument:xmlns:text:1.0" xmlns:table "urn:oasis:names:tc:opendocument:xmlns:table:1.0" xmlns:draw "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0" xmlns:fo "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0" xmlns:xlink "http://www.w3.org/1999/xlink" xmlns:dc "http://purl.org/dc/elements/1.1/" xmlns:meta "urn:oasis:names:tc:opendocument:xmlns:meta:1.0" xmlns:number "urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0" xmlns:presentation "urn:oasis:names:tc:opendocument:xmlns:presentation:1.0" xmlns:svg "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0" xmlns:chart "urn:oasis:names:tc:opendocument:xmlns:chart:1.0" xmlns:dr3d "urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0" xmlns:math "http://www.w3.org/1998/Math/MathML" xmlns:form "urn:oasis:names:tc:opendocument:xmlns:form:1.0" xmlns:script "urn:oasis:names:tc:opendocument:xmlns:script:1.0" xmlns:config "urn:oasis:names:tc:opendocument:xmlns:config:1.0" xmlns:ooo "http://openoffice.org/2004/office" xmlns:ooow "http://openoffice.org/2004/writer" xmlns:oooc "http://openoffice.org/2004/calc" xmlns:dom "http://www.w3.org/2001/xml-events" xmlns:xforms "http://www.w3.org/2002/xforms" xmlns:xsd "http://www.w3.org/2001/XMLSchema" xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance" xmlns:field "urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:field:1.0" office:version "1.1" office:mimetype "application/vnd.oasis.opendocument.spreadsheet")
  <office:body
    <office:spreadsheet
     ,(render-ods component)>>>)
