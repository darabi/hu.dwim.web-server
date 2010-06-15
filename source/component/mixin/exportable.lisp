;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; exportable/component

(def (component e) exportable/component ()
  ())

(def layered-method make-export-commands ((component exportable/component) class prototype value)
  (optional-list (make-export-command :txt component class prototype value)
                 (make-export-command :csv component class prototype value)
                 (make-export-command :pdf component class prototype value)
                 (make-export-command :ods component class prototype value)
                 (make-export-command :odt component class prototype value)
                 (make-export-command :sh component class prototype value)))

(def (layered-function e) export-file-name (format component value)
  (:method :around (format component value)
    (awhen (call-next-layered-method)
      (string+ it "." (string-downcase format))))

  (:method (format component value)
    (lookup-first-matching-resource* (:default "unnamed")
      ("export.default-filename" (string-downcase format))
      "export.default-filename")))

(def macro with-output-to-export-stream ((stream-name &key external-format content-type file-name) &body body)
  (with-unique-names (response-body)
    (once-only (content-type)
      `(bind ((,response-body (with-output-to-sequence (,stream-name :external-format ,external-format
                                                                     :initial-buffer-size 256)
                                ,@body)))
         (make-byte-vector-response* ,response-body :headers (nconc (when ,content-type
                                                                      (list (cons +header/content-type+ ,content-type)))
                                                                    (list (cons +header/content-disposition+
                                                                                (make-content-disposition-header-value
                                                                                 :file-name ,file-name)))))))))

;;;;;;
;;; Text format

(def layered-method export-text ((self exportable/component))
  (with-output-to-export-stream (*text-stream* :content-type +text-mime-type+ :external-format (guess-encoding-for-http-response))
    (with-active-layers (passive-layer)
      (render-text self))))

(def layered-function write-text-line-begin ()
  (:method ()
    (values)))

(def layered-function write-text-line-separator ()
  (:method ()
    (terpri *text-stream*)))

;;;;;;
;;; CSV format

(def layered-method export-csv ((self exportable/component))
  (with-output-to-export-stream (*csv-stream* :content-type +csv-mime-type+ :external-format (guess-encoding-for-http-response))
    (with-active-layers (passive-layer)
      (render-csv self))))

;;;;;;
;;; ODT format

(def with-macro* with-xml-document-header/open-document-format (stream &key (encoding (guess-encoding-for-http-response)) mime-type)
  (emit-xml-prologue :encoding encoding :stream stream :version "1.0")
  <office:document (xmlns:office "urn:oasis:names:tc:opendocument:xmlns:office:1.0"
                    xmlns:style "urn:oasis:names:tc:opendocument:xmlns:style:1.0"
                    xmlns:text "urn:oasis:names:tc:opendocument:xmlns:text:1.0"
                    xmlns:table "urn:oasis:names:tc:opendocument:xmlns:table:1.0"
                    xmlns:draw "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0"
                    xmlns:fo "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0"
                    xmlns:xlink "http://www.w3.org/1999/xlink"
                    xmlns:dc "http://purl.org/dc/elements/1.1/"
                    xmlns:meta "urn:oasis:names:tc:opendocument:xmlns:meta:1.0"
                    xmlns:number "urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0"
                    xmlns:presentation "urn:oasis:names:tc:opendocument:xmlns:presentation:1.0"
                    xmlns:svg "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0"
                    xmlns:chart "urn:oasis:names:tc:opendocument:xmlns:chart:1.0"
                    xmlns:dr3d "urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0"
                    xmlns:math "http://www.w3.org/1998/Math/MathML"
                    xmlns:form "urn:oasis:names:tc:opendocument:xmlns:form:1.0"
                    xmlns:script "urn:oasis:names:tc:opendocument:xmlns:script:1.0"
                    xmlns:config "urn:oasis:names:tc:opendocument:xmlns:config:1.0"
                    xmlns:ooo "http://openoffice.org/2004/office"
                    xmlns:ooow "http://openoffice.org/2004/writer"
                    xmlns:oooc "http://openoffice.org/2004/calc"
                    xmlns:dom "http://www.w3.org/2001/xml-events"
                    xmlns:xforms "http://www.w3.org/2002/xforms"
                    xmlns:xsd "http://www.w3.org/2001/XMLSchema"
                    xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance"
                    xmlns:rpt "http://openoffice.org/2005/report"
                    xmlns:of "urn:oasis:names:tc:opendocument:xmlns:of:1.2"
                    xmlns:rdfa "http://docs.oasis-open.org/opendocument/meta/rdfa#"
                    xmlns:field "urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0"
                    xmlns:formx "urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0"
                    office:version "1.2"
                    office:mimetype ,mime-type)
    <office:meta
      <meta:generator "http://dwim.hu/project/hu.dwim.wui">
      <meta:creation-date ()
        ,(local-time:format-rfc3339-timestring nil (local-time:now))>>
    <office:font-face-decls
      <style:font-face (style:font-pitch "variable" style:font-family-generic "roman" svg:font-family "'Times New Roman'" style:name "Times New Roman")>
      <style:font-face (style:font-pitch "variable" style:font-family-generic "swiss" svg:font-family "Arial" style:name "Arial")>>
    <office:styles
      <style:style (style:class "text" style:family "paragraph" style:name "Standard")>
      <style:style (style:class "text" style:parent-style-name "Standard" style:family "paragraph" style:name "Heading")
        <style:paragraph-properties (fo:keep-with-next "always" fo:margin-bottom "0.0835in" fo:margin-top "0.1665in")>
        <style:text-properties (style:font-size-complex "14pt" style:font-name-complex "Tahoma" style:font-size-asian "14pt" style:font-name-asian "DejaVu Sans" fo:font-size "14pt" style:font-name "Arial")>>
      <style:style (style:class "text" style:default-outline-level "1" style:parent-style-name "Heading" style:family "paragraph" style:display-name "Heading 1" style:name "Heading.1")
        <style:text-properties (style:font-weight-complex "bold" style:font-size-complex "16pt" style:font-weight-asian "bold" style:font-size-asian "14pt" fo:font-weight "bold" fo:font-size "16pt")>>
      <style:style (style:class "text" style:default-outline-level "2" style:parent-style-name "Heading" style:family "paragraph" style:display-name "Heading 2" style:name "Heading.2")
        <style:text-properties (style:font-weight-complex "bold" style:font-style-complex "italic" style:font-size-complex "14pt" style:font-weight-asian "bold" style:font-style-asian "italic" style:font-size-asian "14pt" fo:font-weight "bold" fo:font-style "italic" fo:font-size "14pt")>>
      <style:style (style:class "text" style:default-outline-level "3" style:parent-style-name "Heading" style:family "paragraph" style:display-name "Heading 3" style:name "Heading.3")
        <style:text-properties (style:font-weight-complex "bold" style:font-size-complex "14pt" style:font-weight-asian "bold" style:font-size-asian "14pt" fo:font-weight "bold" fo:font-size "13pt")>>
      <style:style (style:class "text" style:parent-style-name "Standard" style:family "paragraph" style:name "Code")
        <style:text-properties (style:font-name "Courier New")>>>
    <office:master-styles
      <style:master-page (style:page-layout-name "pm1" style:name "Standard")>>
    <office:body
      ,(-with-macro/body-)>>)

(def layered-method export-odt ((self exportable/component))
  (bind ((encoding (guess-encoding-for-http-response)))
    (with-output-to-export-stream (*xml-stream* :content-type +odt-mime-type+ :external-format encoding)
      (with-xml-document-header/open-document-format (*xml-stream* :encoding encoding :mime-type +odt-mime-type+)
        <office:text
          ,(with-active-layers (passive-layer)
             (render-odt self))>))))

;;;;;;
;;; ODS format

(def layered-method export-ods ((self exportable/component))
  (bind ((encoding (guess-encoding-for-http-response)))
    (with-output-to-export-stream (*xml-stream* :content-type +ods-mime-type+ :external-format encoding)
      (with-xml-document-header/open-document-format (*xml-stream* :encoding encoding :mime-type +ods-mime-type+)
        <office:spreadsheet
         ;; TODO i think this table:table should be deleted. check if there can be multiple instances of them and what it means.
         ;; based on that decide where to render it...
         <table:table
           ,(with-active-layers (passive-layer)
              (render-ods self))>>))))

;;;;;;
;;; SH format

(def (layered-method e) export-sh ((self component))
  (with-output-to-export-stream (*text-stream* :content-type +text-mime-type+ :external-format (guess-encoding-for-http-response))
    (with-active-layers (passive-layer)
      (render-sh self))))
