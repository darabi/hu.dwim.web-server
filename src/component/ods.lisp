;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Export

(enable-quasi-quoted-xml-to-binary-emitting-form-syntax 'hu.dwim.wui::*ods-stream* :encoding :utf-8 :with-inline-emitting #t)

(def layered-method execute-export-ods :around ((component component))
  (emit-xml-prologue :stream *ods-stream*)
  <office:document (xmlns:office "urn:oasis:names:tc:opendocument:xmlns:office:1.0" xmlns:style "urn:oasis:names:tc:opendocument:xmlns:style:1.0" xmlns:text "urn:oasis:names:tc:opendocument:xmlns:text:1.0" xmlns:table "urn:oasis:names:tc:opendocument:xmlns:table:1.0" xmlns:draw "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0" xmlns:fo "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0" xmlns:xlink "http://www.w3.org/1999/xlink" xmlns:dc "http://purl.org/dc/elements/1.1/" xmlns:meta "urn:oasis:names:tc:opendocument:xmlns:meta:1.0" xmlns:number "urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0" xmlns:presentation "urn:oasis:names:tc:opendocument:xmlns:presentation:1.0" xmlns:svg "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0" xmlns:chart "urn:oasis:names:tc:opendocument:xmlns:chart:1.0" xmlns:dr3d "urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0" xmlns:math "http://www.w3.org/1998/Math/MathML" xmlns:form "urn:oasis:names:tc:opendocument:xmlns:form:1.0" xmlns:script "urn:oasis:names:tc:opendocument:xmlns:script:1.0" xmlns:config "urn:oasis:names:tc:opendocument:xmlns:config:1.0" xmlns:ooo "http://openoffice.org/2004/office" xmlns:ooow "http://openoffice.org/2004/writer" xmlns:oooc "http://openoffice.org/2004/calc" xmlns:dom "http://www.w3.org/2001/xml-events" xmlns:xforms "http://www.w3.org/2002/xforms" xmlns:xsd "http://www.w3.org/2001/XMLSchema" xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance" xmlns:field "urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:field:1.0" office:version "1.1" office:mimetype "application/vnd.oasis.opendocument.spreadsheet")
    <office:body
      <office:spreadsheet
        ,(call-next-method)>>>)

;;;;;;
;;; Render ods

(def render-ods string
  <text:p ,-self- >)

(def render-ods primitive-component
  <text:p ,(print-component-value -self-) >)

(def render-ods label-component
  (render-ods (component-value-of -self-)))

(def render-ods content-component
  (render-ods (content-of -self-)))

(def render-ods table-component
  <table:table
    <table:table-row ,(foreach #'render-ods (columns-of -self-))>
    ,(foreach #'render-ods (rows-of -self-))>)

(def render-ods row-component
  <table:table-row ,(bind ((table (parent-component-of -self-)))
                          (foreach (lambda (cell column)
                                     (render-ods-table-cell table -self- column cell))
                                   (cells-of -self-)
                                   (columns-of table)))>)

(def (layered-function e) render-ods-table-cell (table row column cell)
  (:method :before ((table table-component) (row row-component) (column column-component) (cell cell-component))
    (ensure-uptodate cell))

  (:method ((table table-component) (row row-component) (column column-component) (cell component))
    <table:table-cell ,(render-ods cell)>)

  (:method ((table table-component) (row row-component) (column column-component) (cell string))
    <table:table-cell ,(render-ods cell)>)
  
  (:method ((table table-component) (row row-component) (column column-component) (cell cell-component))
    (render-ods cell)))

(def render-ods tree-component
  <table:table
    <table:table-row ,(foreach #'render-ods (columns-of -self-))>
    ,(foreach #'render-ods (root-nodes-of -self-))>)

(def render-ods node-component
    <table:table-row ,(foreach (lambda (column cell)
                                 (render-ods-tree-cell *tree* -self- column cell))
                               (columns-of *tree*)
                               (cells-of -self-))>
    (awhen (child-nodes-of -self-)
      <table:table-row-group ,(foreach #'render-ods (child-nodes-of -self-))>))

(def render-ods column-component
  <table:table-cell ,(call-next-method)>)

(def render-ods cell-component
  <table:table-cell ,(call-next-method)>)

(def (layered-function e) render-ods-tree-cell (tree node column cell)
  (:method :before ((tree tree-component) (node node-component) (column column-component) (cell cell-component))
    (ensure-uptodate cell))

  (:method ((tree tree-component) (node node-component) (column column-component) (cell component))
    <table:table-cell ,(render-ods cell)>)

  (:method ((tree tree-component) (node node-component) (column column-component) (cell string))
    <table:table-cell ,(render-ods cell)>)
  
  (:method ((tree tree-component) (node node-component) (column column-component) (cell cell-component))
    (render-ods cell)))

(def render-ods icon-component
  (render-ods (force (label-of -self-))))

(def render-ods alternator-component
    (render-ods (content-of -self-)))

(def render-ods reference-component
  (render-ods (expand-command-of -self-)))

(def render-ods standard-object-detail-component
  )

(def render-ods standard-object-slot-value-group-component
  )

(def render-ods standard-object-slot-value-component
  )

