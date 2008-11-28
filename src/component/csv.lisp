;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Command

(def icon export-csv "static/wui/icons/20x20/document.png")

(def resources hu
    (icon-label.export-csv "CSV")
  (icon-tooltip.export-csv "A tartalom mentése CSV formátumban"))

(def resources en
    (icon-label.export-csv "CSV")
  (icon-tooltip.export-csv "Export content in CSV format"))

(def special-variable *csv-stream*)

(def function make-export-csv-command (component)
  (command (icon export-csv)
           (make-action
             (make-raw-functional-response ((+header/content-type+ +csv-mime-type+))
               (send-headers *response*)
               (bind ((*csv-stream* (network-stream-of *request*)))
                 (execute-export-csv component))))))

(def (layered-function e) execute-export-csv (component)
  (:method ((component component))
    (render-csv component)))

;;;;;;
;;; Render

(def (constant :test #'equal) +whitespace-chars+ '(#\Space #\Tab #\Linefeed #\Return #\Page))

(def constant +csv-quote-char+ #\")

(def constant +csv-value-separator+ #\,)

(def constant +csv-line-separator+ #\NewLine)

(def function whitespacep (char)
  (member char +whitespace-chars+ :test #'char=))

(def function escaped-csv-char-p (char)
  (member char '(#.+csv-value-separator+ #.+csv-line-separator+ #.+csv-quote-char+) :test #'char=))

(def function escape-csv-value (value)
  (with-output-to-string (escaped-value)
    (write-char +csv-quote-char+ escaped-value)
    (iter (for char :in-sequence value)
          (if (char= char +csv-quote-char+)
              (iter (repeat 2)
                    (write-char +csv-quote-char+ escaped-value))
              (write-char char escaped-value)))
    (write-char +csv-quote-char+ escaped-value)))

(def function render-csv-value (value)
  (if (or (string= value "")
          (whitespacep (elt value 0))
          (whitespacep (elt value (1- (length value))))
          (find-if #'escaped-csv-char-p value))
      (write-string (escape-csv-value value) *csv-stream*)
      (write-string value *csv-stream*)))

(def function render-csv-value-separator ()
  (write-char +csv-value-separator+ *csv-stream*))

(def function render-csv-line-separator ()
  (write-char +csv-line-separator+ *csv-stream*))

(def function render-csv-line (elements)
  (iter (for element :in elements)
        (unless (first-iteration-p)
          (render-csv-value-separator))
        (render-csv element)))
