;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Time provider

(def component time-provider (content-component)
  ((time :type prc::timestamp)))

(def component-environment time-provider
  (prc::call-with-time (time-of -self-) #'call-next-method))

;;;;;;
;;; Time selector

(def component time-selector (timestamp-inspector)
  ()
  (:default-initargs :edited #t))

(def (macro e) time-selector (time)
  `(make-instance 'time-selector :component-value ,time))

;;;;;;
;;; Validity selector

(def component validity-selector (member-inspector)
  ()
  (:default-initargs :edited #t :possible-values '(2007 2008 2009) :client-name-generator [integer-to-string !2]))

(def (macro e) validity-selector (&key validity)
  `(make-instance 'validity-selector
                  :component-value ,(if (stringp validity)
                                        (parse-integer validity)
                                        validity)))

(def function compute-timestamp-range (component)
  (bind (((:read-only-slots single) component))
    (if single
        (bind ((partial-timestamp-string (princ-to-string (component-value-of (range-of component)))))
          (list (prc::first-moment-for-partial-timestamp partial-timestamp-string)
                (prc::last-moment-for-partial-timestamp partial-timestamp-string)))
        (not-yet-implemented))))

;;;;;;
;;; Validity provider

(def component validity-provider (content-component)
  ((selector :type component)))

(def (macro e) validity-provider ((&key validity) &body forms)
  `(make-instance 'validity-provider
                  :content (progn ,@forms)
                  :selector (validity-selector :validity ,validity)))

(def render validity-provider ()
  <div ,(render (selector-of -self-))
       ,(call-next-method) >)

(def component-environment validity-provider
  (bind ((year (component-value-of (selector-of -self-))))
    (prc::call-with-validity-range (local-time:encode-timestamp 0 0 0 0 1 1 year :offset 0)
                                   (local-time:encode-timestamp 0 0 0 0 1 1 (1+ year) :offset 0)
                                   #'call-next-method)))

;;;;;;
;;; Coordinates provider

(def component coordinates-provider (content-component)
  ((dimensions)
   (coordinates)))

(def (macro e) coordinates-provider (dimensions coordinates &body content)
  `(make-instance 'coordinates-provider
                  :dimensions (mapcar #'prc:lookup-dimension ,dimensions)
                  :coordinates ,coordinates
                  :content (progn ,@content)))

(def component-environment coordinates-provider
  (bind ((dimensions (dimensions-of -self-))
         (coordinates (force (coordinates-of -self-))))
    (cl-perec:with-coordinates dimensions coordinates
      (with-error-log-decorator (error-log-decorator
                                  (format t "~%The environment of the coordinates-provider ~A follows:" -self-)
                                  (iter (for dimension :in dimensions)
                                        (for coordinate :in coordinates)
                                        (format t "~%  ~S: ~@<~A~:>" (cl-perec:name-of dimension) coordinate)))
        (call-next-method)))))

;;;;;;
;;; Coordinates dependent component mixin

(def component coordinates-dependent-component-mixin ()
  ((dimensions)
   (coordinates)))

(def constructor coordinates-dependent-component-mixin ()
  (with-slots (dimensions coordinates) -self-
    (setf dimensions (mapcar 'prc:lookup-dimension dimensions)
          coordinates (prc:make-empty-coordinates dimensions))))

(def (function e) print-object/coordinates-dependent-component-mixin (self)
  (princ "dimensions: ")
  (princ (if (slot-boundp self 'dimensions)
             (dimensions-of self)
             "<unbound>"))
  (princ ", coordinates: ")
  (princ (if (slot-boundp self 'coordinates)
             (coordinates-of self)
             "<unbound>")))

(def render :before coordinates-dependent-component-mixin ()
  (setf (coordinates-of -self-)
        (iter (for dimension :in (dimensions-of -self-))
              (for old-coordinate :in (coordinates-of -self-))
              (for new-coordinate = (prc:coordinate dimension))
              (unless (prc:coordinate-equal dimension (ensure-list old-coordinate) (ensure-list new-coordinate))
                (mark-outdated -self-))
              (collect new-coordinate))))

(def (function e) coordinates-bound-according-to-dimension-type-p (component)
  (iter (for dimension :in (dimensions-of component))
        (always (etypecase dimension
                  (prc::inheriting-dimension #t)
                  (prc::ordering-dimension #t)
                  (prc::dimension
                   (typep (prc::coordinate dimension) (prc::the-type-of dimension)))))))

;;;;;;
;;; D value inspector reference

(def component d-value-inspector-reference (reference-component)
  ())

(def method make-reference-label ((reference d-value-inspector-reference) class (instance prc::d-value))
  (if (prc::single-d-value-p instance)
      (make-reference-label reference class (prc::single-d-value instance))
      (concatenate-string (write-to-string (length (prc::c-values-of instance))) " values")))

;;;;;
;;; Abstract d value component

(def component abstract-d-value-component (abstract-standard-object-component)
  ())

(def layered-method render-title ((self abstract-d-value-component))
  `xml,"Dimenzionális érték")

;;;;;;
;;; D value inspector

(def component d-value-inspector (abstract-d-value-component
                                  inspector-component
                                  alternator-component
                                  user-message-collector-component-mixin
                                  remote-identity-component-mixin
                                  initargs-component-mixin
                                  layer-context-capturing-component-mixin
                                  recursion-point-component)
  ()
  (:default-initargs :alternatives-factory #'make-d-value-inspector-alternatives)
  (:documentation "Inspector for a D-VALUE instance in various alternative views."))

(def method refresh-component ((self d-value-inspector))
  (bind (((:slots instance default-alternative-type alternatives content command-bar) self)
         (class (find-class 'prc::d-value)))
    (if alternatives
        (setf (component-value-for-alternatives self) instance)
        (setf alternatives (funcall (alternatives-factory-of self) self instance)))
    (if content
        (setf (component-value-of content) instance)
        (setf content (if default-alternative-type
                          (find-alternative-component alternatives default-alternative-type)
                          (find-default-alternative-component alternatives))))
    (setf command-bar (make-alternator-command-bar self alternatives
                                                   (make-standard-commands self class (class-prototype class))))))

;; TODO: factor this out all over the place
(def render d-value-inspector ()
  (bind (((:read-only-slots id content) -self-))
    (flet ((body ()
             (render-user-messages -self-)
             (call-next-method)))
      (if (typep content 'reference-component)
          <span (:id ,id :class "d-value-inspector")
            ,(body)>
          (progn
            <div (:id ,id :class "d-value-inspector")
              ,(body)>
            `js(wui.setup-widget "d-value-inspector" ,id))))))

(def (layered-function e) make-d-value-inspector-alternatives (component instance)
  (:method ((component d-value-inspector) (instance prc::d-value))
    (list (delay-alternative-component-with-initargs 'd-value-table-inspector :instance instance)
          (delay-alternative-component-with-initargs 'd-value-pivot-table-component :instance instance)
          (delay-alternative-component-with-initargs 'd-value-column-chart-component :instance instance)
          (delay-alternative-component-with-initargs 'd-value-pie-chart-component :instance instance)
          (delay-alternative-component-with-initargs 'd-value-line-chart-component :instance instance)
          (delay-alternative-reference-component 'd-value-inspector-reference instance))))

(def function localized-dimension-name (dimension)
  (bind ((name (string-downcase (prc::name-of dimension))))
    (lookup-first-matching-resource
      ("dimension-name" name))))

;;;;;;
;;; D value table inspector

(def component d-value-table-inspector (abstract-d-value-component
                                        inspector-component
                                        table-component
                                        title-component-mixin)
  ())

(def method refresh-component ((self d-value-table-inspector))
  (bind (((:slots instance columns rows) self)
         (dimensions (prc::dimensions-of instance)))
    (setf columns (cons (column "Value") (mapcar [column (localized-dimension-name !1)] dimensions)))
    (setf rows (iter (for (coordinates value) :in-d-value instance)
                     (collect (make-instance 'd-value-row-inspector
                                             :value value
                                             :coordinates coordinates
                                             :instance instance))))))

;;;;;;
;;; D value row inspector

(def component d-value-row-inspector (abstract-d-value-component
                                      inspector-component
                                      row-component)
  ((value)
   (coordinates)))

(def method refresh-component ((self d-value-row-inspector))
  (bind (((:slots value coordinates instance cells) self))
    (setf cells (cons (make-viewer value)
                      (mapcar 'make-coordinate-inspector (prc::dimensions-of instance) coordinates)))))

(def function make-coordinate-inspector (dimension coordinate)
  (if (typep dimension 'prc::ordering-dimension)
      (make-coordinate-range-inspector coordinate)
      (make-viewer (if (length= 1 coordinate)
                       (first coordinate)
                       coordinate)
                   :default-alternative-type 'reference-component)))

(def function make-coordinate-range-inspector (coordinate)
  ;; TODO: KLUDGE: this is really much more complex than this
  (bind ((begin (prc::coordinate-range-begin coordinate))
         (end (prc::coordinate-range-end coordinate)))
    (if (local-time:timestamp= begin end)
        ;; single moment of time
        (localized-timestamp begin)
        (local-time:with-decoded-timestamp (:day day-begin :month month-begin :year year-begin :timezone local-time:+utc-zone+) begin
          (local-time:with-decoded-timestamp (:day day-end :month month-end :year year-end :timezone local-time:+utc-zone+) end
            (cond
              ;; whole year
              ((and (= 1 day-begin)
                    (= 1 month-begin)
                    (= 1 day-end)
                    (= 1 month-end)
                    (= 1 (- year-end year-begin)))
               (integer-to-string year-begin))
              ;; TODO: range of years
              ;; whole month
              ((and (= 1 day-begin)
                    (= 1 day-end)
                    (= year-end year-begin)
                    (= 1 (- month-end month-begin)))
               (localize-month-name (1- month-begin)))
              ;; range of months
              ((and (= 1 day-begin)
                    (= 1 day-end)
                    (or (= year-end year-begin)
                        (and (= 1 (- year-end year-begin))
                             (= 1 month-end))))
               (concatenate-string (localize-month-name (1- month-begin)) " - " (localize-month-name (mod (- month-end 2) 12))))
              ;; TODO: whole day
              ;; TODO: range of days
              (t
               (concatenate-string (localized-timestamp begin) " - " (localized-timestamp end)))))))))

;;;;;;
;;; D value pivot table

(def component d-value-pivot-table-component (abstract-d-value-component
                                              pivot-table-component
                                              title-component-mixin)
  ((cell-component-type 'abstract-d-value-chart-component :type (member abstract-d-value-chart-component d-value-pie-chart-component d-value-column-chart-component d-value-inspector))))

(def method refresh-component :before ((self d-value-pivot-table-component))
  (bind (((:read-only-slots instance) self))
    (iter (for (axes-slot-name . dimension-names) :in (collect-pivot-table-dimension-axes-groups self (class-of instance) instance))
          (dolist (dimension-name dimension-names)
            (bind ((dimension (prc::find-dimension dimension-name)))
              (aif (find-pivot-table-dimension-axis self dimension)
                   (setf (component-value-of it) instance)
                   (appendf (slot-value self axes-slot-name)
                            (list (make-instance 'pivot-table-dimension-axis-component :dimension dimension :instance instance)))))))))

(def layered-method collect-standard-object-detail-inspector-slots ((component standard-object-detail-inspector) (class component-class) (instance d-value-pivot-table-component))
  (list* (find-slot 'd-value-pivot-table-component 'cell-component-type) (call-next-method)))

(def (generic e) collect-pivot-table-dimension-axes-groups (component class instance)
  (:method ((component d-value-pivot-table-component) (class standard-class) (instance prc::d-value))
    (bind (((:read-only-slots instance) component)
           (dimensions (prc::dimensions-of instance))
           (split-position (floor (/ (length dimensions) 2))))
      (list (cons 'row-axes (subseq dimensions 0 split-position))
            (cons 'column-axes (subseq dimensions split-position))))))

(def function find-pivot-table-dimension-axis (pivot-table dimension)
  (bind (((:read-only-slots sheet-axes row-axes column-axes cell-axes) pivot-table))
    (some [find dimension !1 :key #'dimension-of] (list sheet-axes row-axes column-axes cell-axes))))

(def render d-value-pivot-table-component
  <div (:class "d-value-pivot-table")
       ,(render-title -self-)
       ,(call-next-method)>)

(def method make-pivot-table-extended-table-component ((self d-value-pivot-table-component) sheet-path)
  (bind (((:slots instance row-axes column-axes) self))
    (prog1-bind extended-table (call-next-method)
      (setf (cells-of extended-table)
            (bind ((result nil))
              (flet ((map-product* (function &rest lists)
                       (when lists
                         (apply #'map-product function (car lists) (cdr lists)))))
                (apply #'map-product*
                       (lambda (&rest row-path)
                         (apply #'map-product*
                                (lambda (&rest column-path)
                                  (push (make-d-value-pivot-table-cell self sheet-path row-path column-path instance) result))
                                (mapcar #'categories-of column-axes)))
                       (mapcar #'categories-of row-axes)))
              (nreverse result))))))

(def function make-d-value-pivot-table-cell (component sheet-path row-path column-path d-value)
  (bind ((path (append sheet-path row-path column-path))
         (dimensions (prc::dimensions-of d-value))
         (cell-dimensions nil)
         (coordinates (iter (for dimension :in dimensions)
                            (collect (aif (find dimension path :key #'dimension-of)
                                          (coordinate-of it)
                                          (progn
                                            (push dimension cell-dimensions)
                                            prc::+whole-domain-marker+)))))
         (value (prc::value-at-coordinates d-value coordinates)))
    ;; TODO: add type to d-value and pass it down
    (if (prc::single-d-value-p value)
        (aif (prc::single-d-value value)
             (make-viewer it)
             (empty))
        (if (prc::empty-d-value-p value)
            (empty)
            ;; TODO: is this remove-dimensions call dangerous?
            (make-viewer (prc::remove-dimensions value (set-difference dimensions cell-dimensions))
                         :default-alternative-type (cell-component-type-of component))))))

;;;;;;
;;; Dimension pivot table axis component

(def component pivot-table-dimension-axis-component (abstract-d-value-component
                                                     pivot-table-axis-component)
  ((dimension :type prc::dimension)))

(def method refresh-component ((self pivot-table-dimension-axis-component))
  (bind (((:slots dimension categories instance) self))
    (setf categories
          (mapcar (lambda (coordinate)
                    (make-instance 'coordinate-pivot-table-category-component
                                   :dimension dimension
                                   :coordinate coordinate
                                   :content (make-coordinate-inspector dimension coordinate)))
                  (prc::d-value-dimension-coordinate-list instance dimension :mode :intersection)))))

(def method localized-pivot-table-axis ((self pivot-table-dimension-axis-component))
  (localized-dimension-name (dimension-of self)))

(def layered-method collect-standard-object-list-table-inspector-slots ((component standard-object-list-table-inspector) (class component-class) (instance pivot-table-dimension-axis-component))
  (list* (find-slot 'pivot-table-dimension-axis-component 'dimension) (call-next-method)))

(def method make-reference-label ((reference reference-component) (class standard-class) (instance prc::dimension))
  (localized-dimension-name instance))

;;;;;;
;;; Coordinate pivot table category component

(def component coordinate-pivot-table-category-component (pivot-table-category-component)
  ((dimension :type prc::dimension)
   (coordinate :type t)))

;;;;;;
;;; Abstract d value chart component

(def component abstract-d-value-chart-component (abstract-d-value-component content-component)
  ())

(def function collect-d-value-names-and-values (component)
  (bind (((:slots chart-dimensions instance) component)
         (dimensions (prc::dimensions-of instance)))
    (iter (for (coordinates value) :in-d-value instance)
          (when value
            (collect (with-output-to-string (*standard-output*)
                       (map nil [render-string (make-coordinate-inspector !1 !2)] dimensions coordinates))
              :into names)
            (collect value :into values))
          (finally
           (return (values names values))))))

;;;;;;
;;; D value pie chart component

(def component d-value-pie-chart-component (abstract-d-value-chart-component)
  ())

(def method refresh-component ((self d-value-pie-chart-component))
  (bind (((:values names values) (collect-d-value-names-and-values self)))
    (setf (content-of self) (make-pie-chart :names names :values values))))

;;;;;;
;;; D value column chart component

(def component d-value-column-chart-component (abstract-d-value-chart-component)
  ())

(def method refresh-component ((self d-value-column-chart-component))
  (bind (((:values names values) (collect-d-value-names-and-values self)))
    (setf (content-of self) (make-column-chart :names names :values values))))

;;;;;;
;;; D value line chart component

(def component d-value-line-chart-component (abstract-d-value-chart-component)
  ())


;;;;;;
;;; Localization

(def resources hu
  (dimension-name.time "idő")
  (dimension-name.validity "hatályosság")

  (class-name.dimension "dimenzió")
  (class-name.pivot-table-dimension-axis-component "Dimenzió alapú pivot tábla tengely")

  (slot-name.dimension "dimenzió"))
