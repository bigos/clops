;;; clops, see the README.org

(declaim (optimize (speed 0) (safety 2) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(#:cl-gtk4 #:cl-gdk4 #:cl-glib #:cl-cairo2
                  #:serapeum #:defclass-std #:cl-data-structures)))

(defpackage #:checked-class
  (:use :cl)
  (:export
   :checked-class))

(defpackage #:tail-viewer
  (:use #:cl)
  (:import-from :serapeum :~>)
  (:import-from :defclass-std :defclass/std)
  (:export
   :box-div
   :build-first-box
   :build-box-class))

(defpackage #:clops
  (:use #:cl)
  (:import-from :serapeum :~>)
  (:import-from :defclass-std :defclass/std)
  (:export
   :%draw-func
   :*canvas*
   :*lisp-app*
   :box-div
   :build-box-class
   :de-focus-enter
   :de-focus-leave
   :de-key-pressed
   :de-key-released
   :de-menu-bool
   :de-menu-radio
   :de-menu-simple
   :de-motion
   :de-motion-enter
   :de-motion-leave
   :de-pressed
   :de-released
   :de-resize
   :de-scroll
   :de-timeout
   :gtk4-app
   :lisp-app
   :menu-bar-menu
   :window-add
   :window-remove))

(defpackage #:gui-menu
  (:use #:cl)
  (:export
   :build-menu :build-items
   :prepare-submenu :prepare-section :prepare-item-simple :prepare-item-bool
   :prepare-item-radio :prepare-radio-action))

(defpackage #:gui-window
  (:use #:cl)
  (:export
   :present-about-dialog :new-window-for-app :close-all-windows-and-quit
   :window))

;;; =================================== package checked-class ==================
(in-package #:checked-class)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ type checking in a class ~~~~~~~~~~~~~~~~~
;; https://stackoverflow.com/questions/51723992/how-to-force-slots-type-to-be-checked-during-make-instance/56920918

;;; First the metaclass:
;;; first a metaclass for classes which checks slot writes
(defclass checked-class (standard-class)
    ())

;;; this is a MOP method, probably use CLOSER-MOP for a portable version
(defmethod sb-mop:validate-superclass ((class checked-class) (superclass standard-class))
  t)

;; Now we check all slot writes for that metaclass:
;; this is a MOP method, probably use CLOSER-MOP for a portable version

(defmethod (setf sb-mop:slot-value-using-class) :before (new-value (class checked-class) object slot)
  (if (eq 'T (sb-mop:slot-definition-type slot))
      (when NIL
        (warn "debugging: skipping asserting ~S slot ~S"
              class
              (sb-mop:slot-definition-name slot)))
      (progn
        (when NIL
          (warn "debugging: asserting ~S slot ~S with type ~S"
                class
                (sb-mop:slot-definition-name slot)
                (sb-mop:slot-definition-type slot)))

        (assert (typep new-value (sb-mop:slot-definition-type slot))
                ()
                "new value ~s is not of type ~a in object ~a slot ~a"
                new-value
                (sb-mop:slot-definition-type slot)
                object
                (sb-mop:slot-definition-name slot)))))

(defun set-all-unbound-slots (instance &optional (value nil))
  (let ((class (class-of instance)))
    (sb-mop:finalize-inheritance class)
    (loop for slot in (sb-mop:compute-slots class)
          for name = (sb-mop:slot-definition-name slot)
          unless (slot-boundp instance name)
            do (setf (slot-value instance name) value))
    instance))

(defclass set-unbound-slots-mixin () ())

(defmethod initialize-instance :after ((i set-unbound-slots-mixin) &rest initargs)
  (declare (ignore initargs))
  (set-all-unbound-slots i nil)
  (call-next-method))

;;; ========================== package tail-viewer =============================
(in-package #:tail-viewer)

(defclass/std box-div (clops:box-div)
  ((file-path :std "/home/jacek/.bashrc"))
  (:metaclass checked-class:checked-class))

(defun build-box-class (parent class tlx tly brx bry)
  (clops:build-box-class parent class tlx tly brx bry))

;;; ========================== package clops ===================================
(in-package #:clops)

;;; ======================== important macros ==================================
(defmacro cond-step (test body)
    `(if ,test
         (progn
           (warn "going to debug the body")
           (step ,body))
         ,body))

;;; ====================== globals =============================================
(defparameter *model*  nil)
(defparameter *canvas* nil)
(defparameter *lisp-app* nil)
(defparameter *testing-current-window* nil)
(defparameter *break-on-key* nil)
(defparameter *id-counter* 0)
(defparameter *track-mouse-motion* nil)
(defun next-id () (incf *id-counter*))

;;; ====================== classes =============================================

(defclass/std hrel ()
  ((id)
   (parent)
   (children :std (make-hash-table))))

(defclass/std lisp-app ()
  ((gtk4-app :type gir::object-instance )
   (windows :std (make-hash-table)) ; see hasher, the hash keys is a symbol or integer
   (current-window :std nil :type (or null cffi:foreign-pointer keyword)))
  (:metaclass checked-class:checked-class))

(defclass/std lisp-window ()
  ((gir-window :type (or gir::object-instance keyword)
               :documentation "Either gir window or symbol used in test drawing")
   (children :documentation "Window can be divided into multiple sections with different content")
   (current-child :type (or null box))
   (hovered)
   (dimensions)
   (mouse-coordinates))
  (:metaclass checked-class:checked-class))
;;; window can be represented by 4 different classes
;;; clos object lisp-window
;;; gir object-instance
;;; gtk4 memory pointer - gir::this-of gir-object
;;; integer representing the pointer - cffi:pointer-address gtk4-memory-pointer

(defclass/std coordinates ()
  ((x :type integer)
   (y :type integer))
  (:metaclass checked-class:checked-class))

(defclass/std size ()
  ((x :type integer)
   (y :type integer))
  (:metaclass checked-class:checked-class))

(defclass/std box (hrel)
  ((top-left  :documentation "Relative Coordinates" :type coordinates)
   (width-height :documentation "Width and Height"  :type size))
  (:metaclass checked-class:checked-class))

(defclass/std box-status (box)
  ((status :std nil))
  (:metaclass checked-class:checked-class))

(defclass/std box-section (box-status)
  ()
  (:metaclass checked-class:checked-class))

(defclass/std box-div (box-status)
  ()
  (:metaclass checked-class:checked-class))

(defun build-box (parent tlx tly width height)
  (build-box-class parent 'box tlx tly width height))

(defun build-box-class (parent class tlx tly width height)
  ;; build box at TLX TLY relative to PARENT and of WIDTH and HEIGHT
  (let ((new-box (make-instance class :parent parent
                                :id (next-id)
                                ;; TODO make the coordinates relative to parent
                                :top-left  (make-instance 'coordinates
                                                          :x tlx
                                                          :y tly)
                                :width-height (make-instance 'size
                                                          :x (+ width)
                                                          :y (+ height)))))
    ;; ensure we use an instance derived from
    (assert (typep new-box 'box))
    (assert (not (null (id new-box))))
    (assert (<=  (~> new-box top-left x)
                 (~> new-box width-height x)))
    (assert (<=  (~> new-box top-left y)
                 (~> new-box width-height y)))
    ;; (break "examine the new-box")
    (etypecase parent
      (lisp-window (progn
                     (assert (typep new-box 'box-section))
                     (push new-box (children parent))))
      (box     (add-child parent new-box))
      (box-div (add-child parent new-box)))))

;;; ============================ object printing ===============================
(defun print-object-inner (obj  stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a"
            (loop for sl in (sb-mop:class-slots (class-of obj))
                  for slot-name = (sb-mop:slot-definition-name sl)
                  collect (cons slot-name
                                (if (slot-boundp obj slot-name)
                                    (format nil "~S" (slot-value obj slot-name))
                                    (format nil "---unbound---" )))))))

(defmethod print-object ((obj coordinates) stream)
  (print-object-inner obj stream))

(defmethod print-object ((obj size) stream)
  (print-object-inner obj stream))

(defmethod print-object ((obj box) stream)
  (print-object-inner obj stream))

(defmethod print-object ((obj box-div) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s"
            (list
             :id (id obj)
             :parent-id (id (parent obj))
             :coordinates (list (top-left obj) (width-height obj))
             :status (status obj)
             :slot-names :hidden
             ;; (map 'list #'sb-mop:slot-definition-name  (sb-mop:class-slots (class-of obj)))
             ))))

;;; ============================= testing ======================================

;; must try this
;; (step (experiment))

(defun experiment ()

  (cond-step nil
             (progn
               (setf *id-counter* 0
                     *track-mouse-motion* T)

               (setf *lisp-app* (make-instance 'lisp-app))
               (assert (zerop (hash-table-count (windows *lisp-app*))))

               (window-add *lisp-app* :secondary)
               (setf *testing-current-window* :secondary
                     (current-window *lisp-app*) :secondary)

               (window-add *lisp-app* :testme)
               (setf *testing-current-window* :testme
                     (current-window *lisp-app*) :testme)
               (assert (eq 2 (hash-table-count (windows *lisp-app*))))
               (assert (eq :testme *testing-current-window*))
               (assert (null (children (window-get *lisp-app* :testme))))
               (assert (eq 0 (length (children (window-get *lisp-app* :testme)))))))

  (cond-step nil (progn        ; we can limit stepping to the progn!!!
                   (apply 'de-key-pressed '("" "F2" 68 NIL))
                   (assert (eq 1 (length (children (window-get *lisp-app* :testme)))))
                   (assert (eq 1 (~> (window-get *lisp-app* :testme) children first id)))

                   ;; debugging box creation

                   (when nil
                     (let ((the-win (window-get *lisp-app* :testme)))
                       (break "try first box in window ~A" the-win)))

                   (apply 'de-key-pressed '("" "F3" 69 NIL))
                   (assert (eq 2 (length (children (window-get *lisp-app* :testme)))))
                   (assert (eq 2 (~> (window-get *lisp-app* :testme) children first id)))

                   (apply 'de-key-pressed '("" "F4" 69 NIL))
                   (assert (eq 2 (length (children (window-get *lisp-app* :testme)))))
                   (assert (eq 2 (~> (window-get *lisp-app* :testme) children first id)))
                   (assert (eq 3
                               (~> (window-get *lisp-app* :testme) children first children (gethash 3 _) id)))
                   (warn "the above assertion seems to pass")
                   ))

  ;; play with adding a child to the above box
  ;; use F4 to add child
  ;;  file:~/Programming/Lisp/lispy-experiments/clops.lisp::442
  ;;  file:~/Programming/Lisp/lispy-experiments/clops.lisp::116
  (cond-step t
             (progn

               (de-resize 800 600     :testme)
               (assert  (equal (cons 800 600) (dimensions (window-get *lisp-app* :testme))))

               (simulate-draw-func-key :testme)

               (assert  (equal nil (hovered (window-get *lisp-app* :testme))))
               (de-motion-enter 10 10 :testme)
               (de-motion 10 10 :testme)
               (assert  (equal T (hovered (window-get *lisp-app* :testme))))
               (assert  (equal (cons 10 10) (mouse-coordinates (window-get *lisp-app* :testme))))

               (simulate-draw-func-key :testme)
               (de-motion 95 95 :testme)
               (assert  (equal (cons 95 95) (mouse-coordinates (window-get *lisp-app* :testme))))

               (assert (eq :hovered (~> (window-get *lisp-app* :testme) children first status)))
               (assert (eq nil
                           (~> (window-get *lisp-app* :testme) children first children (gethash 3 _) status)))
               (simulate-draw-func-key :testme)

               (de-motion 75 75 :testme)
               (assert  (equal (cons 75 75) (mouse-coordinates (window-get *lisp-app* :testme))))
               (assert (eq :hovered (~> (window-get *lisp-app* :testme) children first status)))
               ;; it fails because we do not have proper relative absolute coordination system

               ;; (assert (eq :hovered
               ;;             (~> (window-get *lisp-app* :testme) children first children (gethash 3 _) status)))
               (simulate-draw-func-key :testme)

               (de-motion 70 70 :testme)
               (apply 'de-key-pressed '("" "F5" 71 NIL))
               (simulate-draw-func-key :testme)

               (de-motion 75 75 :testme)

               (apply 'de-key-pressed '("" "F7" 71 NIL))
               (simulate-draw-func-key :testme)
               (apply 'de-key-pressed '("" "F7" 71 NIL))
               (simulate-draw-func-key :testme)
               (apply 'de-key-pressed '("" "F7" 71 NIL))
               (simulate-draw-func-key :testme)

               (apply 'de-key-pressed '("" "F8" 71 NIL))
               (simulate-draw-func-key :testme)

               ))
  (simulate-draw-func-key :testme)

  (warn "the end of experiment")
  (setf *track-mouse-motion* nil))

;;; ======================== methods ===========================================

(defmethod initialize-instance :after ((hrel hrel) &key)
  (warn "created hrel with id ~s" (id hrel)))

(defmethod add-child ((parent hrel) (child hrel) )
  (setf (parent child) parent
        (gethash (id child) (children parent)) child))

(defmethod ancestors ((hrel hrel))
  (loop for p = (parent hrel) then (parent p)
        until (typep p 'lisp-window)
        collect p))

;;; TODO add chain of children with proper error checking
;; see above
;; (defun build-box-class zzz

(defun tryme ()
  (let ((parent (make-instance 'hrel :id (next-id))))
    (add-child parent (make-instance 'hrel :id (next-id)))
    (add-child parent (make-instance 'hrel :id (next-id)))
    (add-child parent (make-instance 'hrel :id (next-id)))
    parent))

;;; ------------------------ app methods ---------------------------------------
(defmethod current-lisp-window ((app lisp-app))
  "Get the lisp-window object from the current window pointer"
  (window-get app (current-window app)))

(defmethod window-get ((app lisp-app) (window sb-sys:system-area-pointer))
  (gethash (hasher window)
           (windows app)))
(defmethod window-get ((app lisp-app) (window gir::object-instance))
  (gethash (hasher window)
           (windows app)))
(defmethod window-get ((app lisp-app) (window symbol))
  (gethash (hasher window)
           (windows app)))

;;; ------------------------ window methods ------------------------------------
(defmethod top-left ((lisp-window lisp-window))
  (make-instance 'coordinates :x 0 :y 0))

(defmethod width-height ((lisp-window lisp-window))
  (make-instance 'size :x (~> lisp-window dimensions car)
                              :y (~> lisp-window dimensions cdr)))

(defmethod window-stats ((lisp-window lisp-window))
    (format nil "stats ~S"
            (list
             (floor (or (car (mouse-coordinates lisp-window)) 0.0))
             (floor (or (cdr (mouse-coordinates lisp-window)) 0.0))

             :eqptr-current-window

             (if (symbolp (gir-window lisp-window))
                 ;; testing drawing to file
                 (when (gir-window lisp-window)
                   (window-current-p *lisp-app* *testing-current-window*))

                 (when (gir-window lisp-window)
                   (window-current-p *lisp-app* (gir::this-of (gir-window lisp-window)))))

             (dimensions lisp-window)
             (children lisp-window)
             (if (hovered lisp-window)
                 "H"
                 "h")
             (mouse-coordinates lisp-window)))
  ;; add more
  )

;;; either the integer or testing keyword being the key of (gethash window (windows lisp-app))
(defmethod hasher ((window sb-sys:system-area-pointer))
  (cffi:pointer-address window))
(defmethod hasher ((window gir::object-instance))
  (cffi:pointer-address (gir::this-of window)))
(defmethod hasher ((window symbol))
  window)


(defmethod window-add ((app lisp-app) (window gir::object-instance))
    (assert (equal "ApplicationWindow"
                   (gir:info-get-name (gir::info-of (gir:gir-class-of window)))))

  (setf (gethash (hasher window)
                 (windows app))
        (make-instance 'lisp-window
                       :gir-window window)))
(defmethod window-add ((app lisp-app) (window symbol))
    (setf (gethash (hasher window)
                   (windows app))
          (make-instance 'lisp-window
                         :gir-window window)))

(defmethod window-remove ((app lisp-app) (window gir::object-instance))
  (if (remhash (hasher window) (windows app))
      (warn "success, window removed ~S" window)
      (warn "strange, window not removed ~S" window)))
(defmethod window-remove ((app lisp-app) (window symbol))
  (if (remhash (hasher window) (windows app))
      (warn "success, window removed ~S" window)
      (warn "strange, window not removed ~S" window)))

(defmethod window-hover ((app lisp-app) x y window)
  (assert (or (typep window 'gir::object-instance)
              (typep window 'keyword)))
  (setf (mouse-coordinates (window-get app window)) (cons x y))

  (loop for win being the hash-key of (windows app)
        for lisp-window = (gethash win (windows app))
        do (setf (hovered lisp-window) (eq
                                        window
                                        (gir-window lisp-window)))))

(defmethod window-unhover ((app lisp-app) window)
  (assert (or (typep window 'gir::object-instance)
              (typep window 'keyword)))
  (let ((lisp-window (window-get app window)))
    (when lisp-window              ; window may be removed on quitting
      (setf (mouse-coordinates lisp-window) nil
            (hovered lisp-window) nil))))

(defmethod window-motion ((app lisp-app) x y window)
  (assert (or (typep window 'gir::object-instance)
              (typep window 'keyword)))
  (setf (mouse-coordinates (window-get app window)) (cons x y))

  (let* ((lisp-window (window-get app window))
         (children (children lisp-window)))
    (setf (current-child lisp-window) nil)

    (loop for child in children
          do (setf (status child) nil)

             (when (mouse-over-box-p child x y)
               (format t "mouse over box ~S~%" child)

               (setf (current-child lisp-window) child
                     (status child) :hovered)
               (loop for c being the hash-value in (children child)
                     do (motion-child c x y))))))

;;; reported gtk window size also includes the menu bar we are tracking canvas dimensions
(defmethod window-resize ((app lisp-app)  width height window )
  (let ((lisp-window (window-get app window)))
    (when lisp-window              ; window may be removed on quitting
      (setf (dimensions lisp-window) (cons width height))
      ;; TODO resize children
      )))

(defmethod window-current-p ((app lisp-app) (windowptr sb-sys:system-area-pointer))
  (cffi:pointer-eq
   windowptr
   (current-window app)))
(defmethod window-current-p ((app lisp-app) (windowptr symbol))
  (eq
   windowptr
   (current-window app)))

;; NOT RIGHT
;; (defgeneric window-current-p (app windowptr)
;;   (:documentation "Check if windowptr points to current-window")
;;   ;; (#<LISP-APP {1003D2D1B3}> #.(SB-SYS:INT-SAP #X7FA5A440C280))
;;   (:method ((app lisp-window) (windowptr sb-sys:system-area-pointer))
;;     (eq
;;      windowptr
;;      (current-window app)))
;;   (:method ((app lisp-app) (windowptr symbol))
;;     (eq
;;      windowptr
;;      (current-window app))))

;;; ------------------------ box methods ------------------------------------
(defmethod width  ((box box))
  (~> box width-height x))
(defmethod height ((box box))
  (~> box width-height y))

(defmethod absolute-x ((box box))
  (+ (~> box top-left x)
     (loop for a in (ancestors box) sum (~> a top-left x))))

(defmethod absolute-y ((box box))
  (+ (~> box top-left y)
     (loop for a in (ancestors box) sum (~> a top-left y))))

(defmethod absolute-bx ((box box))
  (+ (absolute-x box)
     (~> box width-height x)))

(defmethod absolute-by ((box box))
  (+ (absolute-y box)
     (~> box width-height y)))

(defmethod mouse-over-box-p ((box box-status) x y)
  (warn "absolutes ~S" (list
                        (~> box absolute-x)
                        (~> box absolute-y)
                        (~> box absolute-bx)
                        (~> box absolute-by)))
  (and (and (>= x (~> box absolute-x))
            (<= x (~> box absolute-bx)))
       (and (>= y (~> box absolute-y))
            (<= y (~> box absolute-by)))))

(defmethod motion-child ((box box-status) x y)
  (format t "motioning over child ~S~%" (list x y box) )
  (if (mouse-over-box-p box x y)
      (progn
        (setf (status box) :hovered)
        (loop for c being the hash-value in (children box)
              do (motion-child c x y)))
      (setf (status box) nil)))

(defmethod render ((box box) parent)
  (warn "going to render box")
  (set-rgba "#FF000088")
  (cairo:move-to (x (top-left box)) (y (top-left box)))

  (cairo:rectangle
   (absolute-x box)
   (absolute-y box)
   (width box)
   (height box))

  (cairo:fill-path))

(defmethod render ((box box-status) parent)
  (warn "going to render box-status")
  (if (eq :hovered (status box))
      (set-rgba "#00FF0077")
      (set-rgba "#FF000088"))

  (cairo:move-to (x (top-left box)) (y (top-left box)))

  (cairo:rectangle
   (absolute-x box)
   (absolute-y box)
   (width box)
   (height box))

  (cairo:fill-path)

  (loop for c being the hash-value in (children box)
        do (render c box)))

(defmethod move-right  ((box box))
  (incf (~> box top-left  x) 5))

(defmethod move-left  ((box box))
  (incf (~> box top-left  x) -5))

;;; ================================= All Lisp Events ===========================
(defun le-change-sections  (window sections)   (process-event :change-sections window sections))

;;; ================================= All GUI Events ===========================

;;; these functions are for REPL convenience to play with handling the events
(defun de-menu-simple  (action-name)           (process-event :menu-simple action-name))
(defun de-menu-bool    (action-name bool01)    (process-event :menu-bool   action-name bool01))
(defun de-menu-radio   (action-name radiostr)  (process-event :menu-radio  action-name radiostr))
(defun de-motion       (x y window)            (process-event :motion       x y window))
(defun de-motion-enter (x y window)            (process-event :motion-enter x y window))
(defun de-focus-enter  (window)                (process-event :focus-enter  window))
(defun de-motion-leave (window)                (process-event :motion-leave window))
(defun de-focus-leave  (window)                (process-event :focus-leave  window))
(defun de-pressed      (button x y)            (process-event :pressed   button x y))
(defun de-released     (button x y)            (process-event :released  button x y))
(defun de-scroll       (dx dy)                 (process-event :scroll    dx dy))
(defun de-key-pressed  (letter name code mods) (process-event :key-pressed   letter name code mods))
(defun de-key-released (letter name code mods) (process-event :key-released  letter name code mods))
(defun de-resize       (width height window)   (process-event :resize width height window))
(defun de-timeout      ()                      (process-event :timeout))

;;; ======================= update and view ====================================

(let ((motion-count 0))
  (defun process-event (event &rest args)
    (let ((ignored-tracking-event (or (eq event :timeout)
                                      (and (eq event :motion)
                                           (null *track-mouse-motion*)))))

      (if (eq event :motion)
          (incf motion-count)
          (setf motion-count 0))

      (unless ignored-tracking-event
        (format t "~&processing event ~S ~S~%" event args))

      ;; (loop for awp being the hash-key in (windows *lisp-app*)
      ;;       for n = 0 then (1+ n)
      ;;       for awp-lisp-window = (gethash awp (windows *lisp-app*))
      ;;       do (format t "~A zzzzzzzzzz window ~A ~A~%" n awp (type-of awp)))

      ;; active window
      (let* ((appwindow
               (if (slot-boundp *lisp-app* 'gtk4-app)
                   (let ((gaaw (gtk4:application-active-window (gtk4-app *lisp-app*)) ))
                     (when gaaw
                       (gir::this-of gaaw)))
                   *testing-current-window*)))
        (unless (and appwindow
                     (current-window *lisp-app*)
                     (window-current-p *lisp-app* appwindow))
          (format t "~&======== different ====================================~%")
          (setf (current-window *lisp-app*) appwindow)))

      ;; like in Elm runtime calling the update

      (case event
        ;; GUI
        (:menu-simple (cond
                        ((string= (first args) "about")
                         (gui-window:present-about-dialog))
                        ((string= (first args) "new-window")
                         (window-add *lisp-app* (gui-window:new-window-for-app (gtk4-app *lisp-app*))))
                        ((string= (first args) "quit")
                         (gui-window:close-all-windows-and-quit))))
        (:menu-bool )
        (:menu-radio  )
        (:motion
         (apply #'window-motion *lisp-app* args))
        (:focus-enter)
        (:motion-enter
         (apply #'window-hover *lisp-app* args))
        (:focus-leave)
        (:motion-leave
         (window-unhover *lisp-app* (first args)))
        (:pressed )
        (:released )
        (:scroll )
        (:key-pressed (cond ((equal "Escape" (second args))
                             (setf *break-on-key* (not *break-on-key*))
                             (warn "pressed Escape ~S" *break-on-key*))
                            ((equal "F2" (second args))
                             (build-box-class (current-lisp-window *lisp-app*) 'box-section 0 0 50 50))
                            ((equal "F3" (second args))
                             (build-box-class (current-lisp-window *lisp-app*) 'box-section 50 50 50 50))
                            ((equal "F4" (second args))
                             (tail-viewer:build-box-class (~> (current-lisp-window *lisp-app*) children first)
                                                          'tail-viewer:box-div 10 10 30 30))
                            ((equal "F5" (second args))
                             ;; problems start with switch to hashes
                             (cond-step *testing-current-window*
                                        (let ((parent (~> (current-lisp-window *lisp-app*)
                                                          children
                                                          first
                                                          children
                                                          (gethash 3 _))))
                                          (clops:build-box-class parent 'box-div 5 5 20 20))))

                            ((equal "F7" (second args))
                             (let ((parent (~> (current-lisp-window *lisp-app*) children first)))
                               (move-right parent)))
                            ((equal "F8" (second args))
                             (let ((parent (~> (current-lisp-window *lisp-app*) children first)))
                               (move-left parent)))
                            (T
                             (warn "unhandled key press ~A" args))))
        (:key-released)
        (:resize
         (window-resize *lisp-app* (first args) (second args) (third args)))
        (:timeout))

      ;; inspired by Gtk4, think of separate from update layout phase
      ;; https://docs.gtk.org/gtk4/drawing-model.html
      ;; geometry changes calculated

      ;; like in Elm runtime calling the view
      (if T
          (loop for awp being the hash-key in (windows *lisp-app*)
                for awp-lisp-window = (gethash awp (windows *lisp-app*))
                for awp-gir-window = (gir-window awp-lisp-window)
                do
                   (if *testing-current-window*
                       ;; paint drawn windows to png file
                       (when nil
                         ;; atm we have nil because we call it in experiment, not in the event handling
                         (simulate-draw-func awp-lisp-window))


                       ;; canvas for awp that later will be passed and drawn as lisp-window
                       (gtk4:widget-queue-draw
                        (~> awp-gir-window gtk4:widget-first-child gtk4:widget-first-child))
                       ))))))

;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(cffi:defcstruct gdk-rgba
  (red   :float)
  (green :float)
  (blue  :float)
  (alpha :float))

(defun color-to-rgba (color)
  (cffi:with-foreign-object (rgba '(:struct gdk-rgba))
    (let ((pointer (make-instance 'gir::struct-instance
                                  :class (gir:nget gdk::*ns* "RGBA")
                                  :this rgba)))
      (let ((valid-color (gdk:rgba-parse pointer color)))
        (cffi:with-foreign-slots ((red green blue alpha) rgba (:struct gdk-rgba))
          (list valid-color red green blue alpha))))))

(defun set-rgba (color)
  (let ((parsed-color (color-to-rgba color)))
    (if (first parsed-color)
        (apply 'cairo:set-source-rgba (rest parsed-color))
        (error "~S is not a valid color" color))))
;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;;; drawing callback ===========================================================
(cffi:defcallback %draw-func :void ((area :pointer)
                                    (cr :pointer)
                                    (width :int)
                                    (height :int)
                                    (data :pointer))
  (declare (ignore data))

  (setf cairo:*context* (make-instance 'cairo:context
                                       :pointer cr
                                       :width width
                                       :height height
                                       :pixel-based-p nil))
  ;; ###########################################################################

  ;; call actual drawing
  (draw-objects (window-get *lisp-app* (gtk4:widget-parent
                                        (gtk4:widget-parent
                                         (gir:build-object-ptr (gir:nget gtk4:*ns* "DrawingArea") area))))))

(defun simulate-draw-func (window)
    (let* ((surface (cairo:create-image-surface :argb32
                                                (car (dimensions window))
                                                (cdr (dimensions window)))))
      (let* ((ctx (cairo:create-context surface)))
        (setf  cairo:*context* ctx)
        ;; #######################################################################

        (progn
          ;; call actual drawing
          (draw-objects window)))

      ;; put drawn surface to a file
      (cairo:surface-write-to-png surface
                                  (format nil "~Acairo-simulate-drawing~A-~A.png"
                                          (uiop:temporary-directory)
                                          (get-internal-run-time)
                                          (gir-window window)))))

(defun simulate-draw-func-key (testkey)
  (simulate-draw-func (window-get *lisp-app* testkey)))

;;; ============================ view ==========================================
(defun draw-objects (window)            ; view
    (assert (eq (type-of window) 'lisp-window))

  (let ((cnt (format nil "~A ~S"
                     "Hello, please add some code"
                     :nothing))
        (tpx 0)
        (tpy 0))
    (cairo:set-source-rgb  1 1 1)
    (cairo:paint)


    (cairo:select-font-face "Ubuntu Mono" :normal :bold)
    (cairo:set-font-size 20)
    (multiple-value-bind  (xb yb width height)
        (cairo:text-extents cnt)
      (declare (ignore xb yb width height))
      )
    (setf tpx 6
          tpy 15)


    (cairo:set-source-rgb 0 0 0)
    (cairo:move-to tpx tpy)
    (cairo:show-text (format nil "~A" cnt))

    (cairo:move-to tpx (+ tpy 20))

    (when window
      (cairo:show-text (format nil "~A" (window-stats window)))))

  (loop for box in (children window)
        do (render box window))                ;TODO render box fails


  ;; circle at mouse coordinates
  (when (and (~> window mouse-coordinates)
             ;; now only in testing
             (eq *testing-current-window*
                 (gir-window window)))
    (cairo:set-source-rgba 1 1 0.2 0.4)
    (cairo:set-line-width 6.0)
    (cairo:arc (car (~> window mouse-coordinates))
               (cdr (~> window mouse-coordinates))
               10.0 0 (* 2.0 pi))
    (cairo:fill-path)
    (cairo:arc (car (~> window mouse-coordinates))
               (cdr (~> window mouse-coordinates))
               10.0 0 (* 2.0 pi))
    (set-rgba "green")
    (cairo:set-line-width 0.5)
    (cairo:stroke)))

;;; ===================== menu declaration =====================================

(defun menu-bar-menu (app)
  (let ((menu (gio:make-menu)))
    (gui-menu:build-menu
     menu
     (gui-menu:prepare-submenu
      "File"
      ;; (prepare-section
      ;;  nil
      ;;  (build-items
      ;;   (prepare-item-bool app menu "Dark mode" "dark-mode" nil)))

      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple app menu "New Window" "new-window")))

      ;; (prepare-submenu
      ;;  "New Game"
      ;;  (prepare-section
      ;;   nil
      ;;   (progn
      ;;     (prepare-radio-action app "new-game-size" "SMALL")
      ;;     (build-items
      ;;      (prepare-item-radio app menu "Small 8x8"    "new-game-size" "SMALL")
      ;;      (prepare-item-radio app menu "Medium 16x16" "new-game-size" "MEDIUM")
      ;;      (prepare-item-radio app menu "Large 32x32"  "new-game-size" "LARGE")))))

      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple app menu "Quit" "quit"))))
     (gui-menu:prepare-submenu
      "Help"
      ;; for now I plan to have only the About menu item
      ;; (prepare-section
      ;;  nil
      ;;  (build-items
      ;;   (prepare-item-simple app menu "Help" "help")
      ;;   (prepare-item-simple app menu "Tutorial" "tutorial" :disabled T)))
      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple app menu "About" "about")))))

    (values menu)))

;; =================== package gui-menu ========================================
;; https://docs.gtk.org/gio/ctor.MenuItem.new_section.html ; ===================
(in-package #:gui-menu)

(defun prepare-radio-action (app action-name default)
      (let ((action
              (gio:make-stateful-simple-action :name action-name
                                               :parameter-type (glib:make-variant-type
                                                                :type-string "s")
                                               :state (glib:make-string-variant
                                                       :string default))))
        (gio:action-map-add-action app action)
        (gtk4:connect action "activate"
                      (lambda (event parameter)
                        (declare (ignore event))
                        (gio:action-change-state action parameter)

                        (apply 'clops:de-menu-radio (list action-name
                                                    (glib:variant-string
                                                     (gio:action-state action))))))
        (gobj:object-unref action)))

(defun prepare-item-radio (app menu label action-name string)
  (declare (ignore app))

  (format t "preparing radio item ~S~%" (list label action-name string))

  (let ((item (gio:make-menu-item :model menu
                                  :label label
                                  :detailed-action (format nil "app.~A" action-name))))
    (setf (gio:menu-item-action-and-target-value item)
          (list (format nil  "app.~A" action-name) (glib:make-string-variant
                                       :string string)))
    item))

(defun prepare-item-bool (app menu label action-name default &key (disabled nil))
  (let ((action (gio:make-stateful-simple-action :name action-name
                                                 :parameter-type nil
                                                 :state (glib:make-boolean-variant
                                                         :value default))))
    (when disabled (setf (gio:simple-action-enabled-p action) nil))
    (gio:action-map-add-action app action)

    (gtk:connect action "activate"
                 (lambda (event parameter)
                   (declare (ignore event parameter))
                   (gio:action-change-state action (glib:make-boolean-variant
                                                    :value (if (zerop (glib:variant-hash (gio:action-state action)))
                                                               T
                                                               nil)))
                   (apply 'clops:de-menu-bool
                          (list action-name
                                (glib:variant-hash (gio:action-state action))))))
    (gobj:object-unref action))

  (gio:make-menu-item :model menu
                      :label label
                      :detailed-action (format nil  "app.~A" action-name)))

(defun prepare-item-simple (app menu label action-name &key (disabled nil))
  (let ((action (gio:make-simple-action :name action-name
                                        :parameter-type nil)))
    (when disabled (setf (gio:simple-action-enabled-p action) nil))
    (gio:action-map-add-action app action)

    (gtk4:connect action "activate"
                  (lambda (event parameter)
                    (declare (ignore event parameter))

                    (apply 'clops:de-menu-simple (list action-name))))
    (gobj:object-unref action))

  (gio:make-menu-item :model menu
                      :label label
                      :detailed-action (format nil  "app.~A" action-name)))

(defun prepare-section (label section)
  (gio:make-section-menu-item
   :label label
   :section  section))

(defun prepare-submenu (label &rest submenu-items)
  (list :submenu label
        (apply 'build-items submenu-items)))

(defun build-items (&rest items)
  (let ((submenu (gio:make-menu)))
    (apply 'build-menu submenu items)
    submenu))

(defun build-menu (submenu &rest items)
  (loop for i in items
        for item-class-string = (when (typep i 'gir::object-instance)
                                  (format nil "~A"
                                          (gir:gir-class-of i)))
        do (cond
             ((equalp item-class-string "#O<MenuItem>")
              (gio:menu-append-item submenu i))
             ((and (null item-class-string)
                   (consp i)
                   (eq :submenu (first i)))
              (gio:menu-append-submenu submenu (second i) (third i)))
             (T (error "unexpected item-class-string or option ~S" item-class-string)))))

;; ================================= MENU code ends here =======================

;;; ============================ package gui-window ============================
(in-package #:gui-window)

;; =========================== dialogs =========================================
(defun present-about-dialog ()
    (let ((dialog (about-dialog)))
      (setf (gtk4:window-modal-p dialog) t
            (gtk4:window-transient-for dialog) (gtk4:application-active-window (clops:gtk4-app clops:*lisp-app*)))
      (gtk4:window-present dialog)))

(defun about-dialog ()
  (let ((dialog (gtk4:make-about-dialog)))
    (setf (gtk4:about-dialog-authors      dialog)   (list "Jacek Podkanski")
          (gtk4:about-dialog-website      dialog)   "https://github.com/bigos"
          (gtk4:about-dialog-program-name dialog)   "CLops"
          (gtk4:about-dialog-comments     dialog)   "Common Lisp Overly Premature System"
          (gtk4:about-dialog-license      dialog)   "GPL-3.0-or-later"
          (gtk4:about-dialog-system-information dialog) (format nil "~A" (uiop/os:implementation-identifier))
          (gtk4:about-dialog-logo-icon-name dialog) "application-x-addon")
    (values dialog)))

;; =========================== closing everything ==============================
(defun close-all-windows-and-quit ()
  (loop for aw = (gtk4:application-active-window (clops:gtk4-app clops:*lisp-app*))
        until (null aw)
        do (gtk4:window-close aw)))

;; ============================== app windows ==================================

(defun app-windows ()
  (when (clops:gtk4-app clops:*lisp-app*)
    (let ((app-windows (gtk4:application-windows (clops:gtk4-app clops:*lisp-app*))))
      (loop for pos from 0 below (glib:glist-length app-windows)
            collect (glib:glist-nth app-windows pos)))))

;; ============================= key event translation =========================
(defun translate-key-args (args)
  (destructuring-bind (keyval keycode keymods) args
    (list
     (format nil "~A"
             (let ((unicode (gdk:keyval-to-unicode keyval)))
               (if (or (zerop unicode)
                       (member keyval
                               (list gdk:+key-escape+
                                     gdk:+key-backspace+
                                     gdk:+key-delete+)))
                   ""
                   (code-char unicode))))
     (gdk:keyval-name keyval)
     keycode
     (remove-if (lambda (m) (member m '(:num-lock)))
                (loop
                      for modname in '(:shift :caps-lock :ctrl :alt
                                       :num-lock :k6 :win :alt-gr)
                      for x = 0 then (1+ x)
                      for modcode = (mask-field (byte 1 x) keymods)
                      unless (zerop modcode)
                      collect modname)))))

;; ============================ events =========================================
(defun window-events (window)
  (let ((key-controller   (gtk4:make-event-controller-key))
        (focus-controller (gtk4:make-event-controller-focus)))

    (gtk4:widget-add-controller window key-controller)
    (gtk4:widget-add-controller window focus-controller)

    (gtk4:connect key-controller "key-pressed"
                  (lambda (e &rest args)
                    (declare (ignore e))
                    (apply #'clops:de-key-pressed (funcall #'translate-key-args args))))

    (gtk4:connect key-controller "key-released"
                  (lambda (e &rest args)
                    (declare (ignore e))
                    (apply #'clops:de-key-released (funcall #'translate-key-args args))))

    (gtk4:connect focus-controller "enter"
                  (lambda (e &rest args)
                    (declare (ignore e args))
                    (apply #'clops:de-focus-enter (list window))))

    (gtk4:connect focus-controller "leave"
                  (lambda (e &rest args)
                    (declare (ignore e args))
                    (apply #'clops:de-focus-leave (list window)))))

  (glib:timeout-add 1000
                    (lambda (&rest args)
                      (declare (ignore args))
                      (funcall #'clops:de-timeout)
                      glib:+source-continue+))

  (gtk4:connect window "close-request" (lambda (widget &rest args)
                                         (declare (ignore widget args))
                                         (clops:window-remove clops:*lisp-app* window)
                                         (gtk4:window-close window))))

(defun canvas-events (canvas)
  (labels
      ((canvas-window (c)
         (gtk4:widget-parent (gtk4:widget-parent c))))
    (let ((motion-controller (gtk4:make-event-controller-motion)))
      (gtk4:widget-add-controller canvas motion-controller)

      (gtk4:connect motion-controller "motion"
                    (lambda (e &rest args) (declare (ignore e)) (apply #'clops:de-motion
                                                                       (append args
                                                                               (list
                                                                               (canvas-window
                                                                                canvas))))))
      (gtk4:connect motion-controller "enter"
                    (lambda (e &rest args) (declare (ignore e)) (apply #'clops:de-motion-enter
                                                                       (append args
                                                                               (list
                                                                                (canvas-window
                                                                                 canvas))))))
      (gtk4:connect motion-controller "leave"
                    (lambda (e &rest args) (declare (ignore e)) (apply #'clops:de-motion-leave
                                                                       (append args
                                                                               (list
                                                                                (canvas-window
                                                                                 canvas)))))))

    (let ((scroll-controller (gtk4:make-event-controller-scroll :flags gtk4:+event-controller-scroll-flags-vertical+)))
      (gtk4:widget-add-controller canvas scroll-controller)
      (gtk4:connect scroll-controller "scroll"
                    (lambda (e &rest args) (declare (ignore e)) (apply #'clops:de-scroll
                                                                       args))))

    (let ((gesture-click-controller (gtk4:make-gesture-click))
          (click-fn (lambda (event args click-de-fn)
                      (let ((current-button (gtk4:gesture-single-current-button event)))
                        (apply click-de-fn (list current-button
                                                 (nth 1 args)
                                                 (nth 2 args)))))))
      ;; make gesture click listen to other mouse buttons as well
      (setf (gtk4:gesture-single-button gesture-click-controller) 0)
      (gtk4:widget-add-controller canvas gesture-click-controller)

      (gtk4:connect gesture-click-controller "pressed"
                    (lambda (event &rest args) (apply click-fn (list event args #'clops:de-pressed))))
      (gtk4:connect gesture-click-controller "released"
                    (lambda (event &rest args) (apply click-fn (list event args #'clops:de-released)))))


    ;; for some reason resize signal does not work without  notify
    (gtk4:connect canvas "notify" (lambda (widget &rest args)
                                    (format t "~&>>>>>>>>>>>>>>>>>>>>>>>> notifying ~S ~S~%" widget args)))

    (gtk4:connect canvas "resize" (lambda (widget &rest args)
                                    (declare (ignore widget))
                                    (clops:de-resize (first args) (second args) (canvas-window
                                                                           canvas))))))

;; =============================================================================

(defun new-window-for-app (app)
    (let ((window (gtk4:make-application-window :application app)))
      (gtk4:application-add-window app window)

      (setf
       (gtk4:application-menubar app) (clops:menu-bar-menu app)
       (gtk4:application-window-show-menubar-p window) T)

      (setf
       (gtk4:window-title window) "CLOS Toy"
       (gtk4:window-default-size window) (list 680 140))

      (let ((box (gtk4:make-box :orientation gtk4:+orientation-vertical+
                                :spacing 0)))
        (let ((canvas (gtk4:make-drawing-area)))
          ;; TODO relying on the global variable lead to a bug when adding multiple windows
          (setf clops:*canvas* canvas
                (gtk4:widget-vexpand-p canvas) T
                (gtk4:drawing-area-draw-func canvas) (list (cffi:callback clops:%draw-func)
                                                           (cffi:null-pointer)
                                                           (cffi:null-pointer)))
          (canvas-events canvas)
          (gtk4:box-append box canvas))
        (setf (gtk4:window-child window) box))

      (window-events window)

      (format t "actions defined for app ~A~%"  (gio:action-group-list-actions app))

      (gtk:window-present window)

      window))

(defun window-activation (app)
  (gtk4:connect app "activate"
                (lambda (app)
                  (clops:window-add clops:*lisp-app* (new-window-for-app app)))))

(defun window ()
  (let ((app (gtk:make-application :application-id "org.bigos.gtk4-example.better-menu"
                                   :flags gio:+application-flags-flags-none+)))
    (setf clops:*lisp-app* (make-instance 'clops:lisp-app :gtk4-app app))
    (window-activation app)

    (let ((status (gtk:application-run app nil)))
      (setf clops:*canvas* nil
            clops:*lisp-app*  nil)

      (gobj:object-unref app)
      status)))

;;; ================================ package clops =============================
(in-package #:clops)

(defun main ()
  (when (eq :os-windows (uiop/os:detect-os))
    (examine-windows-system))
  ;; (init-model)
  (gui-window:window))

;;; that is useless because the system fails before the checks are made when we do not have the mingw64/bin in the PATH, but it may be useful for one of the missing libraries
(defun examine-windows-system ()
  (labels
      ((examine-library (path label)
         (if (uiop/filesystem:file-exists-p path)
             (format t "~A library found~%" label)
             (warn "could not find the expected ~A library" label))))
    (progn
      (format t "~&~%~%running launcher on ~A~%" (uiop/os:detect-os))

      (if (eq :os-windows (uiop/os:detect-os))
          (progn
            (format t "OS check OK~%")
            (if (or
                 (search "mingw64/bin"  (uiop/os:getenv "PATH") :test #'equalp)
                 (search "mingw64\\bin" (uiop/os:getenv "PATH") :test #'equalp))
                (progn
                  (format t "MSYS2 bin is found in environmet variable PATH~%")
                  (if (uiop/filesystem:directory-exists-p #p "c:/msys64/mingw64/bin")
                      (progn
                        (format t "MSYS2 bin folder check OK~%~%")

                        (examine-library #p "c:/msys64/mingw64/bin/libgtk-4-1.dll" "gtk")
                        (examine-library "c:/msys64/mingw64/bin/libcairo-2.dll" "cairo")
                        (examine-library "c:/msys64/mingw64/bin/libglib-2.0-0.dll" "glib")
                        (examine-library "c:/msys64/mingw64/bin/libgio-2.0-0.dll" "gio")
                        (examine-library "c:/msys64/mingw64/bin/libgirepository-1.0-1.dll" "girrepository")
                        (examine-library "c:/msys64/mingw64/bin/libgobject-2.0-0.dll" "gobject"))
                      (warn "directory not found ~A" "C:\\msys64\\mingw64\\bin")))
                (warn "MSYS2 bin path not found in the PATH environmental variable")))
          (warn "Expected Windows but found ~A" (uiop/os:detect-os))))))

;;; for repl development
(when nil
  (load "~/Programming/Lisp/lispy-experiments/clops.lisp")
  (in-package #:clops)
  )
