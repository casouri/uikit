;;; uikit.el --- UI Kit for Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Yuan Fu
;;
;; Author: Yuan Fu <casouri@gmail.com>
;;
;; Package-Requires: ((dash "2.14.1"))
;;
;;; This file is NOT part of GNU Emacs
;;
;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.


;;; Commentary:
;;

;;; Code:
;;


(require 'eieio-base)
(require 'dash)

;;; Customize

(defgroup uikit
  ()
  "A UI framework."
  :group 'convenience)


;;; Variable

(defvar uikit--drawing nil
  "Set to t when drawing.")

(defvar uikit-pad-char ?\s
  "The char used to pad views.")

(defvar-local uikit-buffer-scene nil
  "The scene object of current buffer.")

(defvar-local uikit-buffer-scene nil
  "The scene of this buffer.")

;;; Base Structs

(cl-defstruct uikit-pos
  "Represents a position in canvas.
both x and y are zero based.
Note that the length of the unit in y direction is about two times as
the unit in x direction.
For example (20, 10) nearly forms a square."
  (x 0)
  (y 0))

(cl-defmethod uikit-pos+ ((pos uikit-pos) (offset uikit-pos))
  "Add OFFSET to POS destructivly. Return the modified POS."
  (setf (uikit-pos-x pos) (+ (uikit-pos-x pos) (uikit-pos-x offset))
        (uikit-pos-y pos) (+ (uikit-pos-y pos) (uikit-pos-y offset)))
  pos)

;; TODO DOC
;; (cl-defstruct uikit-layout
;;  "Specifies how does a stack arrange
;; space for its subviews.

;; start-at is where does the subviews start stacking.

;; fill is the filling stretagy of subviews.
;; it can be either 'proportional or 'equal.
;; equal only takes effect when one of the directions
;; is unbound.

;; pace-between is the space between subviews.

;; lign is the alignment of the subviews along the stack axis,
;; t can be 'left, 'right, 'center.
;; left and 'right is the relative direction when you regard
;; start-at as top.
;; therefore, if 'start-at is 'bottom, 'left is actually right."
;;  (start-at 'left)
;;  (fill 'proportional)
;;  (space-between 0)
;;  (align 'left))

;; UNUSED
(cl-defstruct uikit-range
  "Defines a range."
  (min 0)
  (max 0))
;; UNUSED
(cl-defmethod uikit-in (num (range uikit-range) &optional min-exclusive max-exclusive)
  "Return t if NUM is in RANGE, return nil if not.
if MIN-EXCLUSIVE is t, range is min exclusive,
else is  for MAX-EXCLUSIVE."
  (let ((min (uikit-range-min range))
        (max (uikit-range-max range))
        (less-than-func (if max-exclusive '< '<=))
        (greater-than-func (if min-exclusive '> '>=)))
    (if (and (funcall greater-than-func num min)
             (funcall less-than-func num max))
        t
      nil)))

;;; Base Function

(defun uikit-prepare-canvas (&optional width height)
  "Prepare the canvas by inserting spaces.
WIDTH and HEIGHT are optional."
  ;; TESTED
  (let ((width (or width (window-width)))
        (height (or height (window-height)))
        current-line-left-over)
    (save-excursion
      (goto-char (point-max))
      (setq current-line-left-over
            (- width
               (- (point)
                  (line-beginning-position))))
      (insert (make-string (1- current-line-left-over) ?\s) ?\n)
      ;; ??? absolute line number?
      (dotimes (_ (- height (line-number-at-pos (point) t)))
        (insert (make-string (1- width) ?\s) ?\n)))))

(defsubst uikit--append (seq elt)
  ;; TOTEST
  "Append ELT to SEQ destructivly. This is a macro."
  (if seq
      (nconc seq (list elt))
    (setq seq (list elt))))

(defsubst uikit--push (elt seq)
  ;; TOTEST
  "Push ELT to SEQ destructivly. This is a macro."
  (if seq
      (push elt seq)
    (setq seq (list elt))))

(defun uikit-replace-with (str beg)
  "Replace strings between BEG and (+ BEG (length STR)) with STR."
  ;; TOTEST hand tested
  (save-excursion
    (delete-region beg (+ beg (length str)))
    (goto-char beg)
    (insert str)))

(defun uikit-goto (x y)
  "Go to X Y."
  ;; TOTEST
  (goto-char (+ 1 x (* y (window-body-width)))))

(defun uikit-pos-to-point (pos)
  "Convert `uikit-pos' POS to point (integer)."
  ;; TOTEST
  (+ 2 (uikit-pos-x pos) (* (1- (uikit-pos-y pos)) (window-body-width))))

(defun uikit-raw-draw (content x y)
  "Draw raw strings to canvas at X and Y.
CONTENT is a list of strings. Generally you want to make them have same length."
  ;; TOTEST hand tested
  (let ((window-width (window-body-width))
        point)
    (save-excursion
      (uikit-goto x y)
      (setq point (point))
      (dolist (line content)
        ;; ??? maybe cache a window width for current scene
        (uikit-replace-with line point)
        (setq point (+ point window-width))))))

;;;; Helpers

(defun uikit--error-nil (var msg &rest args)
  "Raise an error if VAR is nil, show MSG. ARGS are supplied to  `error'."
  (if var
      var
    (apply #'error msg args)))

(defun uikit--ensure>=0 (var)
  "If VAR < 0, return 0, else return VAR."
  (if (< var 0)
      0
    var))

;;; Base Class
;;;; Helpers
(defmacro uikit-defclass (classname superclass-list slot-list &rest rest)
  "A thin wrapper around `defclass'.
Expands to (defclass CLASSNAME SUPERCLASS-LIST SLOT-LIST REST).

Additionally, it gives you a free initarg, accessor, writer and reader
in following conventions:
:initarg :slotname
:accessor uikit-slotname-of
:writer uikit--slotname-set
:reader uikit--slotname-get

Any user defined slot options will override these automatically generated ones."
  ;; TODO hand written accessor and initarg are not needed
  ;; in many earlier class definitions anymore,
  ;; maybe consider removing them
  (declare (indent 2))
  `(defclass ,classname ,superclass-list
     ,(mapcar (lambda (slot-form)
                (let ((slot-name (symbol-name (car slot-form))))
                  (append slot-form
                          ;; slot options appeared earlier
                          ;; takes precedents
                          (list :initarg
                                (intern (format ":%s" slot-name))
                                :accessor
                                (intern (format "uikit-%s-of" slot-name))
                                :reader
                                (intern (format "uikit--%s-get" slot-name))
                                :writer
                                (intern (format "uikit--%s-set" slot-name))))))
              slot-list)
     ,@rest))

;;;; View
;;;;; View
(uikit-defclass uikit-view ()
  ((id
    :initarg :id
    :initform "anonymous"
    :type string
    :accessor uikit-id-of
    :documentation "The identification of this view. Should be a string.
Uikit creates a variable`uikit//ID' for the view,
so make sure to include app name as prefix to avoid name clash.
(Yeah you never escape from prefix)"
    :type (or null string))
   (left
    :initarg :left
    :accessor uikit-raw-left-of
    :initform nil
    :documentation "The left of view. Can be positive integer of (lambda (view)).")
   (right
    :initarg :right
    :accessor uikit-raw-right-of
    :initform nil
    :documentation "The right of view. Can be positive integer of (lambda (view)).")
   (top
    :initarg :top
    :accessor uikit-raw-top-of
    :initform nil
    :documentation "The top of view. Can be positive integer of (lambda (view)).")
   (bottom
    :initarg :bottom
    :accessor uikit-raw-bottom-of
    :initform nil
    :documentation "The bottom of view. Can be positive integer of (lambda (view)).")
   (width
    :initarg :width
    :accessor uikit-raw-width-of
    :initform nil
    :documentation "The width of view. Can be positive integer of (lambda (view)).")
   (height
    :initarg :height
    :accessor uikit-raw-height-of
    :initform nil
    :documentation "The height of view. Can be positive integer of (lambda (view)).")
   (left-cache
    :accessor uikit-left-cache-of
    :initform nil
    :type (or null integer)
    :documentation "Cache of `left' of the view during drawing process.")
   (right-cache
    :accessor uikit-right-cache-of
    :initform nil
    :type (or null integer)
    :documentation "Cache of `right-cache' of the view during drawing process.")
   (top-cache
    :accessor uikit-top-cache-of
    :initform nil
    :type (or null integer)
    :documentation "Cache of `top' of the view during drawing process.")
   (bottom-cache
    :accessor uikit-bottom-cache-of
    :initform nil
    :type (or null integer)
    :documentation "Cache of `bottom' of the view during drawing process.")
   (width-cache
    :accessor uikit-width-cache-of
    :initform nil
    :type (or null integer)
    :documentation "The `width' cache.")
   (height-cache
    :accessor uikit-height-cache-of
    :initform nil
    :type (or null integer)
    :documentation "The `height' cache.")
   ;; drawing
   (parent-stackview
    :accessor uikit-parent-stackview-of
    :initform nil
    :documentation "The parent stackview of this view.
Used to check if the view is within the stackview on screen."))
  "Parent of `uikit-atom-view' and `uikit-stackview'."
  :abstract t)

(defsubst uikit-by-id (id)
  "Return the view by ID."
  (symbol-value (intern (format "uikit//%s" id))))

(cl-defmethod initialize-instance ((view uikit-view) &optional slots)
  "Process the :id key. Create a variable `uikit//ID' as an alias of the view (If there is any).
e.g. uikit//mybutton for id \"mybutton\"."
  (cl-call-next-method view slots)
  (when (uikit-id-of view)
    (setf (symbol-value (intern (format "uikit//%s" (uikit-id-of view))))
          view)))

;;;;; Universal Accessor: Getter & Setter

(defsubst constrain-of (view constrain &optional value)
  "Get/set CONSTRAIN of VIEW.

If VALUE non-nil, set, otherwise get.
VALUE can be a positive integer or a (quoted) form.

Form will be evaluated to get a number during drawing of VIEW,
make sure it returns a positive integer.

Normally you want to just use `left/right/...-of' accessors,
this function is used by stackview during auto layout
because they needs to change left/right/.../ base on their direction."
  (funcall (intern (format "%s-of" (symbol-name constrain))) view value))



;;;;; Special Accessor for View's Constrain

;; SCRATCH
;; (setq cc (lambda (seq) (funcall 'cc seq)))
;;
;; (funcall (let ((cc (lambda (seq) (nth 1 seq))))
;;            cc)
;;          '(1 2 3))
;;
;; (defmacro bb (sym val)
;;   `(defmacro ,sym (a b c)
;;      (list 'list ,val a b c)))
;;
;; (bb cc)
;; (cc 1 2 3)
;; END_SCRATCH

(defmacro uikit--orientation-h/v (stack h v)
  "Evaluate H if orientation of STACK is `left' or `right', eval V if `top' or `bottom'."
  `(pcase (uikit-orientation-of ,stack)
     ((or 'left 'right) ,h)
     ((or 'top 'bottom) ,v)))

;; (defmacro uikit--with-orientation (stack left bottom right top)
;;   "Evaluate LEFT RIGHT TOP BOTTOM base on the orientation of STACK."
;;   `(pcase (uikit-orientation-of ,stack)
;;      ('left ,left)
;;      ('right ,right)
;;      ('top ,top)
;;      ('bottom ,bottom)))

(defmacro uikit--make-special-accessor (constrain form)
  "Define special accessor for CONSTRAIN with FORM.
FORM is calculation logic. E.g, left = right - width"
  (let ((raw-xx-of (intern (format "uikit-raw-%s-of" (symbol-name constrain))))
        ;; (raw-xx (intern (format "raw-%s" (symbol-name constrain))))
        (xx-cache-of (intern (format "uikit-%s-cache-of" (symbol-name constrain)))))
    `(progn
       ;; define a function that `funcall' on the symbol as same as the function itself
       ;; with arguments received
       ;; then we can play with the variable of the same symbol as function
       ;; in closure
       (defmacro ,(intern (format "uikit-%s-of" (symbol-name constrain))) (view &optional value self-recure)
         ,(format "Takes any VIEW of `uikit-view' class and return its `%s' slot.
If `%s' slot is nil, calculate it.
Set the slot to VALUE when VALUE non-nil.
VALUE can be a positive integer or a (quoted) form.
Form will be evaluated to get a number during drawing of VIEW,
make sure it returns a positive integer.
SELF-RECURE is set to t when a view tries to calculate its constrain
from it's own constrains, e.g. right = left + width.
That might end up in infinite recursion."
                  (symbol-name constrain) (symbol-name constrain))
         (list 'funcall ',(intern (format "uikit-%s-of" (symbol-name constrain)))
               view value self-recure))
       (setq ,(intern (format "uikit-%s-of" (symbol-name constrain)))
             (lambda (view &optional value self-recure)
               (if value ;; set value if given
                   (setf (,raw-xx-of view) value)
                 ;; get value
                 (if uikit--drawing
                     ;; drawing: first look at cache, if nil calculate
                     (or (,xx-cache-of view)
                         (setf (,xx-cache-of view)
                               (let ((constrain (,raw-xx-of view)))
                                 (pcase constrain
                                   ;; if there is a function in it, run that function,
                                   ;; the function might refer to constrains of other view, that's ok
                                   ((pred functionp)
                                    (condition-case err
                                        (funcall constrain view)
                                      ((debug error) (message "Error calculating constrain of %s (by calling lambda function), error is: %s"
                                                              (uikit-id-of view)
                                                              err))))
                                   ;; User hard coded this constrain, use it
                                   ((pred integerp) constrain)
                                   ;; ??? How to match nil?
                                   ;;
                                   ;; OK, we need the constrain but it's nil,
                                   ;; try to calculate it based on the other two
                                   ;; note the view to be drew only needs left, width, top & height
                                   ;; right and bottom is only needed when some other view's constrain
                                   ;; refers to this view's right/bottom
                                   ;; if self-recure is t and you ends up here,
                                   ;; too bad, there is not enough constrain, go cry in the bathroom
                                   ;; actually, if return nil, `uikit-draw' will set width to content width
                                   ;; so it's ok for width/height
                                   (_ (if self-recure
                                          nil
                                        ,form))))))
                   ;; not drawing, return the raw-constrain, let it be a number or lambda
                   (,raw-xx-of view))))))))


;; simply return nil if operand(s) is(are) nil
(uikit--make-special-accessor left (ignore-errors (- (uikit-right-of view nil t) (or (uikit-width-of view nil t) ; user configured width
                                                                                     (uikit-content-width-of view))))) ; or content width
(uikit--make-special-accessor right (ignore-errors (+ (uikit-left-of view nil t) (or (uikit-width-of view nil t)
                                                                                     (uikit-content-width-of view)))))
(uikit--make-special-accessor top (ignore-errors (- (uikit-bottom-of view nil t) (or (uikit-height-of view nil t)
                                                                                     (uikit-content-height-of view)))))
(uikit--make-special-accessor bottom (ignore-errors (+ (uikit-top-of view nil t) (or (uikit-height-of view nil t)
                                                                                     (uikit-content-height-of view)))))
;; ??? how is width and height used?
(uikit--make-special-accessor width (ignore-errors (- (uikit-right-of view nil t) (uikit-left-of view nil t))))
(uikit--make-special-accessor height (ignore-errors (- (uikit-bottom-of view nil ) (uikit-top-of view nil t))))

;; in each orientation, the relationship between constrains:
;; (constrain in parenthesis is the actual on-screen constrain)
;;
;; 'left:
;; bottom(bottom) = top(top) + height(height)
;; right(right) = left(left) + width(width)
;;
;; 'bottom:
;; bottom(right) = top(left) + height(width)
;; right(top) = left(bottom) - width(height)
;;
;; 'right
;; bottom(top) = top(bottom) - height(height)
;; right(left) = left(right) - width(width)
;;
;; 'top
;; bottom(left) = top(right) - height(width)
;; right(bottom) = left(top) + width(height)
;;
;; Notice there some are minus and some are plus in the (logically) same equation.
;; e.g., something bottom = top + height, and other times bottom = top - height
;; Obviously thats a problem.
;;
;; To solve the problem, we define directions that shifts basis with our constrains
;; and when calculating between constrains, we use the shifted directions.
;;
;; P.S. in the above code (`uikit--make-special-accessor') we can use plain
;; plus and minus because they are calculating on the on-screen values,
;; which are fixed.
;; On the other hand, our basis shifted part (in `uikit-autolaout') doesn't have
;; that benefit


(defmacro uikit-go-up (base distance)
  "Return the value of going up DISTANCE from BASE."
  `(funcall uikit-go-up ,base ,distance))
(defmacro uikit-go-down (base distance)
  "Return the value of going down DISTANCE from BASE."
  `(funcall uikit-go-down ,base ,distance))
(defmacro uikit-go-left (base distance)
  "Return the value of going left DISTANCE from BASE."
  `(funcall uikit-go-left ,base ,distance))
(defmacro uikit-go-right (base distance)
  "Return the value of going right DISTANCE from BASE."
  `(funcall uikit-go-right ,base ,distance))


(setq uikit-go-up (lambda (base distance) (- base distance))
      uikit-go-down (lambda (base distance) (+ base distance))
      uikit-go-right uikit-go-down
      uikit-go-left uikit-go-up)


;;;; Atom View
;;;;; Class
(uikit-defclass uikit-atom-view (uikit-view)
  ((face
    :accessor uikit-face-of
    :initarg :face
    :initform nil
    :documentation "The face used for this view. Default to nil.
You can set face by property list but this is more convenient.
face in property list will override this. "
    :type (or null (satisfies facep)))
   (pad-char
    :accessor uikit-pad-char-of
    :initarg :pad-char
    :initform (eval uikit-pad-char)
    :documentation "The char used to pad extra space."
    :type character)
   (keymap
    :accessor uikit-keymap-of
    :initarg :keymap
    :initform nil
    :documentation "Keymap on the view
in addition to the default major and minor mode keymaps. Default to nil.
You can set keymap in property list but this is more convenient.
TODO Does keymap in property list override this?"
    :type (or null (satisfies keymapp)))
   (property-list
    :accessor uikit-property-list-of
    :initarg :property-list
    :initform nil
    :documentation "Extra text properties that you want to put to view text.
overwrites face and keymap slot.
each element of the list is an cons of PROPERTY and VALUE that are eligibel for `put-text-property'.")

   ;; content

   (content
    :accessor uikit-content-of
    :initform ()
    :documentation "The actual text that is drew to canvas, cached for later use.
But it doesn't include the padding spaces around, since size might
change more frequent that the content. See `padded-content'.
It is a list of strings, each string is a line.")
   ;; ??? keep it or not?
   (content-changed
    :accessor uikit-content-changed-of
    :initform t
    :documentation "If this is t, then content needs to be recalculated.
Every time a change that alters the visual appearance of the view is made,
this should be set to t. But don't change this value directly, use `uikit-changed'.
Typically this value is changed to nil in`uikit-make-content'.
This being nil only means the content didn't change, the position of the view
is not guaranteed to be the same."
    :type boolean))
  "A view is like a widget. It is the smallest unit of an UI.
his class is an abstract class.")

;;;;; Methods

(cl-defmethod uikit-make-content ((view uikit-atom-view))
  "Make content from data, returna a list of strings, each string is a line.
Each line must have same length and should not contain any return char."
  ;; simply return the content
  (uikit-content-of view))

(cl-defmethod uikit-make-content :around ((view uikit-atom-view))
  "Make content if `content-changed' is t.
This function returns the content just for usability.
It's main job is to (potentially) update the content of VIEW.
It also sets `content-changed', `width', `height', `content' slots.
So the specific `uikit-make-content' of each view class has to return their content."
  (if (uikit-content-changed-of view)
      (let ((content (cl-call-next-method view)))
        (setf (uikit-content-changed-of view) nil
              (uikit-content-of view) content)) ; last val will be returned
    ;; content not changed, just return it
    (uikit-content-of view)))

;; Update content has to be separated from drawing (more specifically, before that)
;; because EVERY view has to have their content ready before
;; ANY view starts to calculate its position.
;; For example, a sbview in a equal spacing stackview calculates
;; its left depends on the last view's right and the equal spacing;
;; equal spacing is depended to ALL subview's width and stackview's length
;; If any view doesn't have its content ready, equal spacing would return wrong number
(cl-defmethod uikit-draw ((view uikit-atom-view) &optional x y)
  "Draw the content on screen.

It just add properties: face, keymap, uikit-atom-view, others in property-list,
and then call `uikit-raw-draw'."
  ;; TOTEST
  ;; normally parent stackview handles this (make content),
  ;; but if the view doesn't have a parent stackview
  ;; it got to do it itself
  (unless (uikit-parent-stackview-of view)
    (uikit-make-content view))
  (let ((content (uikit-content-of view))
        (all-property (append `(uikit-view ,(gv-ref view)
                                           face ,(uikit-face-of view)
                                           keymap ,(uikit-keymap-of view))
                              (uikit-property-list-of view)))
        (inhibit-modification-hooks t)
        (width (or (uikit-width-of view) (uikit-content-width-of view)))
        (height (or (uikit-height-of view) (uikit-content-height-of view)))
        line
        (uikit--drawing t)
        (inhibit-modification-hooks t)
        (undo-inhibit-record-point t))
    (dolist (index (number-sequence 0 (1- (length content))))
      (setf line (gv-ref (nth index content)))
      (setf (gv-deref line)
            ;; return the line with it length regulated to `width'
            (let ((offset (- width (length (gv-deref line)))))
              (if (> offset 0) ;; required width longer than actual length
                  (concat (gv-deref line) (make-string offset ?\s))
                ;; required width shorter than actual length
                (substring (gv-deref line) 0 width))))
      (add-text-properties 0 width all-property (gv-deref line)))
    ;; we just set cache position, draw at there
    (uikit-raw-draw (cl-subseq content 0 height)
                    (or x (uikit--error-nil (uikit-left-of view)
                                            "Warning: %s(%s)'s left is nil"
                                            (uikit-buttonize (uikit-id-of view) (lambda () (interactive) (print (object-print view))))
                                            (eieio-object-class-name view)))
                    (or y (uikit--error-nil (uikit-top-of view)
                                            "Warning: %s(%s)'s top is nil"
                                            (uikit-buttonize (uikit-id-of view) (lambda () (interactive) (print (object-print view))))
                                            (eieio-object-class-name view))))))

(defun uikit-buttonize (str func &optional return-func mouse-2-func mouse-3-func)
  "Make a button string from STR which run FUNC on click.
If RETURN-FUNC, MOUSE-2-FUNC, MOUSE-3-FUNC are set if non-nil.
By default RETURN-FUNC used FUNC when RETURN-FUNC is nil."
  (propertize str 'keymap (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "<mouse-1>") func)
                            (define-key map (kbd "<mouse-2>") mouse-2-func)
                            (define-key map (kbd "<mouse-3>") mouse-3-func)
                            (define-key map (kbd "<return>") (or return-func func))
                            map)
              'face 'uikit-button-face))

(cl-defmethod uikit-content-width-of ((view uikit-atom-view))
  "Return the content width of VIEW. Only look at first line of content."
  ;; TOTEST
  (length (car (uikit-content-of view))))

(cl-defmethod uikit-content-height-of ((view uikit-atom-view))
  "Return the content height of VIEW."
  ;; TOTEST
  (length (uikit-content-of view)))

(cl-defmethod uikit-content-height-in-parent-of ((view uikit-view))
  "Return the content height of VIEW, in respect to its parent stackivew.
That is, if parent stackview's orientation is 'top/'bottom (horizontal),
this fucntion actually returns the width."
  (pcase (uikit-orientation-of (uikit-parent-stackview-of view))
    ((or 'left 'right) (uikit-content-height-of view))
    ((or 'top 'bottom) (uikit-content-width-of view))))

(cl-defmethod uikit-content-width-in-parent-of ((view uikit-view))
  "Return the content height of VIEW, in respect to its parent stackivew.
That is, if parent stackview's orientation is 'top/'bottom (horizontal),
this fucntion actually returns the height."
  (pcase (uikit-orientation-of (uikit-parent-stackview-of view))
    ((or 'left 'right) (uikit-content-width-of view))
    ((or 'top 'bottom) (uikit-content-height-of view))))


(cl-defmethod uikit-content-changed ((view uikit-atom-view))
  "Let UIKit know VIEW's content is changed."
  (setf (uikit-content-changed-of view) t))

;;;; Stackview
;;;;; Class
(uikit-defclass uikit-stackview (uikit-view)
  ((subview-list
    :accessor uikit-subview-list-of
    :initarg :subview-list
    :initform nil
    :documentation "List of subviews of the stack view.")
   (autolayout
    :accessor uikit-autolayout-of
    :initarg :autolayout
    :initform nil
    :documentation "Determins how does stackview \"stack\" subviews together.
It can be nil, 'stacking, 'equal-spacing, 'portion."
    :type (or null symbol))
   (stacking-space
    :initarg :stacking-space
    :accessor uikit-stacking-space-of
    :initform 0
    :type integer
    :documentation "The space between subviews when using 'stacking autolayout.")
   (equal-spacing-space-cache
    :accessor uikit-equal-spacing-space-cache-of
    :initform nil
    :type (or null integer)
    :documentation "Spacing between subviews when using 'equal-spacing auatolayout.")
   (v-align
    :accessor uikit-v-align-of
    :initarg :v-align
    :initform 'top
    :type symbol
    :documentation "Vertical alignment of subviews. Can be 'top, 'center, 'bottom.
Only take effect when `autolayout' is non-nil.")
   (orientation
    :accessor uikit-orientation-of
    :initarg :orientation
    :initform 'left
    :type symbol
    :documentation "The orientation of the stackview. 'left (default) means normal orientation.
'top means rotate clockwise: top becomes left, left becomes bottom, etc.")
   (max-subview-height-cache
    :accessor uikit-max-subview-height-cache-of
    :initform nil
    :type (or null integer)
    :documentation "Cache of `max-subview-height'."))
  "Stack view, used for grouping multiple view together and arrage their position automatically.")

;;;;; Methods

;; special accessor
(defun uikit-max-subview-height-of (stack)
  "Return the largest height of the subviews of stackview STACK."
  (or (uikit-max-subview-height-cache-of stack)
      (setf (uikit-max-subview-height-cache-of stack)
            (apply 'max
                   (or (mapcar (uikit--orientation-h/v stack
                                                       'uikit-content-height-of
                                                       'uikit-content-width-of)
                               (uikit-subview-list-of stack))
                       '(0))))))


(defun uikit-equal-spacing-space-of (stackview)
  "Return the space between subviews of STACKVIEW."
  (or (uikit-equal-spacing-space-cache-of stackview)
      (condition-case nil
          (let ((subview-list (uikit-subview-list-of stackview)))
            ;; (stackview width - sum of subviews' width ) / (subview number - 1)
            (floor (/ (let ((result (- (uikit-width-of stackview)
                                       ;; TODO replace with `uikit-content-width-of'
                                       (cl-reduce '+ (mapcar (lambda (view) (uikit-content-width-of view)) subview-list)))))
                        ;; TODO replace with `uikit--ensure>=0'
                        (if (< result 0)
                            0
                          result))
                      (1- (length subview-list)))))
        ((debug error) (message "Not enough constrain for %s. Cannot calculate equal-spacing-space constrain of it"
                                (uikit-id-of stackview))))))

(cl-defmethod uikit-make-content ((stackview uikit-stackview))
  "STACKVIEW make content by asking its subviews to make content."
  ;; TOTEST
  (dolist (subview (uikit-subview-list-of stackview))
    (uikit-make-content subview)))

(cl-defmethod uikit-quit ((stackview uikit-stackview))
  "Quit stackview and set all subviews to nil."
  ;; TODO not finished
  (dolist (subview (uikit-subview-list-of stackview))
    (uikit-quit subview))
  (setf stackview nil))

(cl-defmethod uikit-draw ((stack uikit-stackview))
  "Draw the stackview."
  (dolist (subview (uikit-subview-list-of stack))
    (uikit-make-content subview))
  ;; Update content has to be separated from drawing (more specifically, before that)
  ;; because EVERY view has to have their content ready before
  ;; ANY view starts to calculate its position.
  ;; For example, a sbview in a equal spacing stackview calculates
  ;; its left depends on the last view's right and the equal spacing;
  ;; equal spacing is depended to ALL subview's width and stackview's length
  ;; If any view doesn't have its content ready, equal spacing would return wrong number
  (dolist (subview (uikit-subview-list-of stack))
    (uikit-draw subview)))

(cl-defmethod uikit-content-width-of ((stack uikit-stackview))
  "Return the literal content width of STACK from left to right on screen.
Doesn't change base on orientation."
  (uikit--orientation-h/v stack
                          (+ (cl-reduce '+ (mapcar 'uikit-content-width-of (uikit-subview-list-of stack)))
                             (if (equal (uikit-autolayout-of stack) 'stacking)
                                 (* (1- (length (uikit-subview-list-of stack)))
                                    (uikit--ensure>=0 (uikit-stacking-space-of stack)))
                               0))
                          (uikit-max-subview-height-of stack)))

(cl-defmethod uikit-content-height-of ((stack uikit-stackview))
  "Return the literal content height of STACK from top to bottom on screen.
Doesn't change base on orientation."
  (uikit--orientation-h/v stack
                          (uikit-max-subview-height-of stack)
                          (+ (cl-reduce '+ (mapcar 'uikit-content-height-of (uikit-subview-list-of stack)))
                             (if (equal (uikit-autolayout-of stack) 'stacking)
                                 (* (1- (length (uikit-subview-list-of stack)))
                                    (uikit--ensure>=0 (uikit-stacking-space-of stack)))
                               0))))

(defun uikit--clear-drawing-cache (view)
  "Clear the cached constrains of VIEW and its (possibly) subviews."
  (when (cl-typep view 'uikit-stackview)
    (mapc 'uikit--clear-drawing-cache (uikit-subview-list-of view))
    (setf (uikit-equal-spacing-space-cache-of view) nil
          (uikit-max-subview-height-cache-of view) nil))
  (setf (uikit-left-cache-of view) nil
        (uikit-right-cache-of view) nil
        (uikit-top-cache-of view) nil
        (uikit-bottom-cache-of view) nil
        (uikit-width-cache-of view) nil
        (uikit-height-cache-of view) nil))

(cl-defmethod uikit-content-changed ((stack uikit-stackview))
  "Mark every subview of STACK as content changed."
  (mapc #'uikit-content-changed (uikit-subview-list-of stack)))



;;;;; Helpers

(defun uikit-subview-append (stackview &rest view-list)
  "Append views in VIEW-LIST to STACKVIEW's `subview-list'."
  ;; TOTEST
  (setf (uikit-subview-list-of stackview )
        (append (uikit-subview-list-of stackview)
                view-list)))

(defun uikit-subview-push (stackview &rest view-list)
  "Push views in VIEW-LIST to STACKVIEW's `subview-list'."
  ;; TOTEST
  (setf (uikit-subview-list-of stackview )
        (append (reverse view-list)
                (uikit-subview-list-of stackview))))

(defun uikit-subview-delete (stackview &rest view-list)
  "Remove every view in VIEW-LIST from STACKVIEW."
  ;; TOTEST
  ;; TODO optimize?
  (mapc (lambda (view) (delete view (uikit-subview-list-of stackview))) view-list))

(defun uikit-subview-drop (stackview &optional index)
  "Drop subview at INDEX of STACKVIEW.
If INDEX is nil, drop the last one."
  ;; TOTEST
  (-remove-at (or index (1- (length (uikit-subview-list-of stackview))))
              (uikit-subview-list-of stackview)))

(defmacro uikit-dosubview (var stackview &rest body)
  "Bind VAR to each subviews of STACKVIEW.
And evaluate BODY for each."
  (declare (indent 2))
  `(dolist (,var (uikit-subview-list-of stackview))
     ,@body))

(defmacro uikit-mapc-subview (func stackview)
  "Call `mapc' with FUNC and subview list of STACKVIEW."
  `(mapc #',func (uikit-subview-list-of stackview)))

;;;; Auto Layout

;; SCRATCH
;; (cl-letf (((symbol-function 'cdr) (symbol-function 'car))
;;           ((symbol-function 'car) (symbol-function 'cdr)))
;;   (cl-letf (((symbol-function 'cdr) (symbol-function 'car))
;;             ((symbol-function 'car) (symbol-function 'cdr)))
;;     (car '(1 2 3 4))))
;; 1

;; (cl-letf (((symbol-function 'cdr) 'car)
;;           ((symbol-function 'car) 'cdr))
;;   (cl-letf (((symbol-function 'cdr) 'car)
;;             ((symbol-function 'car) 'cdr))
;;     (car '(1 2 3 4))))
;; END_SCRATCH

(defmacro uikit--with-orientation (stackview &rest body)
  "Evaluate BODY with orientation transformation of STACKVIEW.
orientation can be 'left/bottom/right/top.
'left: rotate counterclockwise 0 degree;
'bottom:                       90 degree;
'right:                        180 degree;
'top:                          270 degree."
  (declare (indent defun))
  `(let ((orientation (pcase (uikit-orientation-of stackview)
                        ('left 0)
                        ('bottom 1)
                        ('right 2)
                        ('top 3))))
     (let ((uikit-left-of (eval (nth orientation '(uikit-left-of uikit-bottom-of uikit-right-of uikit-top-of))))
           (uikit-right-of (eval (nth orientation '(uikit-right-of uikit-top-of uikit-left-of uikit-bottom-of))))
           (uikit-top-of (eval (nth orientation '(uikit-top-of uikit-left-of uikit-bottom-of uikit-right-of))))
           (uikit-bottom-of (eval (nth orientation '(uikit-bottom-of uikit-right-of uikit-top-of uikit-left-of))))
           (uikit-width-of (eval (nth orientation '(uikit-width-of uikit-height-of uikit-width-of uikit-height-of))))
           (uikit-height-of (eval (nth orientation '(uikit-height-of uikit-width-of uikit-height-of uikit-width-of))))
           (uikit-go-up (eval (nth orientation '(uikit-go-up uikit-go-left uikit-go-down uikit-go-right))))
           (uikit-go-left (eval (nth orientation '(uikit-go-left uikit-go-down uikit-go-right uikit-go-up))))
           (uikit-go-down (eval (nth orientation '(uikit-go-down uikit-go-right uikit-go-up uikit-go-left))))
           (uikit-go-right (eval (nth orientation '(uikit-go-down uikit-go-right uikit-go-up uikit-go-left)))))
       ,@body)))

(defun uikit-autolayout (stackview)
  "Ask STACKVIEW to auto layout."
  (uikit--with-orientation stackview
    (let* ((uikit--drawing nil) ; this should default to nil, but just to make sure
           (left (lambda (view) (uikit-left-of stackview)))
           (space-func (pcase (uikit-autolayout-of stackview) ; function that returns the space length between subviews
                         ('stacking (lambda () (uikit-stacking-space-of stackview)))
                         ('equal-spacing (lambda () (uikit-equal-spacing-space-of stackview)))
                         (_ (message "Warning: No autolayout type (\"autolayout\" slot) provided for %s" (uikit-id-of stackview))))))
      (dolist (subview (uikit-subview-list-of stackview))
        ;; autolayout yourself if you are a stackview
        (when (cl-typep subview 'uikit-stackview)
          (uikit-autolayout subview))
        ;; set your parent
        (setf (uikit-parent-stackview-of subview) stackview)
        ;; set your left to the left value pre-calculated by last subview for you
        ;; you just shut up and use it
        (uikit-left-of subview left)
        ;; calculate left position for next subview
        (setq left (lambda (view)
                     (let ((right (uikit--error-nil
                                   (uikit-right-of subview)
                                   "last subview's right is nil when calculating left for %s"
                                   (uikit-id-of view)))
                           (space (uikit--error-nil
                                   (funcall space-func)
                                   "space from stackview top is nil when calculating left for %s"
                                   (uikit-id-of view))))
                       (+ right space))))
        ;; top & bottom
        (pcase (uikit-v-align-of stackview)
          ('top (message "top") (uikit-top-of subview (lambda (view) (uikit-top-of (uikit-parent-stackview-of view)))))
          ('center (uikit-bottom-of subview (lambda (view)
                                              (uikit-go-up (uikit-bottom-of (uikit-parent-stackview-of view))
                                                           (/ (- (uikit-max-subview-height-of (uikit-parent-stackview-of view))
                                                                 (uikit-content-height-in-parent-of view)) 2)))))
          ('bottom (message "bottom") (uikit-bottom-of subview (lambda (view)
                                                                 (uikit-bottom-of (uikit-parent-stackview-of view))))))))))

;;;; Scene

(uikit-defclass uikit-scene (uikit-stackview)
  ((name
    :initarg :name
    :initform "*UIKit Abstract Scene*"
    :accessor uikit-name-of
    :documentation "The name of this scene."
    :type string)
   (buffer
    :initform nil
    :accessor uikit-buffer-of
    :documentation "The buffer in where the scene is displayed."
    :type (or null buffer))
   (constrain-list
    :initform nil
    :accessor uikit-constrain-list-of
    :documentation "All of the constrains of the subiews of this scene."))
  ;; abstract
  "A scene is like a web page."
  :abstract t)

(cl-defmethod make-instance :after ((scene uikit-scene) &key)
  "Create buffer for scene."
  (setf (uikit-buffer-of scene)
        (let ((buffer (get-buffer-create (uikit-name-of scene))))
          (with-current-buffer buffer
            (setq uikit-buffer-scene scene))
          buffer)))

(defun uikit-configure-constrain (scene)
  ;; TODO
  "Configure constrain for each subview inside scene."
  ())


;;;; App

(uikit-defclass uikit-app (eieio-persistent)
  ((name
    :initarg :name
    :initform "An App"
    :accessor uikit-name-of
    :documentation "The name of the app."
    :type string)
   (scene-ring
    :initform nil
    :accessor uikit-scene-ring-of
    :documentation "The ring of scenes.")
   (current-scene
    :initform nil
    :accessor uikit-current-scene-of
    :documentation "The current scene of the app."
    :type (or null uikit-scene))
   (entry-scene
    :initarg entry-scene
    :initform nil
    :accessor uikit-entry-scene-of
    :documentation "A subclass of `uikit-scene'."))
  "An app is made of a set of related scenes. Subclass it."
  :abstract t)

(cl-defmethod initialize-instance ((app uikit-app) &optional slots)
  "Initialize an app.
Creates a buffer and segue to the entry scene."
  (cl-call-next-method app slots)
  (uikit-segue (setf (uikit-app-current-scene app)
                     (make-instance (uikit-app-entry-scene app)))))

;;; Subclass
;;;; Label

(uikit-defclass uikit-label (uikit-atom-view)
  ((text
    :initarg :text
    :accessor uikit-text-of
    :initform "Label"
    :documentation "The text of label."
    :type string))
  "A label is some text.")

(cl-defmethod uikit-make-content ((label uikit-label))
  "Return content of LABEL."
  ;; TOTEST
  (split-string (uikit-text-of label) "\n"))

;;;; Clickable

(uikit-defclass uikit-clickable ()
  ((mouse-1-func
    :initarg :mouse-1-func
    :initform 'uikit-button-clicked
    :accessor uikit-mouse-1-func-of
    :documentation "The function invoked when mouse-1 clicked on the button.
It can be either a symbol or a lambda."
    :type (or null (satisfies functionp)))
   (mouse-2-func
    :initarg :mouse-2-func
    :initform 'uikit-button-clicked
    :accessor uikit-mouse-2-func-of
    :documentation "The function invoked when mouse-2 clicked on the button.
It can be either a symbol or a lambda."
    :type (or null (satisfies functionp)))
   (mouse-3-func
    :initarg :mouse-3-func
    :initform 'uikit-button-clicked
    :accessor uikit-mouse-3-func-of
    :documentation "The function invoked when mouse-3 clicked on the button.
It can be either a symbol or a lambda."
    :type (or null (satisfies functionp)))
   (return-func
    :initarg :return-func
    :initform 'uikit-button-clicked
    :accessor uikit-return-func-of
    :documentation "The function invoked when RET clicked on the button.
It can be either a symbol or a lambda."
    :type (or null (satisfies functionp)))
   (help
    :initarg :help
    :initform "Click"
    :accessor uikit-help-of
    :documentation "This is displayed when mouse is on button."
    :type string))
  "A clickable item."
  :abstract t)

(cl-defmethod initialize-instance ((button uikit-clickable) &optional slots)
  "Add help to properties."
  ;; TOTEST
  (cl-call-next-method button slots)
  (setf (uikit-keymap-of button) (let ((map (make-sparse-keymap)))
                                   (define-key map (kbd "<mouse-1>") (uikit-mouse-1-func-of button))
                                   (define-key map (kbd "<mouse-2>") (uikit-mouse-2-func-of button))
                                   (define-key map (kbd "<mouse-3>") (uikit-mouse-3-func-of button))
                                   (define-key map (kbd "<return>") (uikit-return-func-of button))
                                   map)))
;;;; Editable
(uikit-defclass uikit-editable ()
  ((edit-mode
    :initform nil
    :accessor uikit-edit-mode-of
    :type boolean
    :documentation "Whether the table is in delete mode.
In this mode cells are appended with a DELETE button,
and the table is appended with a ADD button to add new cell.")
   (edit-mode-hook
    :initargg :edit-mode-hook
    :initform nil
    :accessor uikit-edit-mode-hook-of
    :documentation "The hook that runs when toggling edit mode.
The hook should take the view as argument."))
  "A view that have edit-mode."
  :abstract t)

(cl-defmethod uikit-edit-mode :after ((view uikit-editable) &optional onoff)
  "Run `edit-mode-hook'."
  (run-hook-with-args (uikit-edit-mode-hook-of view) view))

;;;; Button

;;;;; Variables

(defface uikit-button-face '((default . (:box t :weight bold)))
  "Face used by uikit-button.")

(defface uikit-button-mouse-face '((t . (:inherit highlight)))
  "Face used when mouse over a uikit button.")

;;;;; Class

(uikit-defclass uikit-button (uikit-label uikit-clickable)
  ((text ;; override label.text
    :initform "Button"
    :type string))
  "A Button.")

(cl-defmethod initialize-instance ((button uikit-clickable) &optional slots)
  "Add help to properties."
  ;; TOTEST
  (cl-call-next-method button slots)
  (setf (uikit-face-of button) 'uikit-button-face)
  (setf (uikit-property-list-of button)
        (append `(help-echo ,(uikit-help-of button) mouse-face uikit-button-mouse-face) (uikit-property-list-of button))))


(defun uikit-button-clicked ()
  "Button clicked by user."
  (interactive)
  (message "Button pressed"))

;;;; Table
;;;;; Class
(uikit-defclass uikit-table-body (uikit-stackview)
  ((autolayout
    :initform 'stacking)
   (orientation
    :initform 'top)
   (v-align
    :initform 'bottom))
  "A table's body.")

(uikit-defclass uikit-table (uikit-stackview)
  ((autolayout
    :initform 'stacking)
   (orientation
    :initform 'top)
   (v-align
    :initform 'bottom)
   (before-add-cell-hook
    :initarg :before-add-cell-hook
    :initform nil
    :accessor uikit-before-add-cell-hook-of
    :documentation "Hooks run before table adds a cell. Each hook is called by arglist: (table cell).")
   (after-add-cell-hook
    :initarg :after-add-cell-hook
    :initform nil
    :accessor uikit-after-add-cell-hook-of
    :documentation "Hooks run after table adds a cell. Each hook is called by arglist: (table cell).")
   (before-delete-cell-hook
    :initarg :before-delete-cell-hook
    :initform nil
    :accessor uikit-before-delete-cell-hook-of
    :documentation "Hooks run before table deletes a cell. Each hook is called by arglist: (table cell).")
   (after-delete-cell-hook
    :initarg :after-delete-cell-hook
    :initform nil
    :accessor uikit-after-delete-cell-hook-of
    :documentation "Hooks run after table deletes a cell. Each hook is called by arglist: (table cell).")
   (mouse-over-face
    :initarg :mouse-over-face
    :initform nil
    :accessor uikit-mouse-over-face-of
    :documentation "The face used when mouse is over a cell."
    :type (or null (satisfies facep)))
   (new-cell-func
    :initarg :new-cell-func
    :initform (lambda (table) (uikit-add-cell table (uikit-plain-cell :title "A Cell")))
    :accessor uikit-new-cell-func-of
    :documentation "The function that takes the table as argument
and return a cell (to be added to table)."
    :type (or null (satisfies functionp)))
   (cell-list
    :documentation "The cell list of table. This is a virtual slot.")
   (header
    :initform nil
    :type (or null uikit-table-cell)
    :documentation "The header of table, a `uikit-cell'.")
   (footer
    :initform nil
    :type (or null uikit-table-cell)
    :documentation "The footer of table, a `uikit-cell'.")
   (body
    :initform nil
    :type (or null uikit-table-body)
    :documentation "The body of titled table, a `uikit-table'.")
   (title
    :initform ""
    :type string
    :documentation "The tile of table. It is displayed in header if non-nil.")
   (footnote
    :initform ""
    :type string
    :documentation "The footnote of table. It is displayed in footer if non-nil."))
  "A table with title and footnote.")

(cl-defmethod initialize-instance ((table uikit-table) &optional slots)
  "Set title and footnote if there is any."
  (cl-call-next-method table slots)
  (setf (uikit-header-of table) (uikit-plain-cell)
        (uikit-footer-of table) (uikit-plain-cell)
        (uikit-body-of table) (uikit-table-body))
  (setf (uikit-title-of table) (slot-value table 'title)
        (uikit-footnote-of table) (slot-value table 'footnote)
        (uikit-cell-list-of table) (slot-value table 'cell-list))
  (slot-makeunbound table 'title)
  (slot-makeunbound table 'footnote)
  (slot-makeunbound table 'cell-list)
  (setf (uikit-subview-list-of table)
        (list (uikit-header-of table) (uikit-body-of table) (uikit-footer-of table))))

;;;;;; Virtual Slots

(cl-defmethod (setf uikit-cell-list-of) (cell-list (table uikit-table))
  "Set cell-list of TABLE to CELL-LIST."
  (setf (uikit-subview-list-of (uikit-body-of table))
        cell-list))

(cl-defmethod uikit-cell-list-of ((table uikit-table))
  "Return the cell-list slot of TABLE."
  (uikit-subview-list-of (uikit-body-of table)))

;; To make `slot-value' and `set-slot-value' also respect virtual slot,
;; we don't declare the slot in class definition,
;; then `slot-missing' will be called
;; (cl-defmethod slot-missing ((object uikit-table) (slot-name (eql cell-list)) _operation &optional _new-value)
;;   "If SLOT-NAME is `cell-list' call the corresponding reader and writer.
;; Other arguments(OBJECT, _OPERATION, _NEW-VALUE) are the same as in `slot-missing'."
;;   (if (eq _operation 'oset)
;;       ;;set
;;       (setf (uikit-cell-list-of object) new-value)
;;     ;;get
;;     (uikit-cell-list-of object)))

(cl-defmethod (setf uikit-title-of) (title (table uikit-table))
  "Change TABLE's title to TITLE."
  (setf (uikit-title-of (uikit-header-of table)) title))

(cl-defmethod uikit-title-of ((table uikit-table))
  "Return TABLE's title."
  (uikit-title-of (uikit-header-of table)))

(cl-defmethod (setf uikit-footnote-of) (footnote (table uikit-table))
  "Change TABLE's footnote to FOOTNOTE."
  (setf (uikit-title-of (uikit-footer-of table)) footnote))

(cl-defmethod uikit-footnote-of ((table uikit-table))
  "Return TABLE's footnote."
  (uikit-title-of (uikit-footer-of table)))

;;;;; Methods
(cl-defmethod uikit-cell-append ((table uikit-table) cell)
  "Add CELL to the end of TABLE.
Use this function instead of `uikit-subview-append' for tables."
  (run-hook-with-args (uikit-before-add-cell-hook-of table) table cell)
  (uikit-subview-append (uikit-body-of table) cell)
  (run-hook-with-args (uikit-after-add-cell-hook-of table) table cell))

(cl-defmethod uikit-cell-push ((table uikit-table) cell)
  "Add CELL to the beginning TABLE.
Use this function instead of `uikit-subview-push' for tables."
  (run-hook-with-args (uikit-before-add-cell-hook-of table) table cell)
  (uikit-subview-push (uikit-body-of table) cell)
  (run-hook-with-args (uikit-after-add-cell-hook-of table) table cell))

(cl-defmethod uikit-cell-delete ((table uikit-table) cell)
  "Remove every cell equal to CELL from TABLE.
Use this function instead of `uikit-subview-delete'."
  (run-hook-with-args (uikit-before-delete-cell-hook-of table) table cell)
  (uikit-subview-delete (uikit-body-of table) cell)
  (run-hook-with-args (uikit-after-delete-cell-hook-of table) table cell))

(cl-defmethod uikit-cell-drop ((table uikit-table) &optional index)
  "Drop cell at INDEX of TABLE.
If INDEX is nil, drop the last one.
Use this function instead of `uikit-subview-drop'."
  (run-hook-with-args (uikit-before-delete-cell-hook-of table) table cell)
  (uikit-subview-drop (uikit-body-of table) index)
  (run-hook-with-args (uikit-after-delete-cell-hook-of table) table cell))


;;;; Editable Table
;;;;; Class
(uikit-defclass uikit-editable-table (uikit-table uikit-editable)
  ((add-button-cell
    :initarg :add-button-cell
    :accessor uikit-add-button-cell-of
    :documentation "The cell with a button that can add a new cell to
the table in edit mode."
    :type uikit-table-cell)
   (edit-button
    :documentation "The button that on click, will toggle `uikit-edit-mode' of table."
    :type uikit-button))
  "Table with edit-mode - add/delete buttons are added to table temperately.")

(cl-defmethod initialize-instance ((table uikit-editable-table) &optional slots)
  "Set add button cell and header edit button for TABLE."
  (cl-call-next-method table slots)
  (uikit-subview-append (uikit-header-of table)
                        (setf (uikit-edit-button-of table)
                              (uikit-button :text "EDIT"
                                            :mouse-1-func
                                            (lambda () (interactive)
                                              (uikit-edit-mode table)))))
  (setf (uikit-add-button-cell-of table)
        (uikit-table-cell
         :subview-list
         (list (make-instance
                'uikit-button
                :text " + "
                :mouse-1-func (lambda () (interactive)
                                (uikit-cell-append table
                                                   (funcall (uikit-new-cell-func-of table) table))))))))

;;;;; Edit Mode
(defun uikit-toggle-edit-mode ()
  ;; TODO toggle edit mode of view at point
  )

(cl-defmethod uikit-edit-mode ((table uikit-editable-table) &optional onoff)
  "Toggle edit mode of TABLE.
If ONOFF is non-nil, use it to control on/off:
ONOFF >= 0 is on, ONOFF < 0 is off.
If ONOFF is nil, toggle on if currently off and vice versa."
  (if (if onoff
          (< onoff 0)
        (uikit-edit-mode-of table))
      ;; turn off
      (progn
        (setf (uikit-edit-mode-of table) nil)
        (uikit-mapc-subview (lambda (cell) (when (not (equal cell (uikit-header-of table))
                                                      (equal cell (uikit-footer-of table)))
                                             (uikit-edit-mode cell -1))) table)
        (uikit-subview-delete table (uikit-add-button-cell-of table)))
    ;; turn on
    (setf (uikit-edit-mode-of table) t)
    (uikit-mapc-subview (lambda (cell) (when (not (equal cell (uikit-header-of table))
                                                  (equal cell (uikit-footer-of table)))
                                         (uikit-edit-mode cell 1))) table)
    (uikit-subview-append table (uikit-add-button-cell-of) table))
  (uikit-content-changed table))

(cl-defmethod uikit-edit-mode ((cell uikit-table-cell) &optional onoff)
  "Toggle edit mode of CELL.
If ONOFF >= 0, toggle on, otherwise toggle off.
If ONOFF is nil, toggle on if currently off and vice versa."
  (if (if onoff
          (< onoff 0)
        (uikit-edit-mode-of cell))
      ;; turn off: remove delete button
      (progn
        (setf (uikit-edit-mode-of cell) nil)
        (uikit-subview-delete cell (uikit-delete-button-of cell)))
    ;; turn on: add delete button
    (setf (uikit-edit-mode-of cell) t)
    (uikit-subview-append cell (uikit-delete-button-of cell))))

;;;; Table Cell

(uikit-defclass uikit-table-cell (uikit-stackview)
  ((autolayout
    :initform 'stacking)
   (delete-button
    :initarg :delete-button
    :accessor uikit-delete-button-of
    :documentation "The delete button of cell."
    :type uikit-button))
  "A cell of a table.")

(cl-defmethod initialize-instance ((cell uikit-table-cell) &optional slots)
  "Set delete-button for CELL."
  (cl-call-next-method cell slots)
  (setf (uikit-delete-button-of cell)
        (uikit-button :text " X "
                      :mouse-1-func (lambda ()
                                      (interactive)
                                      (uikit-delete-cell cell)))))


;;;; Plain Cell
;;;;; Class
(uikit-defclass uikit-plain-cell (uikit-table-cell)
  ((title
    :initarg :title
    :initform "Cell"
    :accessor uikit-title-of
    :type string
    :documentation "The title of the cell.")
   (title-label
    :accessor uikit-title-label-of
    :type uikit-label
    :documentation "The label displayed by cell.")))

(cl-defmethod initialize-instance ((cell uikit-plain-cell) &optional slots)
  "Create cell contents."
  (cl-call-next-method cell slots)
  (setf (uikit-title-label-of cell) (uikit-label)
        (uikit-text-of (uikit-title-label-of cell)) (slot-value cell 'title))
  (uikit-subview-append cell (uikit-title-label-of cell))
  (slot-makeunbound cell 'title))

;;;;; Virtual slots
(cl-defmethod (setf uikit-title-of) (title (cell uikit-plain-cell))
  "Change `title' of CELL to TITLE."
  (setf (uikit-text-of (uikit-title-label-of cell)) title))

(cl-defmethod uikit-title-of ((cell uikit-plain-cell))
  "Return `title' of CELL."
  (uikit-text-of (uikit-title-label-of cell)))




;;; Debug Helper

(defun uikit-top-view-at-point (&optional point)
  "Return the top (most inner) view at POINT or (point)."
  (get-text-property (or point (point)) 'uikit-view))

(provide 'uikit)

;;; uikit.el ends here
