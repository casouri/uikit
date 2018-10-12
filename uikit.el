;;; uikit.el --- UI Kit for Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Yuan Fu

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;

;;; Code:
;;


(require 'eieio-base)

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

(defface uikit-button-face '((t (:weight bold :underline t)))
  "Face used for UIKit buttons."
  :group 'uikit)

(defface uikit-mouse-face '((t (:inherit highlight)))
  "Face used when mouse is on button."
  :group 'uikit)

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

;;; Base Class

;;;; View

(defclass uikit-view ()
  (;; required in init
   (id
    :initarg :id
    :accessor uikit--id-of
    :documentation "The identification of this view. Should be a string.
Uikit creates a variable`uikit--id-ID' for the view,
so make sure to include app name as prefix to avoid name clash.
(Yeah you never escape from prefix)"
    :type string)
   ;; not required in init
   (left
    :accessor uikit--raw-left-of
    :initform nil
    :documentation "The left of view. Can be positive integer of (lambda (SELF)).")
   (right
    :accessor uikit--raw-right-of
    :initform nil
    :documentation "The right of view. Can be positive integer of (lambda (SELF)).")
   (top
    :accessor uikit--raw-top-of
    :initform nil
    :documentation "The top of view. Can be positive integer of (lambda (SELF)).")
   (bottom
    :accessor uikit--raw-bottom-of
    :initform nil
    :documentation "The bottom of view. Can be positive integer of (lambda (SELF)).")
   (width
    :accessor uikit--raw-width-of
    :initform nil
    :documentation "The width of view. Can be positive integer of (lambda (SELF)).")
   (height
    :accessor uikit--raw-height-of
    :initform nil
    :documentation "The height of view. Can be positive integer of (lambda (SELF)).")
   (left-cache
    :accessor uikit--left-cache-of
    :initform nil
    :type (or null integer)
    :documentation "Cache of `left' of the view during drawing process.")
   (right-cache
    :accessor uikit--right-cache-of
    :initform nil
    :type (or null integer)
    :documentation "Cache of `right-cache' of the view during drawing process.")
   (top-cache
    :accessor uikit--top-cache-of
    :initform nil
    :type (or null integer)
    :documentation "Cache of `top' of the view during drawing process.")
   (bottom-cache
    :accessor uikit--bottom-cache-of
    :initform nil
    :type (or null integer)
    :documentation "Cache of `bottom' of the view during drawing process.")
   (width-cache
    :accessor uikit--width-cache-of
    :initform nil
    :type (or null integer)
    :documentation "The `width' cache.")
   (height-cache
    :accessor uikit--height-cache-of
    :initform nil
    :type (or null integer)
    :documentation "The `height' cache."))
  "Parent of `uikit-atom-view' and `uikit-stackview'.
Both be a subview of one another."
  :abstract t)

;;;;; Process :id

(defsubst uikit-by-id (id)
  "Return the view by ID."
  (intern (format "uikit//%s" id)))

(cl-defmethod initialize-instance :after ((view uikit-view) &rest _)
  "Process the :id key. Create a variable `uikit--id-ID' as an alias of the view.
e.g. uikit--id-mybutton for id \"mybutton\"."
  (setf (symbol-value (intern (format "uikit//%s" (uikit--id-of view))))
        view))

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

(defmacro uikit--make-special-accessor (constrain form)
  "Define special accessor for CONSTRAIN with FORM.
FORM is calculation logic."
  (let ((raw-xx-of (intern (format "uikit--raw-%s-of" (symbol-name constrain))))
        ;; (raw-xx (intern (format "raw-%s" (symbol-name constrain))))
        (xx-cache-of (intern (format "uikit--%s-cache-of" (symbol-name constrain)))))
    `(defun ,(intern (format "uikit-%s-of" (symbol-name constrain))) (view &optional value self-recure)
       ,(format "Takes any VIEW of `uikit-view' class and return its `%s' slot.
If `%s' slot is nil, calculate it.
Set the slot to VALUE when VALUE non-nil.
VALUE can be a positive integer or a (quoted) form.

Form will be evaluated to get a number during drawing of VIEW,
make sure it returns a positive integer.

SELF-RECURE is set to t when a view tries to calculate its constrain
from it's own constrains, e.g. right = left + width.
That might end up in infinite recursion." (symbol-name constrain) (symbol-name constrain))
       (if value
           (setf (,raw-xx-of view) value)
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
                              (error (message "Error calculating constrain of %s: %s"
                                              (uikit--id-of view)
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
           ;; not drawing, return the raw-constrain, let it be a number or form
           (,raw-xx-of view))))))

;; simply return nil if operand(s) is(are) nil
(uikit--make-special-accessor left (ignore-errors (- (uikit-right-of view nil t) (uikit-width-of view nil t))))
(uikit--make-special-accessor right (ignore-errors (+ (uikit-left-of view nil t) (uikit-width-of view nil t))))
(uikit--make-special-accessor top (ignore-errors (- (uikit-bottom-of view nil t) (uikit-height-of view nil t))))
(uikit--make-special-accessor bottom (ignore-errors (+ (uikit-top-of view nil t) (uikit-height-of view nil t))))
(uikit--make-special-accessor width (ignore-errors (- (uikit-right-of view nil t) (uikit-left-of view nil t))))
(uikit--make-special-accessor height (ignore-errors (- (uikit-bottom-of view nil ) (uikit-top-of view nil t))))

(defun uikit--content-width-of (view)
  "Return the content width of VIEW. Only look at first line of content."
  (length (car (uikit--content-of view))))

(defun uikit--content-height-of (view)
  "Return the content height of VIEW."
  (length (uikit--content-of view)))

(defun uikit-content-changed (view)
  "Clear constrain cache & set `content-chenged' slot of VIEW."
  (setf (uikit--left-cache-of view) nil)
  (setf (uikit--right-cache-of view) nil)
  (setf (uikit--top-cache-of view) nil)
  (setf (uikit--bottom-cache-of view) nil)
  (setf (uikit--width-cache-of view) nil)
  (setf (uikit--height-cache-of view) nil)
  (setf (uikit--left-cache-of view) nil)
  (when (uikit-stackview-p view)
    (setf (uikit--equal-spacing-space-cache-of view) nil))
  (setf (uikit--content-changed-of view) t))

;;;; View

(defclass uikit-atom-view (uikit-view)
  ((face
    :accessor uikit--face-of
    :initarg :face
    :initform nil
    :documentation "The face used for this view. Default to nil.
You can set face by property list but this is more convenient.
face in property list will override this. "
    :type (or null (satisfies facep)))
   (pad-char
    :accessor uikit--pad-char-of
    :initarg :pad-char
    :initform (eval uikit-pad-char)
    :documentation "The char used to pad extra space."
    :type character)
   (keymap
    :accessor uikit--keymap-of
    :initarg :keymap
    :initform nil
    :documentation "Keymap on the view
in addition to the default major and minor mode keymaps. Default to nil.
You can set keymap in property list but this is more convenient.
TODO Does keymap in property list override this?
"
    :type (or null (satisfies keymapp)))
   (property-list
    :accessor uikit--property-list-of
    :initarg :property-list
    :initform nil
    :documentation "Extra text properties that you want to put to view text.
overwrites face and keymap slot.
each element of the list is an cons of PROPERTY and VALUE that are eligibel for `put-text-property'."
    :type (or null list))

   ;; drawing
   (parent-stackview
    :accessor uikit--parent-stackview-of
    :initform nil
    :documentation "The parent stackview of this view.
Used to check if the view is within the stackview on screen.")

   ;; content

   (content
    :accessor uikit--content-of
    :initform ()
    :documentation "The actual text that is drew to canvas, cached for later use.
But it doesn't include the padding spaces around, since size might
change more frequent that the content. See `padded-content'.
It is a list of strings, each string is a line.")
   (padded-content
    :accessor uikit--padded-content-of
    :initform ()
    :documentation "The actual text that is drew on canvas, with padded space.
It is a list for the same reason as `content'.")
   (content-changed
    :accessor uikit--content-changed-of
    :initform t
    :documentation "If this is t, then content needs to be recalculated.
Everythime a change that alters the visual appearance of the view is made,
this should be set to t. But don't change this value directly, use `uikit-changed'.
Typically this value is changed to t in`uikit-make-content' and to nil in `uikit-draw'."
    :type boolean))
  "A view is like a widget. It is the smallest unit of an UI.
his class is an abstract class."
  :abstract t)

(cl-defmethod uikit-make-content ((_ uikit-atom-view))
  "Make content from data, returna a list of strings, each string is a line.
Each line must have same length and should not contain any return char."
  ;; abstract
  nil)

(cl-defmethod uikit-make-content :around ((view uikit-atom-view))
  "Make content if `content-changed' is t.
This function returns the content just for usability.
It's main job is to (potentially) update the content of VIEW.
Instead, it sets `content-changed', `width', `height', `content' slots.
So the specific `uikit-make-content' of each view class has to return their content."
  (if (uikit--content-changed-of view)
      (let ((content (cl-call-next-method view)))
        (setf (uikit--content-changed-of view) t
              (uikit--raw-width-of view) (length (car content))
              (uikit--raw-height-of view) (length content)
              (uikit--content-of view) content)) ; last val will be returned
    ;; content not changed, just return it
    (uikit--content-of view)))

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
  (let ((content (uikit--content-of view))
        (all-property (append `(uikit-view ,view
                                           face ,(uikit--face-of view)
                                           keymap ,(uikit--keymap-of view))
                              (uikit--property-list-of view)))
        (inhibit-modification-hooks t)
        (width (or (uikit-width-of view) (uikit--content-width-of view)))
        (height (or (uikit-height-of view) (uikit--content-height-of view)))
        line
        (uikit--drawing t)
        (inhibit-modification-hooks t)
        (undo-inhibit-record-point t))
    (dolist (index (number-sequence 0 (1- (length content))))
      (setf line (gv-ref (nth index content)))
      (setf (gv-deref line) (substring (gv-deref line) 0 width))
      (add-text-properties 0 width all-property (gv-deref line)))
    ;; we just set cache position, draw at there
    (uikit-raw-draw (cl-subseq content 0 height)
                    (or x (uikit-left-of view))
                    (or y (uikit-top-of view)))))

;;;; Stackview

(defclass uikit-stackview (uikit-view)
  ((subview-list
    :accessor uikit--subview-list-of
    :initarg :subview-list
    :initform nil
    :documentation "List of subviews of the stack view.")
   (autolayout
    :accessor uikit--autolayout-of
    :initarg :autolayout
    :initform nil
    :documentation "Determins how does stackview \"stack\" subviews together.
It can be nil, 'stacking, 'equal-spacing, 'portion."
    :type symbol)
   (stacking-space
    :accessor uikit--stacking-space-of
    :initform 0
    :type integer
    :documentation "The space between subviews when using 'stacking autolayout.")
   (equal-spacing-space-cache
    :accessor uikit--equal-spacing-space-cache-of
    :initform nil
    :type (or null integer)
    :documentation "Spacing between subviews when using 'equal-spacing auatolayout.")
   (v-align
    :accessor uikit--v-align-of
    :initform 'top
    :type symbol
    :documentation "Vertical alignment of subviews. Can be 'top, 'center, 'bottom.
Only take effect when `autolayout' is non-nil.")
   (orientation
    :accessor uikit--orientation-of
    :initform 'left
    :type symbol
    :documentation "The orientation of the stackview. 'left (default) means normal orientation.
'top means rotate clockwise: top becomes left, left becomes bottom, etc.")
   (portion-list
    :accessor uikit--portion-list-of
    :initform nil
    :documentation "A list of portions of subviews in stackview in order when using 'portion autolayout.")
   (max-subview-height
    :accessor uikit--max-subview-height
    :initform (byte-compile (lambda (stackview) (cl-reduce
                                                 '+
                                                 (mapcar 'uikit--height-of
                                                         (uikit--subview-list-of stackview)))))
    :documentation "The height of the tallest subview in stackview.")
   (max-subview-height-cache
    :accessor uikit--max-subview-height-cache
    :initform nil
    :documentation "Cache of `max-subview-height'."))
  "Stack view, used for grouping multiple view together and arrage their position automatically.")

(defun uikit-equal-spacing-space-of (stackview)
  "Return the space between subviews of STACKVIEW."
  (or (uikit--equal-spacing-space-cache-of stackview)
      (condition-case nil
          (let ((subview-list (uikit--subview-list-of stackview)))
            ;; (stackview width - sum of subviews' width ) / (subview number - 1)
            (floor (/ (let ((result (- (uikit-width-of stackview)
                                       (cl-reduce '+ (mapcar 'uikit--content-width-of subview-list)))))
                        (if (< result 0)
                            0
                          result))
                      (1- (length subview-list)))))
        ((debug error) (message "Not enough constrain for %s. Cannot calculate equal-spacing-space constrain of it"
                                (uikit--id-of stackview))))))

(cl-defmethod uikit-make-content ((stackview uikit-stackview))
  "STACKVIEW make content by asking its subviews to make content."
  ;; TOTEST
  (dolist (subview (uikit--subview-list-of stackview))
    (uikit-make-content subview)))

(cl-defmethod uikit-quit ((stackview uikit-stackview))
  "Quit stackview and set all subviews to nil."
  ;; TODO not finished
  (dolist (subview (uikit--subview-list-of stackview))
    (uikit-quit subview))
  (setf stackview nil))

(cl-defmethod uikit-draw ((stack uikit-stackview))
  "Draw the stackview."
  (dolist (subview (uikit--subview-list-of stack))
    (uikit-make-content subview))
  ;; Update content has to be separated from drawing (more specifically, before that)
  ;; because EVERY view has to have their content ready before
  ;; ANY view starts to calculate its position.
  ;; For example, a sbview in a equal spacing stackview calculates
  ;; its left depends on the last view's right and the equal spacing;
  ;; equal spacing is depended to ALL subview's width and stackview's length
  ;; If any view doesn't have its content ready, equal spacing would return wrong number
  (dolist (subview (uikit--subview-list-of stack))
    (uikit-draw subview)))

;;;;; Constrain

;;;;;; Auto Layout

(defun uikit-autolayout (stackview)
  "Ask STACKVIEW to auto layout."
  (let* ((orientation-index (pcase (uikit--orientation-of stackview)
                              ('left 0) ; rotate clockwise 0 degree
                              ('bottom 1) ; rotate 90 degree
                              ('right 2) ; 180 degree
                              ('top 3))) ; 270 degree
         (left (lambda (SELF) (uikit-left-of stackview)))
         (space-func (pcase (uikit--autolayout-of stackview)
                       ('stacking (lambda () (uikit--stacking-space-of stackview)))
                       ('equal-spacing (lambda () (uikit-equal-spacing-space-of stackview)))))
         (top-func (pcase (uikit--v-align-of stackview)
                     ('top (lambda (SELF) (uikit-top-of (uikit--parent-stackview-of SELF))))
                     ('center (lambda (SELF)
                                (+ (uikit-top-of (uikit--parent-stackview-of SELF))
                                   (/ (- (uikit--max-subview-height (uikit--parent-stackview-of SELF))
                                         (uikit-content-height-of SELF)) 2))))
                     ('bottom (lambda (SELF)
                                (- (uikit-bottom-of (uikit--parent-stackview-of SELF))
                                   (uikit-content-width-of SELF)))))))
    (let ((uikit-left-of (nth orientation-index '(uikit-left-of uikit-bottom-of uikit-right-of uikit-top-of)))
          (uikit-bottom-of (nth orientation-index '(uikit-bottom-of uikit-right-of uikit-top-of uikit-left-of)))
          (uikit-right-of (nth orientation-index '(uikit-right-of uikit-top-of uikit-left-of uikit-bottom-of)))
          (uikit-top-of (nth orientation-index '(uikit-top-of uikit-left-of uikit-bottom-of uikit-right-of))))
      ;; during the drawing process `uikit-<constrain>-of' will be
      ;; bind to another function that calculates the constrain into
      ;; actual number and saves to cache
      (dolist (subview (uikit--subview-list-of stackview))
        ;; autolayout yourself if you are a stackview
        (when (cl-typep subview 'uikit-stackview)
          (uikit-autolayout subview))
        ;; set your parent
        (setf (uikit--parent-stackview-of subview) stackview)
        ;; set your left to the left value pre-calculated by last subview for you
        ;; just shut up and use it, no complain
        (uikit-left-of subview left)
        ;; calculate left position for next subview
        (setq left (lambda (SELF)
                     (let ((left (+ (uikit-right-of subview)
                                    (funcall space-func))))
                       (uikit-right-of subview)
                       (funcall space-func)
                       left)))
        ;; top & bottom
        (uikit-top-of subview top-func)))))

;;;; Scene

(defclass uikit-scene (uikit-stackview)
  ((name
    :initarg :name
    :initform "*UIKit Abstract Scene*"
    :accessor uikit--name-of
    :documentation "The name of this scene."
    :type string)
   (buffer
    :initform nil
    :accessor uikit--buffer-of
    :documentation "The buffer in where the scene is displayed."
    :type (or null buffer))
   (constrain-list
    :initform nil
    :accessor uikit--constrain-list-of
    :documentation "All of the constrains of the subiews of this scene."))
  ;; abstract
  "A scene is like a web page."
  :abstract t)

(cl-defmethod make-instance :after ((scene uikit-scene) &key)
  "Create buffer for scene."
  (setf (uikit--buffer-of scene)
        (let ((buffer (get-buffer-create (uikit--name-of scene))))
          (with-current-buffer buffer
            (setq uikit-buffer-scene scene))
          buffer)))

(defun uikit-configure-constrain (scene)
  ;; TODO
  "Configure constrain for each subview inside scene."
  ())


;;;; App

(defclass uikit-app (eieio-persistent)
  ((name
    :initarg :name
    :initform "An App"
    :accessor uikit--name-of
    :documentation "The name of the app."
    :type string)
   (scene-ring
    :initform nil
    :accessor uikit--scene-ring-of
    :documentation "The ring of scenes.")
   (current-scene
    :initform nil
    :accessor uikit--current-scene-of
    :documentation "The current scene of the app."
    :type (or null uikit-scene))
   (entry-scene
    :initarg entry-scene
    :initform nil
    :accessor uikit--entry-scene-of
    :documentation "A subclass of `uikit-scene'."))
  "An app is made of a set of related scenes. Subclass it."
  :abstract t)

(cl-defmethod initialize-instance :after ((app uikit-app &key))
  "Initialize an app.
Creates a buffer and segue to the entry scene."
  (uikit-segue (setf (uikit-app-current-scene app)
                     (make-instance (uikit-app-entry-scene app)))))

;;; Subclass

;;;; Label

(defclass uikit-label (uikit-atom-view)
  ((text
    :initarg :text
    :accessor uikit--text-of
    :initform "Text"
    :documentation "The text of label."
    :type string))
  "A label is a one-line text.")

(cl-defmethod uikit-make-content ((label uikit-label))
  "Return content of LABEL."
  ;; TOTEST
  (split-string (uikit--text-of label) "\n"))

;;;; Button

(defclass uikit-button (uikit-label)
  ((text ;; override label.text
    :initform "Button")
   (mouse-1-func
    :initarg :mouse-1-func
    :initform 'uikit-button-clicked
    :accessor uikit--mouse-1-func-of
    :documentation "The function invoked when mouse-1 clicked on the button.
It can be either a symbol or a lambda.")
   (mouse-2-func
    :initarg :mouse-2-func
    :initform 'uikit-button-clicked
    :accessor uikit--mouse-2-func-of
    :documentation "The function invoked when mouse-2 clicked on the button.
It can be either a symbol or a lambda.")
   (mouse-3-func
    :initarg :mouse-3-func
    :initform 'uikit-button-clicked
    :accessor uikit--mouse-3-func-of
    :documentation "The function invoked when mouse-3 clicked on the button.
It can be either a symbol or a lambda.")
   (return-func
    :initarg :return-func
    :initform 'uikit-button-clicked
    :accessor uikit--return-func-of
    :documentation "The function invoked when RET clicked on the button.
It can be either a symbol or a lambda.")
   (help
    :initarg :help
    :initform "Click"
    :accessor uikit--help-of
    :documentation "This is displayed when mouse is on button."
    :type string))
  "A button.")


(cl-defmethod initialize-instance :after ((button uikit-button) &rest _)
  "Add help to properties."
  ;; TOTEST
  (setf (uikit--keymap-of button) (let ((map (make-sparse-keymap)))
                                    (define-key map (kbd "<mouse-1>") (uikit--mouse-1-func-of button))
                                    (define-key map (kbd "<mouse-2>") (uikit--mouse-2-func-of button))
                                    (define-key map (kbd "<mouse-3>") (uikit--mouse-3-func-of button))
                                    (define-key map (kbd "<return>") (uikit--return-func-of button))
                                    map))
  (setf (uikit--face-of button) 'uikit-button-face)
  (setf (uikit--property-list-of button)
        (append `(help-echo ,(uikit--help-of button) mouse-face uikit-mouse-face) (uikit--property-list-of button))))


(defun uikit-button-clicked ()
  "Button clicked by user."
  (interactive)
  (message "Button pressed"))

;;; Debug

(defun uikit-top-view-at-point (&optional point)
  "Return the top (most inner) view at POINT or (point)."
  (get-text-property (or point (point)) 'uikit-view))

(provide 'uikit)

;;; uikit.el ends here
