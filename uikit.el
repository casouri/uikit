;;; uikit.el --- UI kit for Emacs

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

;;; Base structs

(cl-defstruct uikit-pos
 "Represents a position in cavas.
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

                                        ; TODO DOC
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

;;; Base function

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

(defmacro uikit-append (seq elt)
  ;; TOTEST
  "Append ELT to SEQ destructivly. This is a macro."
  `(if ,seq
       (nconc ,seq (list ,elt))
     (setq ,seq (list ,elt))))

(defmacro uikit-push (elt seq)
  ;; TOTEST
  "Push ELT to SEQ destructivly. This is a macro."
  `(if ,seq
       (push ,elt ,seq)
     (setq ,seq (list ,elt))))

(defun uikit-replace-with (str beg)
  "Replace strings between BEG and (+ BEG (length STR)) with STR."
  ;; TOTEST hand tested
  (save-excursion
    (delete-region (1+ beg) (+ 1 beg (length str)))
    (goto-char beg)
    (insert str)))

(defun uikit-goto (pos)
  "Go to `uikit-pos' POS."
  ;; TOTEST
  (goto-char (uikit-pos-to-point pos)))

(defun uikit-pos-to-point (pos)
  "Convert `uikit-pos' POS to point (integer)."
  ;; TOTEST
  (+ 2 (uikit-pos-x pos) (* (1- (uikit-pos-y pos)) (window-body-width))))

(defun uikit-raw-draw (content pos)
  "Draw raw strings to canvas at POS.
CONTENT is a list of strings. Generally you want to make them have same length."
  ;; TOTEST hand tested
  (let ((window-width (window-body-width))
        point)
    (save-excursion
      (uikit-goto pos)
      (setq point (point))
      (dolist (line content)
        ;; ??? maybe cache a window width for current scene
        (uikit-replace-with line point)
        (setq point (+ point window-width))))))

;;; Base Class
;;;; Abstract View

(defclass uikit-abstract-view ()
  (;; required in init
   (id
    :initarg :id
    :documentation "The identification of this view. Should be a symbol."
    :type symbol)
   ;; not required in init
   (left
    :accessor raw-left-of
    :initform nil
    :type (or null integer)
    :documentation "Distance between the left of view and the left of scene.")
   (right
    :accessor raw-right-of
    :initform nil
    :type (or null integer)
    :documentation "Distance between the right of view and the right of scene.")
   (top
    :accessor raw-top-of
    :initform nil
    :type (or null integer)
    :documentation "Distance between the top of view and the top of scene.")
   (bottom
    :accessor raw-bottom-of
    :initform nil
    :type (or null integer)
    :documentation "Distance between the bottom of view and the bottom of scene.")
   (width
    :accessor raw-width-of
    :initform nil
    :type (or null integer)
    :documentation "The width.")
   (height
    :accessor raw-height-of
    :initform nil
    :type (or null integer)
    :documentation "The height.")
   (left-cache
    :accessor left-cache-of
    :initform nil
    :type (or null integer)
    :documentation "Cache of `left' of the view during drawing process.")
   (right-cache
    :accessor right-cache-of
    :initform nil
    :type (or null integer)
    :documentation "Cache of `right-cache' of the view during drawing process.")
   (top-cache
    :accessor top-cache-of
    :initform nil
    :type (or null integer)
    :documentation "Cache of `top' of the view during drawing process.")
   (bottom-cache
    :accessor bottom-cache-of
    :initform nil
    :type (or null integer)
    :documentation "Cache of `bottom' of the view during drawing process.")
   (width-cache
    :accessor width-cache-of
    :initform nil
    :type (or null integer)
    :documentation "The `width' cache.")
   (height-cache
    :accessor height-cache-of
    :initform nil
    :type (or null integer)
    :documentation "The `height' cache."))
  "Parent of `uikit-view' and `uikit-stack'.
Both be a subview of one another."
  :abstract t)

;;;;; Process :id

(defsubst uikit-by-id (id)
  "Return the view by ID."
  (intern (format "uikit--id-%s" id)))

(defmethod initialize-instanc :after ((view uikit-abstract-view) &rest rest)
  "Process the :id key. Create a variable `uikit--id-ID' as an alias of the view.
e.g. uikit--id-mybutton for id \"mybutton\"."
  (setf (intern (format "uikit--id-%s" id)) view))

;;;;; Universal Accessor: Getter & Setter

(defsubst dimension-of (view dimension &optional value)
  "Get/set DIMENSION of VIEW.

If VALUE non-nil, set, otherwise get.
VALUE can be a positive integer or a (quoted) form.

Form will be evaluated to get a number during drawing of VIEW,
make sure it returns a positive integer.

Normally you want to just use `left/right/...-of' accessors,
this function is used by stack during auto layout
because they needs to change left/right/.../ base on their direction."
  (funcall (intern (format "%s-of" (symbol-name dimension))) view value))


;;;;; Special Accessor for View's Dimension

(defun left-of (view &optional value)
  "Takes any VIEW of `abstract-view' class and return its `left' slot.
If `left' slot is nil, calculate by `right' and `width'.
Set the slot to VALUE when VALUE non-nil.
VALUE can be a positive integer or a (quoted) form.

Form will be evaluated to get a number during drawing of VIEW,
make sure it returns a positive integer."
  (if value
      (setf (raw-left-of view) value)
    (let ((raw-left (raw-left-of view)))
      (or raw-left
          (condition-case nil
              (- (raw-right-of view) (raw-width-of view))
            (error "Not enough constrain for %s. Cannot calculate left constrain of it" (id-of view)))))))
(defun right-of (view &optional value)
  "Takes any VIEW of `abstract-view' class and return its `right' slot.
If `right' slot is nil, calculate by `left' and `width'.
Set the slot to VALUE when VALUE non-nil.
VALUE can be a positive integer or a (quoted) form.

Form will be evaluated to get a number during drawing of VIEW,
make sure it returns a positive integer."
  (if value
      (setf (raw-right-of view) value)
    (let ((raw-left (raw-left-of view)))
      (or raw-left
          (condition-case nil
              (+ (raw-left-of view) (raw-width-of view))
            (error "Not enough constrain for %s. Cannot calculate right constrain of it" (id-of view)))))))
(defun top-of (view &optional value)
  "Takes any VIEW of `abstract-view' class and return its `top' slot.
If `top' slot is nil, calculate by `bottom' and `width'.
Set the slot to VALUE when VALUE non-nil.
VALUE can be a positive integer or a (quoted) form.

Form will be evaluated to get a number during drawing of VIEW,
make sure it returns a positive integer."
  (if value
      (setf (raw-top-of view) value)
    (let ((raw-left (raw-left-of view)))
      (or raw-left
          (condition-case nil
              (- (raw-bottom-of view) (raw-height-of view))
            (error "Not enough constrain for %s. Cannot calculate top constrain of it" (id-of view)))))))
(defun bottom-of (view &optional value)
  "Takes any VIEW of `abstract-view' class and return its `bottom' slot.
If `bottom' slot is nil, calculate by `top' and `width'.
Set the slot to VALUE when VALUE non-nil.
VALUE can be a positive integer or a (quoted) form.

Form will be evaluated to get a number during drawing of VIEW,
make sure it returns a positive integer."
  (if value
      (setf (raw-bottom-of view) value)
    (let ((raw-left (raw-left-of view)))
      (or raw-left
          (condition-case nil
              (+ (raw-top-of view) (raw-height-of view))
            (error "Not enough constrain for %s. Cannot calculate left constrain of it" (id-of view)))))))
(defun width-of (view &optional value)
  "Takes any VIEW of `abstract-view' class and return its `width' slot.
If `width' slot is nil, calculate by `left' and `right'.
Set the slot to VALUE when VALUE non-nil.
VALUE can be a positive integer or a (quoted) form.

Form will be evaluated to get a number during drawing of VIEW,
make sure it returns a positive integer."
  (if value
      (setf (raw-width-of view) value)
    (let ((raw-left (raw-left-of view)))
      (or raw-left
          (condition-case nil
              (- (raw-right-of view) (raw-left-of view))
            (error "Not enough constrain for %s. Cannot calculate width constrain of it" (id-of view)))))))
(defun height-of (view &optional value)
  "Takes any VIEW of `abstract-view' class and return its `height' slot.
If `height' slot is nil, calculate by `left' and `bottom'.
Set the slot to VALUE when VALUE non-nil.
VALUE can be a positive integer or a (quoted) form.

Form will be evaluated to get a number during drawing of VIEW,
make sure it returns a positive integer."
  (if value
      (setf (raw-height-of view) value)
    (let ((raw-left (raw-left-of view)))
      (or raw-left
          (condition-case nil
              (- (raw-bottom-of view) (raw-top-of view))
            (error "Not enough constrain for %s. Cannot calculate height constrain of it" (id-of view)))))))

;;;;; Other Functions

(defun uikit-attribute-by-id (id attribute &optional value)
  "Get or set ATTRIBUTE of view with id ID.
If VALUE is non-nil, set; otherwise get."
  (funcall (intern (format "%s.%s" (symbol-name id) (symbol-name attribute))) value))

(defun get/set-pos-of (view &optional pos)
  "Return the up left position of VIEW.
Position is a `uikit-pos'.

If POS non-nil set instead of get."
  ;; TESTED
  (if pos
      (let ((x (uikit-pos-x pos))
            (y (uikit-pos-x pos)))
        (setf (left-of view) x
              (top-of view) y)
        ;; return pos
        pos)
    (let ((x (uikit-attribute-by-id (id-of view) 'left))
          (y (uikit-attribute-by-id (id-of view) 'top)))
      (make-uikit-pos :x x :y y))))

;;;; View

(defclass uikit-view (uikit-abstract-view)
  ((face
    :accessor face-of
    :initarg :face
    :initform nil
    :documentation "The face used for this view. Default to nil.
You can set face by property list but this is more convenient.
face in property list will override this. "
    :type (or null (satisfies facep)))
   (pad-char
    :accessor pad-char-of
    :initarg :pad-char
    :initform (eval uikit-pad-char)
    :documentation "The char used to pad extra space."
    :type character)
   (keymap
    :accessor keymap-of
    :initarg :keymap
    :initform nil
    :documentation "Keymap on the view
in addition to the default major and minor mode keymaps. Default to nil.
You can set keymap in property list but this is more convenient.
TODO Does keymap in property list override this?
"
    :type (or null (satisfies keymapp)))
   (property-list
    :accessor property-list-of
    :initarg :property-list
    :initform nil
    :documentation "Extra text properties that you want to put to view text.
overwrites face and keymap slot.
each element of the list is an cons of PROPERTY and VALUE that are eligibel for `put-text-property'."
    :type (or null list))

   ;; content

   (content
    :accessor content-of
    :initform ()
    :documentation "The actual text that is drew to canvas, cached for later use.
But it doesn't include the padding spaces around, since size might
change more frequent that the content. See `padded-content'.
It is a list of strings, each string is a line.")
   (padded-content
    :accessor padded-content-of
    :initform ()
    :documentation "The actual text that is drew on canvas, with padded space.
It is a list for the same reason as `content'.")
   (content-changed
    :accessor content-changed-of
    :initform t
    :documentation "If this is t, then content needs to be recalculated.
Everythime a change that alters the visual appearance of the view is made,
this should be set to t. But don't change this value directly, use `uikit-changed'.
Typically this value is changed to t in`uikit-make-content' and to nil in `uikit-draw'."
    :type boolean))
  "A view is like a widget. It is the smallest unit of an UI.
his class is an abstract class."
  :abstract t)

(cl-defmethod uikit-make-content ((view uikit-view))
 "Make content from data, returna a list of strings, each string is a line.
Each line must have same length and should not contain any return char."
 ;; abstract
 nil)

(cl-defmethod uikit-make-content :around ((view uikit-view))
  "Make content if `content-changed' is t.
This function returns the content just for usability.
It's main job is to (potentially) update the content of VIEW.
Instead, it sets `content-changed', `width', `height', `content' slots.
So the specific `uikit-make-content' of each view class has to return their content."
  (if (content-changed-of view)
      (let ((content (cl-call-next-method view)))
        (setf (content-changed-of view) t
              (width-of view) (length (car content))
              (height-of view) (length content)
              (content-of view) content)) ; last val will be returned
    ;; content not changed, just return it
    (content-of view)))

(cl-defmethod uikit-draw ((view uikit-view) &optional pos)
  "Draw the content on screen.

It just add properties: face, keymap, uikit-view, others in property-list,
and then call `uikit-raw-draw'."
  ;; TOTEST
  (let ((content (uikit-make-content view))
        (all-property (append `(uikit-view ,view
                                           face ,(face-of view)
                                           keymap ,(keymap-of view))
                              (property-list-of view)))
        line-length
        (inhibit-modification-hooks t)
        (pos (if pos
                 (get/set-pos-of view pos)
               (get/set-pos-of view))))
    (dolist (line content)
      (add-text-properties 0 (length line) all-property line))
    (uikit-raw-draw content pos)))

;;;; Stack

(defclass uikit-stack (uikit-abstract-view)
 ((subview-list
   :accessor subview-list-of
   :initarg :subview-list
   :initform nil
   :documentation "List of subviews of the stack view.")
  (stack-style
   :accessor stack-style-of
   :initarg :stack-style
   :initform 'stack
   :documentation "Determins how does stack \"stack\" subviews together.
It can be 'stack, 'equal-space or 'portion."
   :type symbol)
  (portion-plist
   :accessor portion-plist-of
   :initform nil
   :documentation "A plist of subview id and their portion in stack.
Only takes effect if stack-style of the stack is 'portion.this is not good, this is too slow."))
 "Stack view, used for grouping multiple view together and arrage their position automatically.")

(cl-defmethod uikit-make-content ((stack uikit-stack))
 "STACK make content by asking its subviews to make content."
 ;; TOTEST
 (dolist (subview (subview-list-of stack))
   (uikit-make-content subview)))

(cl-defmethod uikit-quit ((stack uikit-stack))
 "Quit stack and set all subviews to nil."
 ;; TODO not finished
 (dolist (subview (subview-list-of stack))
   (uikit-quit subview))
 (setf stack nil))


;;;; Scene

(defclass uikit-scene (uikit-stack)
 ((name
   :initarg :name
   :initform "*UIKit Abstract Scene*"
   :accessor name-of
   :documentation "The name of this scene."
   :type string)
  (buffer
   :initform nil
   :accessor buffer-of
   :documentation "The buffer in where the scene is displayed."
   :type (or null buffer))
  (constrain-list
   :initform nil
   :accessor constrain-list-of
   :documentation "All of the constrains of the subiews of this scene."))
 ;; abstract
 "A scene is like a web page."
 :abstract t)

(cl-defmethod make-instance :after ((scene uikit-scene) &key)
 "Create buffer for scene."
 (setf (buffer-of scene)
       (let ((buffer (get-buffer-create (name-of scene))))
         (with-current-buffer buffer
           (setq uikit-buffer-scene scene))
         buffer)))

(defun uikit-configure-constrain (scene)
  ;; TODO
  "Configure constrain for each subview inside scene."
  ())

;;;;; Constrain

;;;;;; Main function

(defun uikit-constrain-find-entry (left stack)
  "Find the entry by LEFT side in constrain-list of STACK.
nil if none found."
  (catch 'return
    (dolist (entry (constrain-list-of stack))
      (when (equal (car entry) left)
        (throw 'return entry)))))


(defun uikit-configure-stack-auto-constrain (scene)
  "Let each stack of SCENE configure its subviews
position automatically by its stacking style.
\stack, equal-space, portion\)"
  ;; TODO
  )

;;;; App

(defclass uikit-app (eieio-singleton eieio-persistent)
  ((name
    :initarg :name
    :initform "An App"
    :accessor name-of
    :documentation "The name of the app."
    :type string)
   (scene-ring
    :initform nil
    :accessor scene-ring-of
    :documentation "The ring of scenes.")
   (current-scene
    :initform nil
    :accessor current-scene-of
    :documentation "The current scene of the app."
    :type (or null uikit-scene))
   (entry-scene
    :initarg entry-scene
    :initform nil
    :accessor entry-scene-of
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

(defclass uikit-label (uikit-view)
 ((text
   :initarg :text
   :accessor text-of
   :initform "Text"
   :documentation "The text of label."
   :type string))
 "A label is a one-line text.")

(cl-defmethod uikit-make-content ((label uikit-label))
  "Return content of LABEL."
  ;; TOTEST
  (split-string (text-of label) "\n"))

;;;; Button

(defclass uikit-button (uikit-label)
  ((text ;; override label.text
    :initform "Button")
   (mouse-1-func
    :initarg :mouse-1-func
    :initform 'uikit-button-clicked
    :accessor mouse-1-func-of
    :documentation "The function invoked when mouse-1 clicked on the button.
It can be either a symbol or a lambda.")
   (mouse-2-func
    :initarg :mouse-2-func
    :initform 'uikit-button-clicked
    :accessor mouse-2-func-of
    :documentation "The function invoked when mouse-2 clicked on the button.
It can be either a symbol or a lambda.")
   (mouse-3-func
    :initarg :mouse-3-func
    :initform 'uikit-button-clicked
    :accessor mouse-3-func-of
    :documentation "The function invoked when mouse-3 clicked on the button.
It can be either a symbol or a lambda.")
   (return-func
    :initarg :return-func
    :initform 'uikit-button-clicked
    :accessor return-func-of
    :documentation "The function invoked when RET clicked on the button.
It can be either a symbol or a lambda.")
   (help
    :initarg :help
    :initform "Click"
    :accessor help-of
    :documentation "This is displayed when mouse is on button."
    :type string))
  "A button.")


(cl-defmethod initialize-instance :after ((button uikit-button) &rest _)
  "Add help to properties."
  ;; TOTEST
  (setf (keymap-of button) (let ((map (make-sparse-keymap)))
                             (define-key map (kbd "<mouse-1>") (mouse-1-func-of button))
                             (define-key map (kbd "<mouse-2>") (mouse-2-func-of button))
                             (define-key map (kbd "<mouse-3>") (mouse-3-func-of button))
                             (define-key map (kbd "<return>") (return-func-of button))
                             map))
  (setf (face-of button) 'uikit-button-face)
  (setf (property-list-of button)
        (append `(help-echo ,(help-of button) mouse-face uikit-mouse-face) (property-list-of button))))


(defun uikit-button-click ()
  "Button clicked by user."
  (interactive)
  (message "Button pressed"))

;;; Debug

(defun uikit-top-view-at-point (&optional point)
  "Return the top (most inner) view at POINT or (point)."
  (get-text-property (or point (point)) 'uikit-view))

(provide 'uikit)

;;; uikit.el ends here
