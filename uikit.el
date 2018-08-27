;;; uikit.el --- UI kit for Emacs

;;; Commentary:
;; 

;;; Code:
;;

(require 'eieio-base)

;;; Variable

(defgroup uikit
  ()
  "A UI framework."
  :group 'convenience)

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

;;; Base structs

; You can convert between raw pos (absolute pos)
; and (possibily) relative pos.
; both direction are zero based.
(cl-defstruct uikit-pos
 "Represents a position in cavas.
oth x and y are zero based.
ote that the length of the unit in y direction is about two times as
he unit in x direction.
or example (20, 10) nearly forms a square."
 (x 0)
 (y 0))

(cl-defmethod uikit-pos+ ((pos uikit-pos) (offset uikit-pos))
 "Add OFFSET to POS destructivly. Return the modified POS."
 (setf (uikit-pos-x pos) (+ (uikit-pos-x pos) (uikit-pos-x offset))
       (uikit-pos-y pos) (+ (uikit-pos-y pos) (uikit-pos-y offset)))
 pos)

; TODO DOC
(cl-defstruct uikit-layout
 "Specifies how does a stack arrange
pace for its subviews.

start-at is where does the subviews start stacking.

fill is the filling stretagy of subviews.
t can be either 'proportional or 'equal.
equal only takes effect when one of the directions
s unbound.

pace-between is the space between subviews.

lign is the alignment of the subviews along the stack axis,
t can be 'left, 'right, 'center.
left and 'right is the relative direction when you regard
start-at as top.
therefore, if 'start-at is 'bottom, 'left is actually right."
 (start-at 'left)
 (fill 'proportional)
 (space-between 0)
 (align 'left))


(cl-defstruct uikit-range
 "Defines a range."
 (min 0)
 (max 0))

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
  "Append ELT to SEQ destructivly. This is a macro."
  `(if ,seq
       (nconc ,seq (list ,elt))
     (setq ,seq (list ,elt))))

(defmacro uikit-push (elt seq)
  "Push ELT to SEQ destructivly. This is a macro."
  `(if ,seq
       (push ,elt ,seq)
     (setq ,seq (list ,elt))))

(defun uikit-replace-with (str start end)
  "Replace strings between START and END with STR."
  (save-excursion
    (delete-region start end)
    (goto-char start)
    (insert str)))

(defun uikit-goto (pos)
  "Go to `uikit-pos' POS."
  ;; TOTEST
  (goto-char (point-min))
  (forward-line (uikit-pos-y pos))
  (goto-char (uikit-pos-x pos)))

(defun uikit-pos-to-point (pos)
  "Convert `uikit-pos' POS to point (integer)."
  ;; TOTEST
  (+ (uikit-pos-x pos) (* (uikit-pos-y pos) (window-body-width))))

(defun uikit-raw-draw (content pos)
  "Draw raw strings to canvas at POS.
CONTENT is a list of strings, their length have to equal to each other."
  ;; TOTEST hand tested
  (let (line
        (width (length (car content)))
        (height (length content)))
    (save-excursion
      (uikit-goto pos)
      (while (setq line (pop content))
        (let ((point (point)))
          (uikit-replace-with line point (+ point width))
          ;; ??? maybe cache a window width for current scene
          (goto-char (+ point (window-body-width))))))))

;;; Base class

;;;; Subview

(defclass uikit-abstract-view ()
  (;; required in init
   (id
    :initarg :id
    :documentation "The identification of this view. Should be a string."
    :type string)
   ;; not required in init
   (left
    :accessor left-of
    :initform nil
    :type (or null integer)
    :documentation "Distance between the left of view and the left of scene.")
   (right
    :accessor right-of
    :initform nil
    :type (or null integer)
    :documentation "Distance between the right of view and the right of scene.")
   (top
    :accessor top-of
    :initform nil
    :type (or null integer)
    :documentation "Distance between the top of view and the top of scene.")
   (bottom
    :accessor bottom-of
    :initform nil
    :type (or null integer)
    :documentation "Distance between the bottom of view and the bottom of scene.")
   (width
    :accessor width-of
    :initform nil
    :type (or null integer)
    :documentation "The cached width.")
   (height
    :accessor height-of
    :initform nil
    :type (or null integer)
    :documentation "The cached height."))
  "Parent of `uikit-view' and `uikit-stack'.
Both be a subview of one another."
  :abstract t)

(cl-defmethod make-instance :around ((view-class (subclass uikit-abstract-view)) &keys id &rest args)
  ;; TESTED
  "Create getter&setter for left/right/top/bottom/width/height of VIEW.

They are named base on ID: e.g. ID.left / ID.right / etc
ARGS are passed to `make-instance'."
  (let ((view (cl-call-next-method))
        (id (symbol-name id)))
    (when (member 'uikit-stack (eieio-class-parents view-class))
      ;; TOTEST
      (setf (symbol-function (intern (format "%s.space" id)))
            (eval `(lambda ()
                     "Returns the length of the expected space between subviews."
                     (let* ((subview-list (subview-list-of ,view))
                            (id-list (mapcar 'id-of subview-list))
                            (width-list (mapcar (lambda (id) (funcall (symbol-function (intern (format "%s.width" id)))))
                                                id-list))
                            (stack-width (funcall (symbol-function (intern (format "%s.width" ,id))))))
                       (/ (- stack-width (apply '+ width-list)) (length subview-list)))))
            ))
    (setf
     ;; view.left
     (symbol-function (intern (format "%s.left" id)))
     (eval `(lambda (&optional left)
              ,(format "Set/get LEFT of %s.
If LEFT non-nil, set; otherwise get.
To set to nil, use symbol 'null." id)
              (if left
                  (setf (left-of ,view) (if (eq left 'null) nil
                                          left))
                (or (left-of ,view)
                    (condition-case nil
                        (- (right-of ,view) (width-of ,view))
                      (error nil))))))
     ;; view.right
     (symbol-function (intern (format "%s.right" id)))
     (eval `(lambda (&optional right)
              ,(format "Set/get RIGHT of %s.
If RIGHT non-nil, set; otherwise get.
To set to nil, use symbol 'null." id)
              (if right
                  (setf (right-of ,view) (if (eq right 'null) nil
                                           right))
                (or (right-of ,view)
                    (condition-case nil
                        (+ (left-of ,view) (width-of ,view))
                      (error nil))))))
     ;; view.top
     (symbol-function (intern (format "%s.top" id)))
     (eval `(lambda (&optional top)
              ,(format "Set/get TOP of %s.
If TOP non-nil, set; otherwise get.
To set to nil, use symbol 'null." id)
              (if top
                  (setf (top-of ,view) (if (eq top 'null) nil
                                         top))
                (or (top-of ,view)
                    (condition-case nil
                        (- (bottom-of ,view) (height-of ,view))
                      (error nil))))))
     ;; view.bottom
     (symbol-function (intern (format "%s.bottom" id)))
     (eval `(lambda (&optional bottom)
              ,(format "Set/get BOTTOM of %s.
If BOTTOM non-nil, set; otherwise get.
To set to nil, use symbol 'null." id)
              (if bottom
                  (setf (bottom-of ,view) (if (eq bottom 'null) nil
                                            bottom))
                (or (bottom-of ,view)
                    (condition-case nil
                        (+ (top-of ,view) (height-of ,view))
                      (error nil))))))
     ;; view.width
     (symbol-function (intern (format "%s.width" id)))
     (eval `(lambda (&optional width)
              ,(format "Set/get WIDTH of %s.
If WIDTH non-nil, set; otherwise get.
To set to nil, use symbol 'null." id)
              (if width
                  (setf (width-of ,view) (if (eq width 'null) nil
                                           width))
                (or (width-of ,view)
                    (condition-case nil
                        (- (right-of ,view) (left-of ,view))
                      (error nil))))))
     ;; view.height
     (symbol-function (intern (format "%s.height" id)))
     (eval `(lambda (&optional height)
              ,(format "Set/get HEIGHT of %s.
If HEIGHT non-nil, set; otherwise get.
To set to nil, use symbol 'null." id)
              (if height
                  (setf (height-of ,view) (if (eq height 'null) nil
                                            height))
                (or (height-of ,view)
                    (condition-case nil
                        (- (bottom-of ,view) (top-of ,view))
                        (error nil)))))))))

(defun uikit-attribute-by-id (id attribute &optional value)
  "Get or set ATTRIBUTE of view with id ID.
If VALUE is non-nil, set; otherwise get."
  (funcall (intern (format "%s.%s" (symbol-name id) (symbol-name attribute))) value))

(defun pos-of (view)
  "Return the up left position of VIEW.
Position is a `uikit-pos'."
  ;; TESTED
  (let ((x (uikit-attribute-by-id (id-of view) 'left))
        (y (uikit-attribute-by-id (id-of view) 'top)))
    (make-uikit-pos :x x :y y)))

;;;; View

(defclass uikit-view (uikit-abstract-view)
 ((face
   :accessor face-of
   :initarg :face
   :initform nil
   :documentation "The face used for this view. Default to nil.
ou can set face by property list but this is more convenient.
ace in property list will override this. "
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
n addition to the default major and minor mode keymaps. Default to nil.
ou can set keymap in property list but this is more convenient.
ODO Does keymap in property list override this?
"
   :type (or null (satisfies keymapp)))
  (property-list
   :accessor property-list-of
   :initarg :property-list
   :initform nil
   :documentation "Extra text properties that you want to put to view text.
verwrites face and keymap slot.
ach element of the list is an cons of PROPERTY and VALUE that are eligibel for `put-text-property'."
   :type (or null list))

  ;; content

  (content
   :accessor content-of
   :initform ""
   :documentation "The actual text that is drew to canvas.
ached for later use.
ut it doesn't include the padding spaces around, since size might
hange more frequent that the content. See `padded-content'")
  (padded-content
   :accessor padded-content-of
   :initform ""
   :documentation "The actual text that is drew on canvas, with padded space."
   :type string)
  (content-changed
   :accessor content-changed-of
   :initform t
   :documentation "If this is t, then content needs to be recalculated.
verythime a change that alters the visual appearance of the view is made,
his should be set to t. Don't change this value directly, use `uikit-changed'."
   :type boolean))
 "A view is like a widget. It is the smallest unit of an UI.
his class is an abstract class."
 :abstract t)

(cl-defmethod uikit-make-content ((view uikit-view))
 "Make content from data, returna a list of strings, each string is a line.
ach line must have same length and should not contain any return char."
 ;; abstract
 nil)

(cl-defmethod uikit-make-content :around ((view uikit-view))
 "Make content if `content-changed' is t and return content,
otherwise return cached content.
Sets `content-changed', `width', `height', `content' slots."
 (if (content-changed-of view)
     (let ((content (cl-call-next-method view)))
       (setf (content-changed-of view) t
             (width-of view) (length (car content))
             (height-of view) (length content)
             (content-of view) content))
   (content-of view)))

(cl-defmethod uikit-draw ((view uikit-view) &optional pos)
 "Draw the content on screen.

dd properties: face, keymap, uikit-view, others in property-list."
 ;; TOTEST
 (let ((content (uikit-make-content view))
       (all-property (append `(uikit-view ,view
                                          face ,(face-of view)
                                          keymap ,(keymap-of view))
                             (property-list-of view)))
       line-length
       (inhibit-modification-hooks t)
       (pos (if pos
                (setf (pos-of view) pos)
              (pos-of view))))
   ;; measure line length by the first line of content.
   (setq line-length (length (car content)))
   (dolist (line content)
     (add-text-properties 0 line-length all-property line))
   (uikit-raw-draw content pos)))

;;;; Stack

(defclass uikit-stack (uikit-abstract-view)
 ((subview-list
   :accessor subview-list-of
   :initarg :subview-list
   :initform nil
   :documentation "List of subviews of the stack view."))
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

(cl-defmethod initialize-instance :after ((scene uikit-scene) &key)
 "Create buffer for scene."
 (setf (buffer-of scene) (get-buffer-create (name-of scene))))

;;;;; Constrain

;;;;;; Main function

(defun uikit-constrain-find-entry (left stack)
  "Find the entry by LEFT side in constrain-list of STACK.
nil if none found."
  (catch 'return
    (dolist (entry (constrain-list-of stack))
      (when (equal (car entry) left)
        (throw 'return entry)))))

(defun uikit-constrain-resolve-entry (stack entry)
  "ENTRY is a constrain entry of STACK: (left = right)."
  ;; TOTEST
  (let* ((left-symbol (car entry))
         (right-symbol (nth 2 entry)))
    (funcall
     ;; a function that sets and returns val
     (uikit-constrain-resolve-set stack left-symbol)
     ;; val
     (uikit-constrain-resolve-rhs stack right-symbol))))

(defun uikit-arrage-constrain (stack)
  "Arrange subviews of STACK."
  (dolist (subview (subview-list-of stack))
    (setf (pos-of subview) nil))
  (dolist (entry (constrain-list-of stack))
    (uikit-constrain-resolve-entry entry stack))
  ;; now the pos in each subview is relative pos
  ;; relative to stack pos
  ;; there shouldn't be any nil pos,
  ;; if there are, the constrains have error
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
 (list (text-of label)))

;;;; Button

(defclass uikit-button (uikit-label)
  ((text ;; override label.text
    :initform "Button")
   (help
    :initarg :help
    :initform "Click"
    :accessor help-of
    :documentation "This is displayed when mouse is on button."
    :type string))
  "A button.")


(cl-defmethod initialize-instance :after ((button uikit-button) &key)
  "Add help to properties."
  ;; TOTEST
  (setf (keymap-of button) (let ((map (make-sparse-keymap)))
                             (define-key map (kbd "<mouse-1>") #'uikit-button-click)
                             (define-key map (kbd "<mouse-2>") #'uikit-button-click)
                             (define-key map (kbd "<mouse-3>") #'uikit-button-click)
                             (define-key map (kbd "<return>") #'uikit-button-click)
                             map))
  (setf (face-of button) 'uikit-button-face)
  (setf (property-list-of button)
        (append `(help-echo ,(help-of button) mouse-face uikit-mouse-face) (property-list-of button))))


(defun uikit-button-click ()
  "Button clicked by user."
  (interactive)
  (uikit-invoke-button (uikit-top-view-at-point)))

(cl-defmethod uikit-invoke-button ((button uikit-button))
  "Button clicked by mouse-1/2/3 or RET. Overload with (eql) specializer."
  (message "Button clicked. You should overload this method."))

;;; Debug

(defun uikit-top-view-at-point (&optional point)
  "Return the top (most inner) view at POINT or (point)."
  (get-text-property (or point (point)) 'uikit-view))

(provide 'uikit)

;;; uikit.el ends here
