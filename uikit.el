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

(defvar uikit-infinity 1.0e+INF
  "Infinity.")

(defvar uikit-negative-infinity -1.0e+INF
  "Negative infinity.")

(defvar-local uikit-buffer-scene nil
  "The scene object of current buffer.")

(defface uikit-button-face '((t (:weight bold :underline t)))
  "Face used for UIKit buttons."
  :group 'uikit)

(defface uikit-mouse-face '((t (:inherit highlight)))
  "Face used when mouse is on button."
  :group 'uikit)

;;; Base function

(defun uikit-prepare-canvas (&optional width height)
  "Prepare the canvas by inserting spaces.
WIDTH and HEIGHT are optional."
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
      (dotimes (_ (- height (line-number-at-pos (point))))
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
  "Replace strings between START and END with STR.
This function doesn't move point."
  (save-excursion
    (delete-region start end)
    (goto-char start)
    (insert str)))

(defun uikit-goto (pos)
  "Go to `uikit-pos' POS."
  ;; TOTEST
  (goto-char (point-min))
  (forward-line (uikit-pos-y pos))
  (goto-char (+ (point) (uikit-pos-x pos))))

(defun uikit-pos-to-point (pos)
  "Convert `uikit-pos' POS to point (integer)."
  ;; TOTEST
  (+ (uikit-pos-x pos) (* (uikit-pos-y pos) (window-body-width))))

(defmethod uikit-draw ((content list) pos)
  "Draw raw strings to canvas.
CONTENT is a list of strings, their length have to equal to each other."
  ;; TOTEST
  (let (line
        (width (length (car content)))
        (height (length content)))
    (save-excursion
      (while (setq line (pop content))
        (goto-char (uikit-goto pos))
        (let ((point (point)))
          (uikit-replace-with line point (+ point width))
          (goto-char point)
          (forward-line))))))

;;; App

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

;;; View

;;;; Base structs

;; You can convert between raw pos (absolute pos)
;; and (possibily) relative pos.
(cl-defstruct uikit-pos
  (offset '(0 0))
  (x 0)
  (y 0))

;; TODO DOC
(cl-defstruct uikit-layout
  (direction 'horizontal)
  ;; start, end, spread-hard, spread-soft
  (align 'start)
  (space-between 0)
  ;; 0 means percisely the content size
  (min-width 0)
  (min-height 0)
  (max-width 0)
  (max-height 0))

(cl-defstruct uikit-contrain
  ;; distance to bound in these four directions
  (top 0)
  (bottom 0)
  (left 0)
  (right 0)
  ;; only effective when parent's max-height/width is not infinity
  ;; 0 means no constrain
  (height-portion 0)
  (width-portion 0))

(cl-defstruct uikit-range
  "Defines a range."
  (min 0)
  (max 0))

(cl-defmethod uikit-in (num (range uikit-range) &optional min-exclusive max-exclusive)
  "Return t if NUM is in RANGE, return nil if not.
If MIN-EXCLUSIVE is t, range is min exclusive,
same for MAX-EXCLUSIVE."
  (let ((min (uikit-range-min range))
        (max (uikit-range-max range))
        (less-than-func (if max-exclusive '< '<=))
        (greater-than-func (if min-exclusive '> '>=)))
    (if (and (funcall greater-than-func num min)
             (funcall less-than-func num max))
        t
      nil)))

;;;; Base class

(defclass uikit-view ()
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
   (constrain
    :accessor constrain-of
    :initarg :constrain
    :initform (make-uikit-contrain)
    :documentation "The layout information of the view."
    :type uikit-constrain)
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
Overwrites face and keymap slot.
Each element of the list is an cons of PROPERTY and VALUE that are eligibel for `put-text-property'."
    :type (or null list))

   ;; content

   (content
    :accessor content-of
    :initform ""
    :documentation "The actual text that is drew to canvas.
Cached for later use.
But it doesn't include the padding spaces around, since size might
change more frequent that the content. See `padded-content'"
    :type string)
   (padded-content
    :accessor padded-content-of
    :initform ""
    :documentation "The actual text that is drew on canvas, with padded space."
    :type string)
   (content-changed
    :accessor content-changed-of
    :initform t
    :documentation "If this is t, then content needs to be recalculated.
Everythime a change that alters the visual appearance of the view is made,
this should be set to t. Don't change this value directly, use `uikit-changed'."
    :type boolean)
   (height
    :accessor height-of
    :initform 0
    :documentation "The cached height of view."
    :type integer)
   (width
    :accessor width-of
    :initform 0
    :documentation "The cached width of view."
    :type integer))
  "A view is like a widget. It is the smallest unit of an UI.
This class is an abstract class."
  :abstract t)

(cl-defmethod uikit-report-width ((view uikit-view))
  "Report the width of the view so its parent uikit can arrange a pos and size for it to draw.
Return a integer."
  ;; abstract
  nil)

(cl-defmethod uikit-report-height ((view uikit-view))
  "Report the height of the view so its parent uikit can arrange a pos and size for it to draw.
Return a integer."
  ;; abstract
  nil)

(cl-defmethod uikt-make-content ((view uikit-view))
  "Make content from data, returna a list of strings, each string is a line."
  ;; abstract
  nil)

(cl-defmethod uikit-draw ((view uikit-view) pos width height)
  "Draw the content on screen."
  ;; TOTEST
  (let ((content (if (and (equal width (width-of view))
                          (equal height (height-of view)))
                     (padded-content-of view)
                   (uikit-pad-content (uikit-make-content view))))
        (all-property (append `(uikit-view ,view face ,(face-of view) keymap ,(keymap-of view))
                              (property-list-of view)))
        line-length)
    (setf (width-of view) width
          (height-of view) height)
    ;; measure line length by the first line of content.
    (setq line-length (length (car content)))
    (dolist (line content)
      (add-text-properties 0 line-length all-property line))
    (uikit-draw content)))

(defclass uikit-stack ()
  ((subview-list
    :accessor subview-list-of
    :initarg :subview-list
    :initform nil
    :documentation "List of subviews of the stack view.")
   (subview-layout
    :accessor subview-layout-of
    :initarg :subview-layout
    :initform (make-uikit-layout)
    :documentation "The layout of subviews."
   ;; TODO more doc 
    :type uikit-layout))
  "Stack view, used for grouping multiple view together and arrage their position automatically."
  :abstract t)


(cl-defmethod uikit-report-width ((stack uikit-stack))
  "Report the width of the view so its parent uikit can arrange a pos and size for it to draw.
Return a integer."
  ;; TOTEST
  (apply '+ (mapcar 'width-of (subview-list-of stack))))

(cl-defmethod uikit-report-height ((stack uikit-stack))
  "Report the height of the view so its parent uikit can arrange a pos and size for it to draw.
Return a integer."
  ;; TOTEST
  (apply '+ (mapcar 'height-of (subview-list-of stack))))

(cl-defmethod uikit-draw ((stack uikit-stack) pos width height)
  ;; TODO
  "Draw the content on screen."
  nil)

(cl-defmethod uikit-quit ((stack uikit-stack))
  "Quit stack and set all subviews to nil."
  ;; TOTEST
  (dolist (subview (subview-list-of stack))
    (uikit-quit subview))
  (setf stack nil))

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
    :type (or null buffer)))
  ;; abstract
  "A scene is like a page.")

(cl-defmethod initialize-instance :after ((scene uikit-scene) &key)
  "Create buffer for scene."
  (setf (buffer-of scene) (get-buffer-create (name-of scene))))

;;;; Subclass

(defclass uikit-label (uikit-view)
  ((text
    :initarg :text
    :accessor text-of
    :initform ""
    :documentation "The text of label."
    :type string)
   (text-range
    :initarg :text-range
    :accessor text-range-of
    :initform ((make-uikit-pos) . (make-uikit-pos))
    :documentation "A cons of upper left and down right of text."))
  "A label is a one-line text.")

(cl-defmethod uikit-draw ((label uikit-label) pos width height)
  "Draw LABEL at POS."
  ;; TOTEST
  (uikit-draw (list (text-of label))) pos)

(defclass uikit-button (uikit-view)
  ((help
    :initarg help
    :initform "Click"
    :accessor help-of
    :documentation "This is displayed when mouse is on button."
    :type string))
  "A button.")

(cl-defmethod initialize-instance :after ((button uikit-button))
  "Add help to properties."
  ;; TOTEST
  (setf (keymap-of button) (let ((map (make-sparse-keymap)))
                             (define-key map (kbd "<mouse-1>") #'uikit-invoke-button)
                             (define-key map (kbd "<mouse-2>") #'uikit-invoke-button)
                             (define-key map (kbd "<mouse-3>") #'uikit-invoke-button)
                             (define-key map (kbd "<return>") #'uikit-invoke-button)
                             map))
  (setf (face-of button) 'uikit-button-face)
  (setf (property-list-of button)
        (append `(help-echo ,(help-of button) mouse-face uikit-mouse-face) (property-list-of button))))

(cl-defmethod uikit-invoke-button ((button uikit-button))
  "Button clicked by mouse-1/2/3 or RET, it should be overloaded."
  ;; abstract
  nil)

(provide 'uikit)

;;; uikit.el ends here
