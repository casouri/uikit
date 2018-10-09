(require 'uikit)


(ert-deftest uikit--in-test ()
  "Test of `uikit--in'."
  (let ((myrange (make-uikit-range :min 1 :max 10)))
    (should (uikit--in 5 myrange))
    (should (uikit--in 1 myrange))
    (should (uikit--in 10 myrange))
    (should (uikit--in 1 myrange nil t))
    (should (uikit--in 10 myrange t))
    (should-not (uikit--in 1 myrange t))
    (should-not (uikit--in 10 myrange nil t))))

(ert-deftest uikit-gettter/setter ()
  "Test automatically generated getter/setter."
  (let ((button (make-instance 'uikit-button :id "mybutton" :text "my button"))
        (stack (make-instance 'uikit-stackview :id "mystack"))
        (uikit--drawing t))
    ;; normal setter & getter
    (uikit-left-of button 10)
    (uikit-right-of button 10)
    (uikit-top-of button 10)
    (uikit-bottom-of button 10)
    (uikit-width-of button 10)
    (uikit-height-of button 10)
    (should (eq (uikit-left-of button) 10))
    (should (eq (uikit-right-of button) 10))
    (should (eq (uikit-top-of button) 10))
    (should (eq (uikit-bottom-of button) 10))
    (should (eq (uikit-width-of button) 10))
    (should (eq (uikit-height-of button) 10))

    ;; calculate one from the other two
    (uikit-left-of button nil)
    (uikit-right-of button nil)
    (uikit-top-of button nil)
    (uikit-bottom-of button nil)
    (uikit-width-of button nil)
    (uikit-height-of button nil)

    (uikit-left-of button 10)
    (uikit-right-of button 20)
    (should (eq (uikit-width-of button) 10))

    (uikit-top-of button 10)
    (uikit-bottom-of button 20)
    (should (eq (uikit-height-of button) 10))

    ;; set constrain to lambda
    (uikit-left-of button (lambda (SELF) 10))
    (should (eq (uikit-left-of button) 10))
    (setf (uikit--left-cache-of button) nil)

    ;; constrain based on parent
    (setf (uikit--parent-stackview-of button) stack)
    (uikit-left-of button (lambda (SELF) (uikit-left-of (uikit--parent-stackview-of SELF))))
    (uikit-left-of stack 100)
    (should (eq (uikit-left-of button) 100))
    ))


(defun uikit-test-prepare-canvas ()
  "Prepare canvas for test."
  (switch-to-buffer (get-buffer-create "TEST"))
  (erase-buffer)
  (uikit-prepare-canvas))

(defun uikit-test-drawing-process ()
  "Test drawing process."
  (interactive)
  (make-instance 'uikit-button :id "mybutton")
  (uikit-left-of uikit//mybutton 20)
  (uikit-top-of uikit//mybutton 10)
  (let ((lexical-binding t))
    (uikit-test-prepare-canvas)
    (uikit-draw uikit//mybutton)))

(defun uikit-test-autolayout-stacking ()
  "Test autolayout."
  (interactive)
  (make-instance 'uikit-button :id "mybutton1")
  (make-instance 'uikit-button :id "mybutton2")
  (make-instance 'uikit-stackview :id "mystack")
  (uikit-left-of uikit//mystack 20)
  (uikit-top-of uikit//mystack 10)
  (setf (uikit--subview-list-of uikit//mystack) (list uikit//mybutton1 uikit//mybutton2))
  (setf (uikit--autolayout-of uikit//mystack) 'stacking)
  (let ((lexical-binding t))
    (uikit-test-prepare-canvas)
    (uikit-autolayout uikit//mystack)
    (uikit-draw uikit//mystack)))

(defun uikit-test-autolayout-equal-spacing ()
  "Test autolayout."
  (interactive)
  (make-instance 'uikit-button :id "mybutton1")
  (make-instance 'uikit-button :id "mybutton2")
  (make-instance 'uikit-button :id "mybutton3")
  (make-instance 'uikit-stackview :id "mystack")
  (uikit-left-of uikit//mystack 20)
  (uikit-top-of uikit//mystack 10)
  (uikit-right-of uikit//mystack 60)
  (setf (uikit--subview-list-of uikit//mystack) (list uikit//mybutton1 uikit//mybutton2 uikit//mybutton3))
  (setf (uikit--autolayout-of uikit//mystack) 'equal-spacing)
  (let ((lexical-binding t))
    (uikit-test-prepare-canvas)
    (uikit-autolayout uikit//mystack)
    (uikit-draw uikit//mystack)))


(ert-deftest uikit-constrain-test-obsolute ()
  "Test of constrains."
  (let ((stack (make-instance 'uikit-stack))
        (label0 (make-instance 'uikit-label :text "Label 0"))
        (label1 (make-instance 'uikit-label :text "Label 1"))
        (label2 (make-instance 'uikit-label :text "Label 2")))
    (setf (subview-list-of stack) '(label0 label1 label2))
    (setf (constrain-list-of stack) '((subview0-top <- parent-top)
                                      (subview0-left <- parent-left)
                                      (subview1-top <- parent-top)
                                      (subview1-left <- subview1-right)
                                      (subview2-top <- parent-top)
                                      (subview2-left <- subview1-right)))))

(ert-deftest uikit-pos-of-obsolute ()
  "Test of `pos-of'."
  (let ((view (make-instance 'uikit-button))
        pos)
    ;; left & top
    (setf (left-of view) 10
          (top-of view) 20)
    (setq pos (pos-of view))
    (should (eq (uikit-pos-x pos) 10))
    (should (eq (uikit-pos-y pos) 20))

    ;; width + right & height + bottom
    (setf (left-of view) nil
          (top-of view) nil
          (width-of view) 10
          (right-of view) 15
          (height-of view) 20
          (bottom-of view) 30)
    (setq pos (pos-of view))
    (should (eq (uikit-pos-x pos) 5))
    (should (eq (uikit-pos-y pos) 10))

    ;; not enough
    (setf (left-of view) nil
          (top-of view) nil
          (width-of view) nil
          (right-of view) 15
          (height-of view) nil
          (bottom-of view) nil)
    (should-error (pos-of view))

    ;; note enough #2
    (setf (left-of view) nil
          (top-of view) nil
          (width-of view) nil
          (right-of view) nil
          (height-of view) nil
          (bottom-of view) 30)
    (should-error (pos-of view))))


(ert-deftest uikit-gettter/setter-obsolute ()
  "Test automatically generated getter/setter."
  (let ((button (make-instance 'uikit-button :id 'mybutton :text "my button")))
    ;; normal setter & getter
    (mybutton.left 10)
    (mybutton.right 10)
    (mybutton.top 10)
    (mybutton.bottom 10)
    (mybutton.width 10)
    (mybutton.height 10)
    (should (eq (mybutton.left) 10))
    (should (eq (mybutton.right) 10))
    (should (eq (mybutton.top) 10))
    (should (eq (mybutton.bottom) 10))
    (should (eq (mybutton.width) 10))
    (should (eq (mybutton.height) 10))

    ;; calculate one from the other two
    (mybutton.left 'null)
    (mybutton.right 'null)
    (mybutton.top 'null)
    (mybutton.bottom 'null)
    (mybutton.width 'null)
    (mybutton.height 'null)

    (mybutton.left 10)
    (mybutton.right 20)
    (should (eq (mybutton.width) 10))

    (mybutton.top 10)
    (mybutton.bottom 20)
    (should (eq (mybutton.height) 10))))


