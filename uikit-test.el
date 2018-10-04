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
  (let ((button (make-instance 'uikit-button :id "mybutton" :text "my button")))
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
    (should (eq (uikit-height-of button) 10))))

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


