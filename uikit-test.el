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

(ert-deftest uikit-content-width-for-stackview ()
  "Test if `uikit--content-width-of' function works well."
  (make-instance 'uikit-button :id "mybutton0")
  (make-instance 'uikit-button :id "mybutton1")
  (make-instance 'uikit-stackview :id "mystack")
  (uikit-subview-append uikit//mystack uikit//mybutton0 uikit//mybutton1)
  (uikit-make-content uikit//mystack)
  (should (eq (uikit--content-width-of uikit//mystack) 12)))

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
  (uikit-test-prepare-canvas)
  (uikit-draw uikit//mybutton))

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

(defun uikit-test-autolayout-top-orientation ()
  "Test top orientation."
  (interactive)
  (make-instance 'uikit-stackview :id "mystack")
  (make-instance 'uikit-button :id "mybutton0" :text "BUTTON0")
  (make-instance 'uikit-button :id "mybutton1" :text "BUTTON1")
  (setf (uikit--orientation-of uikit//mystack) 'top
        (uikit--autolayout-of uikit//mystack) 'stacking)
  (uikit-left-of uikit//mystack 20)
  (uikit-top-of uikit//mystack 10)
  (uikit-subview-append uikit//mystack uikit//mybutton0 uikit//mybutton1)
  (uikit-autolayout uikit//mystack)

  (uikit-test-prepare-canvas)
  (uikit-draw uikit//mystack))

(defun uikit-test-autolayout-stacking-space ()
  "Test stacking space."
  (interactive)
  (make-instance 'uikit-stackview :id "mystack"
                 :autolayout 'stacking
                 :stacking-space 5
                 :subview-list (list (make-instance 'uikit-button) (make-instance 'uikit-button)))
  (uikit-left-of uikit//mystack 20)
  (uikit-top-of uikit//mystack 10)
  (uikit-autolayout uikit//mystack)
  (uikit-test-prepare-canvas)
  (uikit-draw uikit//mystack))

(defun uikit-test-autolayout-bulk ()
  "Test autolayout."
  (interactive)
  (make-instance 'uikit-stackview :id "mystack")
  (setf (uikit--autolayout-of uikit//mystack) 'stacking
        (uikit--orientation-of uikit//mystack) 'top
        (uikit--v-align-of uikit//mystack) 'top)

  (dolist (num0 (number-sequence 0 9))
    (uikit-subview-append
     uikit//mystack
     (make-instance 'uikit-stackview :id (format "mystack%d" num0)))
    (setf (uikit--autolayout-of (symbol-value (intern (format "uikit//mystack%d" num0))))
          'stacking)
    (dolist (num1 (number-sequence 0 9))
      (uikit-subview-append
       (symbol-value (intern (format "uikit//mystack%d" num0)))
       (make-instance 'uikit-button :id (format "mybutton%d%s" num0 num1)))))

  (uikit-left-of uikit//mystack 20)
  (uikit-top-of uikit//mystack 10)
  ;; (uikit-right-of uikit//mystack 60)
  ;; (setf (uikit--autolayout-of uikit//mystack) 'equal-spacing)
  (let ((lexical-binding t)
        ;; (gc-cons-threshold 20000000)
        )
    (uikit-test-prepare-canvas)
    (uikit-autolayout uikit//mystack)
    (uikit-draw uikit//mystack)
    ;; (print (benchmark-run 1000 (uikit-draw uikit//mystack)))
    ))

(defun uikit-test-padding ()
  "Test that the content is padded with space when each lines are not in the same length.
There should be spacing in the 2nd line."
  (interactive)
  (uikit-test-autolayout-bulk)
  (make-instance 'uikit-label :id "mylabel")
  (setf (uikit--text-of uikit//mylabel) "##########\n#####\n##########")
  (uikit-left-of uikit//mylabel 20)
  (uikit-top-of uikit//mylabel 10)
  (uikit-draw uikit//mylabel))

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

(defun uikit-test-nest-stackview ()
  "Test nesting stackview."
  (interactive)
  (make-instance 'uikit-stackview :id "mystack0")
  (make-instance 'uikit-stackview :id "mystack1")
  (make-instance 'uikit-stackview :id "mystack2")
  (make-instance 'uikit-button :id "mybutton")
  (uikit-subview-append uikit//mystack0 uikit//mystack1)
  (uikit-subview-append uikit//mystack1 uikit//mystack2)
  (uikit-subview-append uikit//mystack2 uikit//mybutton)
  (uikit-left-of uikit//mystack0 20)
  (uikit-top-of uikit//mystack0 10)
  (setf (uikit--autolayout-of uikit//mystack0) 'stacking
        (uikit--autolayout-of uikit//mystack1) 'stacking
        (uikit--autolayout-of uikit//mystack2) 'stacking)
  (uikit-autolayout uikit//mystack0)
  (uikit-test-prepare-canvas)
  (uikit-draw uikit//mystack0))


(ert-deftest uikit-table-subview-list ()
  "Test subview list for header and footer."
  (make-instance 'uikit-table-cell :id "myheader")
  (make-instance 'uikit-table-cell :id "myfooter")
  (make-instance 'uikit-table :id "mytable" :header uikit//myheader :footer uikit//myfooter)
  (should (equal (uikit--subview-list-of uikit//mytable) (list uikit//myheader uikit//myfooter))))
