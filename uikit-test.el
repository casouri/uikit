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


