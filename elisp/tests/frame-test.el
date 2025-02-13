;;; -*- lexical-binding: t; -*-

(require 'excursion-frame)

(ert-deftest frame-single-segment-test ()
  (let ((frame (excursion--frame-create-from ?^ '(8) "~foo/bar")))
    (progn
      (should (equal (excursion--frame-type frame) 'Data))
      (should (equal (excursion--frame-len frame) 8))
      (should (equal (excursion--frame-positions frame) [(0 . 8)]))
      (should (equal (excursion--frame-get-segment frame 0) "~foo/bar"))
      (should (equal (excursion--frame-get-segment frame 1) nil)))))

(ert-deftest frame-multi-segment-test ()
  (let ((frame (excursion--frame-create-from ?^ '(8 4) "~foo/barmarcs")))
    (progn
      (should (equal (excursion--frame-type frame) 'Data))
      (should (equal (excursion--frame-len frame) 12))
      (should (equal (excursion--frame-positions frame) [(0 . 8)
                                                         (8 . 12)]))
      (should (equal (excursion--frame-get-segment frame 0) "~foo/bar"))
      (should (equal (excursion--frame-get-segment frame 1) "marc"))
      (should (equal (excursion--frame-get-segment frame 2) nil)))))

(provide 'frame-test)
