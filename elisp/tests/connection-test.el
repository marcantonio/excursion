;;; -*- lexical-binding: t; -*-

(require 'excursion)

;; TODO: Add bad frames and closed connections

(ert-deftest conn-one-frame-test ()
  (let ((excursion--data "^8|~foo/bar"))
    (let* ((frame (excursion--read-frame))
           (type (excursion--frame-type frame))
           (len (excursion--frame-len frame))
           (data (excursion--frame-data frame)))
      (progn (should (equal type 'Data))
             (should (equal len 8))
             (should (equal data "~foo/bar"))
             (should (equal (excursion--frame-get-segment frame 0) "~foo/bar"))
             (should (equal (excursion--frame-get-segment frame 1) nil))))))

(ert-deftest conn-one-frame+1-test ()
  (let ((excursion--data "^8|~foo/bara"))
    (let* ((frame (excursion--read-frame))
           (type (excursion--frame-type frame))
           (len (excursion--frame-len frame))
           (data (excursion--frame-data frame)))
      (progn (should (equal type 'Data))
             (should (equal len 8))
             (should (equal data "~foo/bar"))))))

(ert-deftest conn-frame-under-test ()
  (let ((excursion--data "^8|~foo/ba"))
    (let ((frame (excursion--read-frame)))
      (progn (should (equal frame nil))))))

(ert-deftest successive-read-multiple-frame-test ()
  (let ((network-data "^8|~foo/bar^8|~foo/bar!")
        (excursion--data ""))
    (dotimes (i (length network-data))
      (setq excursion--data
            (concat excursion--data
                    (substring network-data i (1+ i))))
      (when-let ((frame (excursion--read-frame))
                 (type (excursion--frame-type frame))
                 (len (excursion--frame-len frame))
                 (data (excursion--frame-data frame)))
        (when frame
          (should (equal type 'Data))
          (should (equal len 8))
          (should (equal data "~foo/bar")))))
    (should (equal excursion--data "!"))))

(ert-deftest conn-multi-len-test ()
  (let ((excursion--data "^8;4|~foo/barmarc"))
    (let* ((frame (excursion--read-frame))
           (type (excursion--frame-type frame))
           (positions (excursion--frame-positions frame))
           (len (excursion--frame-len frame))
           (data (excursion--frame-data frame)))
      (progn (should (equal type 'Data))
             (should (equal len 12))
             (should (equal positions [(0 . 8) (8 . 12)]))
             (should (equal data "~foo/barmarc"))
             (should (equal (excursion--frame-get-segment frame 0) "~foo/bar"))
             (should (equal (excursion--frame-get-segment frame 1) "marc"))
             (should (equal (excursion--frame-get-segment frame 2) nil))))))

(provide 'connection-test)
