;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

;; TODO: Add bad frames and closed connections

(ert-deftest conn-one-frame-test ()
  (let ((excursion--data "^8|~foo/bar"))
    (let* ((frame (excursion--read-frame))
           (type (alist-get 'type frame))
           (len (alist-get 'data-len frame))
           (data (alist-get 'data frame)))
      (progn (should (equal type 'Data))
             (should (equal len 8))
             (should (equal data "~foo/bar"))))))

(ert-deftest conn-one-frame+1-test ()
  (let ((excursion--data "^8|~foo/bara"))
    (let* ((frame (excursion--read-frame))
           (type (alist-get 'type frame))
           (len (alist-get 'data-len frame))
           (data (alist-get 'data frame)))
      (progn (should (equal type 'Data))
             (should (equal len 8))
             (should (equal data "~foo/bar"))))))

(ert-deftest conn-frame-under-test ()
  (let ((excursion--data "^8|~foo/ba"))
    (let* ((frame (excursion--read-frame))
           (type (alist-get 'type frame))
           (len (alist-get 'data-len frame))
           (data (alist-get 'data frame)))
      (progn (should (equal type nil))
             (should (equal len nil))
             (should (equal data nil))))))

(ert-deftest successive-read-multiple-frame-test ()
  (let ((network-data "^8|~foo/bar^8|~foo/bar!")
        (excursion--data ""))
    (dotimes (i (length network-data))
      (setq excursion--data
            (concat excursion--data
                    (substring network-data i (1+ i))))
      (let* ((frame (excursion--read-frame))
             (type (alist-get 'type frame))
             (len (alist-get 'data-len frame))
             (data (alist-get 'data frame)))
        (when frame
          (should (equal type 'Data))
          (should (equal len 8))
          (should (equal data "~foo/bar")))))
    (should (equal excursion--data "!"))))

(ert-deftest conn-multi-len-test ()
  (let ((excursion--data "^8;4|~foo/barmarc"))
    (let* ((frame (excursion--read-frame))
           (type (alist-get 'type frame))
           (len (alist-get 'data-len frame))
           (data (alist-get 'data frame)))
      (progn (should (equal type 'Data))
             (should (equal len 8))
             (should (equal data "~foo/bar"))))))

(provide 'connection-test)
